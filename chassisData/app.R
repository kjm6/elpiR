library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

# Load your data
dataELPI <- readRDS("data/dataELPI.rds")
dataTruck <- readRDS("data/dataTruck.rds")
temp_dataAQ <- readRDS("data/dataAQ.rds")

# Ensure timezone is consistent
dataELPI$DateTime <- force_tz(dataELPI$DateTime, tzone = "America/Los_Angeles")
dataTruck$DateTime <- force_tz(dataTruck$DateTime, tzone = "America/Los_Angeles")

# Add UTC versions
dataELPI <- dataELPI %>%
  mutate(
    DateTime_UTC = with_tz(DateTime, tzone = "UTC"),
    DateTime_rounded = round_date(DateTime_UTC, "second")
  )

dataTruck <- dataTruck %>%
  mutate(
    DateTime_UTC = with_tz(DateTime, tzone = "UTC"),
    DateTime_rounded = round_date(DateTime_UTC, "second"),
    WheelBasedVehicleSpeed = as.numeric(WheelBasedVehicleSpeed),
    WheelBasedVehicleSpeed = ifelse(is.na(WheelBasedVehicleSpeed), 0, WheelBasedVehicleSpeed),
    WheelBasedVehicleSpeed = ifelse(WheelBasedVehicleSpeed > 150, NA, WheelBasedVehicleSpeed)
  )

# Time defaults
start_fixed_local <- as.POSIXct("2025-06-20 11:14:00", tz = "America/Los_Angeles")
end_fixed_local <- as.POSIXct("2025-06-20 15:00:00", tz = "America/Los_Angeles")

ui <- fluidPage(
  titlePanel("June 2025: Chassis Dynamometer Campaign"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      airDatepickerInput(
        inputId = "start_time",
        label = "Start Date-Time (local time)",
        value = start_fixed_local,
        timepicker = TRUE,
        timepickerOpts = list(enableSeconds = TRUE),
        width = "100%"
      ),
      airDatepickerInput(
        inputId = "end_time",
        label = "End Date-Time (local time)",
        value = end_fixed_local,
        timepicker = TRUE,
        timepickerOpts = list(enableSeconds = TRUE),
        width = "100%"
      ),
      selectizeInput(
        inputId = "columns_select",
        label = "Select Columns to Plot",
        choices = names(dataELPI)[grepl("^Stage \\d+ \\(.*\\)$", names(dataELPI))],
        selected = c("Stage 5 (Dp50 = 98nm)", "Stage 9 (Dp50 = 0.91um)"),
        multiple = TRUE,
        options = list(plugins = list('remove_button')),
        width = "100%"
      ),
      br(),
      h4("Summary Statistics"),
      tableOutput("summary_stats"),
      br(),
      h4("Air Quality"),
      tableOutput("airmonitor")
    ),
    mainPanel(
      width = 8,
      plotOutput("time_series_plot", height = "450px"),
      br(),
      plotOutput("airmonitor_plot", height = "450px")
    )
  )
)

server <- function(input, output, session) {

  input_times_utc <- reactive({
    req(input$start_time, input$end_time)

    start_local <- force_tz(input$start_time, "America/Los_Angeles")
    end_local <- force_tz(input$end_time, "America/Los_Angeles")

    start_utc <- with_tz(start_local, "UTC")
    end_utc <- with_tz(end_local, "UTC")

    list(start_utc = start_utc, end_utc = end_utc)
  })

  filtered_data <- reactive({
    times <- input_times_utc()
    dataELPI %>%
      filter(DateTime_rounded >= times$start_utc,
             DateTime_rounded <= times$end_utc)
  })

  temp_filtered <- reactive({
    times <- input_times_utc()
    temp_dataAQ %>%
      mutate(DateTime_rounded = round_date(with_tz(as.POSIXct(timestamp), tzone = "UTC"), "second")) %>%
      filter(DateTime_rounded >= times$start_utc, DateTime_rounded <= times$end_utc)
  })

  output$airmonitor <- renderTable({
    df <- temp_filtered()
    validate(need(nrow(df) > 0, "No temperature data available for the selected time range."))

    avg_temp <- round(mean(df$temp, na.rm = TRUE), 2)
    avg_rh <- round(mean(df$rh, na.rm = TRUE), 2)

    data.frame(
      Variable = c("Temperature (°C)", "Relative Humidity (%)"),
      Average = c(avg_temp, avg_rh)
    )
  })

  output$summary_stats <- renderTable({
    req(input$columns_select)
    filtered <- filtered_data()

    validate(need(nrow(filtered) > 0, "No particle data available for the selected time range."))

    summary_df <- filtered %>%
      select(DateTime_rounded, all_of(input$columns_select)) %>%
      pivot_longer(cols = all_of(input$columns_select),
                   names_to = "variable",
                   values_to = "value") %>%
      mutate(value = as.numeric(value)) %>%
      group_by(variable) %>%
      summarize(
        Mean = round(mean(value, na.rm = TRUE), 2),
        Median = round(median(value, na.rm = TRUE), 2),
        StandardDeviation = round(sd(value, na.rm = TRUE), 2),
        CoVar = ifelse(mean(value, na.rm = TRUE) != 0,
                       round(sd(value, na.rm = TRUE) / mean(value, na.rm = TRUE), 2),
                       NA_real_),
        Maximum = round(max(value, na.rm = TRUE), 2),
        Range = round(max(value, na.rm = TRUE) - min(value, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      pivot_longer(cols = -variable, names_to = "statistic", values_to = "value") %>%
      pivot_wider(names_from = variable, values_from = value) %>%
      arrange(statistic)
  })

  output$time_series_plot <- renderPlot({
    req(input$columns_select)
    filtered <- filtered_data()
    validate(need(nrow(filtered) > 0, "No particle data available for the selected time range."))

    combined <- filtered %>%
      left_join(dataTruck, by = "DateTime_rounded")

    plot_data <- combined %>%
      select(DateTime_rounded, all_of(input$columns_select), WheelBasedVehicleSpeed) %>%
      pivot_longer(cols = all_of(input$columns_select),
                   names_to = "variable",
                   values_to = "value") %>%
      mutate(value = as.numeric(value))

    max_particle <- max(plot_data$value, na.rm = TRUE)
    max_speed <- max(combined$WheelBasedVehicleSpeed, na.rm = TRUE)
    speed_scale_factor <- ifelse(max_speed > 0, max_particle / max_speed, 1)

    ggplot(plot_data, aes(x = DateTime_rounded)) +
      geom_line(aes(y = value, color = variable), linewidth = 0.5) +
      geom_line(aes(y = WheelBasedVehicleSpeed * speed_scale_factor),
                data = combined, color = "grey50", alpha = 0.5) +
      scale_y_continuous(
        name = "Particle Count (Relative)",
        sec.axis = sec_axis(~ . / speed_scale_factor, name = "Vehicle Speed (km/h)")
      ) +
      labs(title = "ELPI: Particulate Release from Chassis Dynamometer", x = "") +
      theme_classic() +
      theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold")
      ) +
      scale_x_datetime(
        breaks = scales::pretty_breaks(n = 6),
        labels = function(x) format(with_tz(x, "America/Los_Angeles"), "%H:%M")
      )
  })

  output$airmonitor_plot <- renderPlot({
    df <- temp_filtered()
    validate(need(nrow(df) > 0, "No temperature data available."))

    max_temp <- max(df$temp, na.rm = TRUE)
    min_temp <- min(df$temp, na.rm = TRUE)
    max_rh <- max(df$rh, na.rm = TRUE)
    min_rh <- min(df$rh, na.rm = TRUE)

    scale_factor <- (max_temp - min_temp) / (max_rh - min_rh)
    offset <- min_temp - min_rh * scale_factor

    ggplot(df, aes(x = DateTime_rounded)) +
      geom_line(aes(y = temp, color = "Temperature"), size = 0.5) +
      geom_line(aes(y = rh * scale_factor + offset, color = "Relative Humidity"), size = 0.5, alpha = 0.7) +
      labs(title = "QuantAQ: Air Quality", x = "", y = "Temperature (°C)", color = "") +
      theme_classic() +
      theme(
        legend.position = "bottom",
        text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 16, face = "bold")
      ) +
      scale_color_manual(values = c("Temperature" = "forestgreen", "Relative Humidity" = "dodgerblue")) +
      scale_x_datetime(
        breaks = scales::pretty_breaks(n = 6),
        labels = function(x) format(with_tz(x, "America/Los_Angeles"), "%H:%M")
      ) +
      scale_y_continuous(
        name = "Temperature (°C)",
        sec.axis = sec_axis(~ (. - offset) / scale_factor, name = "Relative Humidity (%)")
      )
  })
}

shinyApp(ui, server)

