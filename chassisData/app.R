library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)

# Load your data
dataAll <- readRDS("C:/Users/katie/OneDrive - UBC/Research/Tirewear-particulate/R/elpiR/dataAll.rds")

# UI
ui <- fluidPage(
  titlePanel("ELPI Data Viewer"),
  sidebarLayout(
    sidebarPanel(
      airDatepickerInput(
        inputId = "start_time",
        label = "Start Date-Time",
        value = min(dataAll$DateTime),
        timepicker = TRUE,
        timepickerOpts = list(enableSeconds = TRUE)
      ),
      airDatepickerInput(
        inputId = "end_time",
        label = "End Date-Time",
        value = max(dataAll$DateTime),
        timepicker = TRUE,
        timepickerOpts = list(enableSeconds = TRUE)
      ),
      selectInput(
        inputId = "column_select",
        label = "Select Column to Plot",
        choices = names(dataAll)[grepl("Stage\\d+_raw|PM1|PM2.5|PM10", names(dataAll))],
        selected = "PM2.5"
      )
    ),
    mainPanel(
      plotOutput("time_series_plot")
    )
  )
)

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    req(input$start_time, input$end_time)
    dataAll %>%
      filter(DateTime >= input$start_time,
             DateTime <= input$end_time)
  })

  output$time_series_plot <- renderPlot({
    req(input$column_select)
    ggplot(filtered_data(), aes(x = DateTime, y = .data[[input$column_select]])) +
      geom_line(color = "#0072B2") +
      labs(title = paste("Time Series of", input$column_select),
           x = "Time", y = input$column_select) +
      theme_minimal()
  })
}

shinyApp(ui, server)
