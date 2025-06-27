read_elpi_dat_file <- function(file_path) {

  library(readr)
  library(stringr)
  library(dplyr)
  library(purrr)
  library(lubridate)

  lines <- readLines(file_path)

  # Extract headers
  header_str <- str_remove(lines[str_detect(lines, "^DataOrder=")], "^DataOrder=")
  header_list <- str_split(header_str, ",")[[1]]

  make_unique_names <- function(names_vector) {
    counts <- table(names_vector)
    suffixes <- c("raw", "calc")
    new_names <- character(length(names_vector))
    used <- list()
    for (i in seq_along(names_vector)) {
      name <- names_vector[i]
      if (counts[name] == 1) {
        new_names[i] <- name
      } else {
        if (is.null(used[[name]])) used[[name]] <- 0
        used[[name]] <- used[[name]] + 1
        suffix <- suffixes[used[[name]]] %||% as.character(used[[name]])
        new_names[i] <- paste0(name, "_", suffix)
      }
    }
    return(new_names)
  }

  headers <- make_unique_names(header_list)

  # Extract data lines
  data_start <- which(str_detect(lines, "^\\[Data\\]")) + 1
  data_lines <- lines[data_start:length(lines)]

  # Read as character
  data_raw <- read_delim(
    paste(data_lines, collapse = "\n"),
    delim = ",",
    col_names = headers,
    col_types = cols(.default = col_character())
  )

  # Parse DateTime and drop raw column
  datetime_col <- headers[str_detect(headers, "DateTime")][1]
  data <- data_raw %>%
    mutate(
      DateTime_raw = parse_date_time(.data[[datetime_col]], orders = "Y/m/d H:M:OS"),
      DateTime = round_date(DateTime_raw, unit = "second")
    ) %>%
    select(-all_of(datetime_col), -DateTime_raw)

  # Convert applicable columns to numeric
  data <- data_raw %>%
    mutate(across(
      where(function(col) {
        is.character(col) && all(str_detect(col[!is.na(col)], "^[-+eE0-9.]+$"))
      }),
      as.numeric
    ))

  return(data)
}

