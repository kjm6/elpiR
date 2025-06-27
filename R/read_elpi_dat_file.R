#' Read ELPI+ .dat File
#'
#' Reads an ELPI+ formatted .dat file and returns a tidy dataframe.
#'
#' @param file_path Path to the .dat file.
#'
#' @return A tibble with DateTime and particle stage columns.
#' @export
#' @examples read_elpi_data("path/to/your/file.dat")

read_elpi_dat_file<- function(file_path) {
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(lubridate)

  lines <- readLines(file_path)

  # Extract start date and time from metadata
  date_line <- str_extract(lines[str_detect(lines, "^Date=")], "\\d{2}/\\d{2}/\\d{4}")
  time_line <- str_extract(lines[str_detect(lines, "^Time=")], "\\d{2}:\\d{2}:\\d{2}")
  if (is.na(date_line) || is.na(time_line)) {
    stop("Start date or time not found in file header.")
  }

  start_datetime <- dmy_hms(paste(date_line, time_line))

  # Extract DataOrder header
  dataorder_line <- str_remove(lines[str_detect(lines, "^DataOrder=")], "^DataOrder=")
  raw_headers <- str_split(dataorder_line, "\t|\\s{2,}|\u00A0|\\s+")[[1]] # handles variable spacing

  # Make column names unique
  headers <- make.unique(raw_headers)

  # Find [Data] section
  data_start <- which(str_detect(lines, "^\\[Data\\]")) + 1
  data_lines <- lines[data_start:length(lines)]

  # Read the data block into a dataframe
  data_raw <- read_delim(
    paste(data_lines, collapse = "\n"),
    delim = "\t",
    col_names = headers,
    col_types = cols(.default = col_character())
  )

  # Convert Time column to POSIXct using start_datetime
  # Assume first column is relative time (mm:ss.s)
  time_strings <- data_raw[[1]]

  # Convert relative times to seconds
  time_seconds <- str_split_fixed(time_strings, ":", 2) %>%
    as.data.frame() %>%
    mutate(
      minutes = as.numeric(V1),
      seconds = as.numeric(V2),
      total_seconds = minutes * 60 + seconds
    )

  data <- data_raw %>%
    mutate(
      DateTime = start_datetime + seconds(time_seconds$total_seconds)
    ) %>%
    select(DateTime, everything(), -1) %>%
    mutate(across(-DateTime, parse_guess))

  return(data)
}
