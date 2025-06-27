#' Read ELPI+ .dat File
#'
#' Reads an ELPI+ formatted .dat file and returns a tidy dataframe.
#'
#' @param file_path Path to the .dat file.
#'
#' @return A tibble with DateTime and particle stage columns.
#' @export
#' @examples read_elpi_data("path/to/your/file.dat")

read_elpi_dat_file <- function(file_path) {

  library(readr)
  library(stringr)
  library(dplyr)
  library(purrr)

  # Read file
  lines <- readLines(file_path)

  # Extract headers
  header_str <- str_remove(lines[str_detect(lines, "^DataOrder=")], "^DataOrder=")
  header_list <- str_split(header_str, ",")[[1]]

  # Function to make duplicate names unique (e.g. Stage1_raw, Stage1_calc)
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

  # Read data as character
  data_raw <- read_delim(
    paste(data_lines, collapse = "\n"),
    delim = ",",
    col_names = headers,
    col_types = cols(.default = col_character())
  )

  # Detect datetime column
  datetime_col <- headers[str_detect(headers, "DateTime")][1]

  # Clean and convert
  data <- data_raw %>%
    mutate(
      DateTime_raw = parse_date_time(`DateTime(yyyy/mm/dd hh:mm:ss)`, orders = "Y/m/d H:M:OS"),
      DateTime = round_date(DateTime_raw, unit = "second")
    ) %>%
    select(-`DateTime(yyyy/mm/dd hh:mm:ss)`, -DateTime_raw)



  return(data)
}
