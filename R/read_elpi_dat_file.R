#' Read and Process ELPI Data File
#'
#' Reads an ELPI (Electrical Low Pressure Impactor) data file, extracts and parses
#' the data including timestamps, converts applicable columns to numeric, and
#' renames particle size stage columns to more informative names with median diameters.
#'
#' @param file_path Character string. Path to the ELPI data file to be read.
#'
#' @return A tibble containing the processed ELPI data with:
#'   \itemize{
#'     \item A POSIXct \code{DateTime} column rounded to the nearest second.
#'     \item Numeric columns for particle stages and other measurements.
#'     \item Particle stage columns renamed to include approximate median diameter.
#'   }
#'
#' @details
#' The function:
#' \itemize{
#'   \item Reads the raw data lines from the file.
#'   \item Extracts the header from the "DataOrder=" line, making duplicate names unique.
#'   \item Parses the datetime column with Pacific Time zone ("America/Los_Angeles").
#'   \item Converts character columns with numeric-like values to numeric.
#'   \item Renames particle stage columns (e.g. \code{Stage1_calc}) to human-readable
#'   names with diameter estimates.
#' }
#'
#' @importFrom readr read_delim
#' @importFrom stringr str_remove str_detect str_split str_detect
#' @importFrom dplyr mutate select rename_with across any_of where
#' @importFrom lubridate parse_date_time round_date
#'
#' @examples
#' \dontrun{
#'   data <- read_elpi_dat_file("path/to/elpi_data_file.dat")
#'   head(data)
#' }
#'
#' @export

read_elpi_dat_file <- function(file_path) {
  library(readr)
  library(stringr)
  library(dplyr)
  library(lubridate)

  # Read the entire file as lines
  lines <- readLines(file_path)

  # Extract headers from "DataOrder=" line
  header_str <- str_remove(lines[str_detect(lines, "^DataOrder=")], "^DataOrder=")
  header_list <- str_split(header_str, ",")[[1]]

  # Function to make duplicate column names unique
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
    new_names
  }

  headers <- make_unique_names(header_list)

  # Extract data starting after the "[Data]" line
  data_start <- which(str_detect(lines, "^\\[Data\\]")) + 1
  data_lines <- lines[data_start:length(lines)]

  # Read data with auto type detection (no col_types override)
  data_raw <- read_delim(
    paste(data_lines, collapse = "\n"),
    delim = ",",
    col_names = headers,
    show_col_types = FALSE
  )

  # Parse datetime
  datetime_col <- headers[str_detect(headers, "DateTime")][1]
  data <- data_raw %>%
    mutate(
      DateTime_raw = parse_date_time(.data[[datetime_col]], orders = "Y/m/d H:M:OS", tz = "America/Los_Angeles"),
      DateTime = round_date(DateTime_raw, unit = "second")
    ) %>%
    select(-all_of(datetime_col), -DateTime_raw)

  # Identify columns to exclude from numeric conversion
  exclude_cols <- c("DateTime", "CAL", "CON", "COM", "UserComment", "source_file", "MISC", "Status")

  # Convert applicable character columns with numeric data to numeric
  numeric_candidates <- data %>%
    select(where(is.character)) %>%
    select(-any_of(exclude_cols)) %>%
    names()

  data <- data %>%
    mutate(across(all_of(numeric_candidates),
                  ~ if (all(str_detect(.[!is.na(.)], "^[-+eE0-9.]+$"))) as.numeric(.) else .))

  stage_diameters <- c(
  "Stage1_calc" = "Stage 1 (Dp50 = 14.2nm)",
  "Stage2_calc" = "Stage 2 (Dp50 = 19.3nm)",
  "Stage3_calc" = "Stage 3 (Dp50 = 33.8nm)",
  "Stage4_calc" = "Stage 4 (Dp50 = 51nm)",
  "Stage5_calc" = "Stage 5 (Dp50 = 98nm)",
  "Stage6_calc" = "Stage 6 (Dp50 = 0.17um)",
  "Stage7_calc" = "Stage 7 (Dp50 = 0.32um)",
  "Stage8_calc" = "Stage 8 (Dp50 = 0.59um)",
  "Stage9_calc" = "Stage 9 (Dp50 = 0.91um)",
  "Stage10_calc" = "Stage 10 (Dp50 = 1.64um)",
  "Stage11_calc" = "Stage 11 (Dp50 = 2.48um)",
  "Stage12_calc" = "Stage 12 (Dp50 = 3.67um)",
  "Stage13_calc" = "Stage 13 (Dp50 = 5.39um)",
  "Stage14_calc" = "Stage 14 (Dp50 = 9.93um)"
)

# Rename only those columns that exist in your data
existing_stage_cols <- intersect(names(data), names(stage_diameters))
data <- data %>%
  rename_with(~ stage_diameters[.x], all_of(existing_stage_cols))

  return(data)
}

