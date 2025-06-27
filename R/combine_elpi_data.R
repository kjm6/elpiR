#' Read and combine multiple ELPI+ .dat files
#'
#' Reads multiple ELPI+ formatted .dat files and combines them into one tidy tibble.
#'
#' @param file_paths A character vector of file paths to ELPI .dat files.
#' @param add_filename Logical, if TRUE adds a column with the source filename.
#'
#' @return A combined tibble with data from all files.
#' @export
#'
#' @examples
#' files_list <- c("path/to/file1.dat", "path/to/file2.dat")
#' combined <- combine_elpi_files(files_list, add_filename = TRUE)



combine_elpi_data <- function(file_paths, add_filename = TRUE) {
  library(dplyr)
  library(purrr)

  # Read each file and optionally add filename column
  data_list <- map(file_paths, function(fp) {
    df <- read_elpi_dat_file(fp)
    if (add_filename) {
      df <- df %>% mutate(source_file = basename(fp))
    }
    return(df)
  })

  # Combine all data frames
  combined <- bind_rows(data_list)

  return(combined)
}
