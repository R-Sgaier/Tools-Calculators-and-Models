
# Import data from filled excel template

library(readxl)
data <- read_excel("Template_Data_Entry.xlsx",
col_types = c("text", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric"))


# Function to remove rows with only NA values
#' Title remove_na_rows
#'
#' @param data
#'
#' @return data matrix, empty rows removed
#' @export
#'
#' @examples
remove_na_rows <- function(data) {
  # First column as row names
  rownames(data) <- data[, 1]
  data <-data[, -1]
  # Use the `rowSums` function to identify rows with all NA values
  non_na_rows <- rowSums(!is.na(data)) > 0

  # Subset the data to keep only rows that have at least one non-NA value
  cleaned_data <- data[non_na_rows, ]

  return(cleaned_data)
}



# Function to check if all values in a data frame are numeric
#' Title check_all_numeric
#'
#' @param cleaned_data numeric matrix, missing values removed
#'
#' @return
#' @export
#'
#' @examples
check_all_numeric <- function(cleaned_data) {
  # Check if all columns are numeric
  is_all_numeric <- all(sapply(cleaned_data, is.numeric))

  # Print appropriate message
  if (is_all_numeric) {
    message("Data is ready for calculation")
  } else {
    message("Error: Data contains non-numeric values")
  }
}
