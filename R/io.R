# Input / output functions


#' Import
#'
#' @param path The path to the .csv file
#'
#' @return A clean dataframe
#' @export
#'
#' @seealso \code{\link{clean}}
#'
import <- function(path) {
  raw <- readr::read_csv(path)
  data <- clean(raw)
}

#' Clean and filter the raw input from import
#'
#' @param raw The raw data
#'
#' @return The clean data
#'
#'
#' @seealso \code{\link{import}}
#'
clean <- function(raw) {
  # Select the relevant fields
  data <- dplyr::select(raw, abstime, time, location, data1, data2)

  # Rename the coordinates as x and y
  data <- dplyr::mutate(data, x = data1, y = data2)
  data <- dplyr::select(data, -data1, -data2)

  # Remove artifacts
  data <- dplyr::filter(data, time > 0) # Drop null times
  data <- data[complete.cases(data), ] # Drop nans
  data <- dplyr::filter(data, (x > 0) & (y > 0)) # Drop center
}

