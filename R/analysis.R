# Analysis functions

#' Returns a dataframe with extra columns with dynamical information
#'
#' @param data_loc The clean data from a given location
#'
#' @return A data frame including the speeds and accelerations
#' @export
#'
#' @seealso \code{\link{speed}, \link{accel}}
#'
append_dynamics <- function(data_loc) {
  # Directional dynamical data
  speeds <- speed(data_loc$time, data_loc$x, data_loc$y)
  accels <- accel(data_loc$time, data_loc$x, data_loc$y)

  # Absolute dynamical data
  aspeed <- sqrt(speeds$vx^2 + speeds$vy^2)
  aaccel <- sqrt(accels$ax^2 + accels$ay^2)

  # Paste everything together
  data <- cbind(data_loc, speeds, aspeed, accels, aaccel)
}

#' Return speeds
#'
#' @param t The times vector
#' @param x The x positions
#' @param y The y positions
#'
#' @return The speeds
#' @export
#'
#' @seealso \code{\link{accel}}
#'
speed <- function(t, x, y) {
  # Turn data into function...
  x_fun <- splinefun(t, x)
  y_fun <- splinefun(t, y)

  # ... so numDeriv can be used to differentiate
  vx <- numDeriv::grad(x_fun, t)
  vy <- numDeriv::grad(y_fun, t)

  speeds <- data.frame(vx, vy)
}


#' Return accelerations
#'
#' @param t The times vector
#' @param x The x positions
#' @param y The y positions
#'
#' @return The accelerations
#' @export
#'
#' @seealso \code{\link{speed}}
#'
accel <- function(t, x, y) {
  # First get speeds...
  speeds <- speed(t, x, y)

  # ... and then differentiate again, as in previous function
  vx_fun <- splinefun(t, speeds$vx)
  vy_fun <- splinefun(t, speeds$vy)

  ax <- numDeriv::grad(vx_fun, t)
  ay <- numDeriv::grad(vy_fun, t)

  accels <- data.frame(ax, ay)
}


filter_data <- function(input_data, filter_location){
  input_data %>% filter(location == filter_location) 
}