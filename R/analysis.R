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
  curv_radius <- curvature(data_loc$time, data_loc$x, data_loc$y)

  # Paste everything together
  data <- cbind(data_loc, speeds, aspeed, accels, aaccel, curv_radius)
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

#' Return curvatures
#'
#' @param t The times vector
#' @param x The x positions
#' @param y The y positions
#'
#' @return The local radii of curvature
#' @export
#'
#' @seealso \code{\link{speed}, \link{accel}}
#'
curvature <- function(t, x, y) {
  # First get speeds and accelerations
  speeds <- speed(t, x, y)
  aspeed <- sqrt(speeds$vx^2 + speeds$vy^2)
  accels <- accel(t, x, y)

  # Calculate the local radius of curvature
  cross_prod <- speeds$vx*accels$ay - speeds$vy*accels$ax
  curv_radius <- abs(aspeed^3)/abs(cross_prod)
}

#' Returns a dataframe with rows filtered
#'
#' @param input_data The clean data from all locations
#' @param filter_location The location on which the data will be filtered
#'
#' @return A data frame including only the rows belonging to one location
#' @export
#'
filter_data <- function(input_data, filter_location){
  input_data %>% filter(location == filter_location) 
}


#' Returns a dataframe with extra columns with polar coordinates
#'
#' @param data_loc The clean data from a given location
#'
#' @return A data frame including the polar coordinates
#' @export
#'
#' @seealso \code{\link{speed}, \link{accel}}
#'
append_polar_coordinates <- function(data_loc) {
  
  # Load data on arenas
  # Load coordinates of gammarus or snail protocol
  arenas <- read.csv(paste('data/arenas_', data_loc$test_species[1], '.csv', sep = ''))
  # Convert scale
  scale_value <- 0.46 # This value is extracted from the protocol
  arenas[2:5] <- apply(arenas[2:5], 2, function(x) x*scale_value)
  # Merge with other data
  data_loc <- merge(data_loc, arenas, by.x = 'location_temp', by.y = 'id')
  
  # # Calculate min and max of x and y coordinates
  # max_x <- max(data_loc$x)
  # min_x <- min(data_loc$x)
  # max_y <- max(data_loc$y)
  # min_y <- min(data_loc$y)
  
  # Calculate center of the petridish
  #r_0_x <- mean(c(max_x, min_x))
  r_0_x <- data_loc$cx[1]
  #r_0_y <- mean(c(max_y, min_y))
  r_0_y <- data_loc$cy[1]
  
  # Correct the coordinates according to the center of the petridish
  x_r <- data_loc$x-r_0_x
  y_r <- data_loc$y-r_0_y
  
  # Calculate the polar coordinates
  r <- sqrt(x_r^2+y_r^2)
  th <- atan2(y = y_r, x = x_r)
  
  # Calculate the cumulative distance traveled
  d <- cumsum(r) # TODO check this function
  
  # Paste everything together
  data <- cbind(data_loc, x_r, y_r, r, th, d)
}

#' Returns a dataframe with an extra column containing the timebin
#'
#' @param data_loc The clean data from a given location
#'
#' @return A data frame including the timebin
#' @export
#'
#' @seealso \code{\link{speed}, \link{accel}}
#'
append_time_bins <- function(data_loc) {
  data_loc$time_bin <- ifelse(data_loc$time < 1.2e8, 1, 
                              ifelse(data_loc > 1.2e8 & data_loc$time < 2.4e8, 2, 
                                     ifelse(data_loc$time > 2.4e8 & data_loc$time < 3.6e8, 3, 4)))
  # Convert microseconds to seconds
  data_loc$time <- data_loc$time/1e6
  return(data_loc)
}


#' Returns a dataframe with extra columns indicating experimental information
#'
#' @param data_loc The clean data from a given location
#' @param file_name The original name of the file
#'
#' @return A data frame including the cosmnr
#' @export
#'
#' @seealso \code{\link{speed}, \link{accel}}
#'
append_exp_info <- function(data_loc, file_name){
  # Split filename so that experimental information can be extracted
  file_name <- strsplit(file_name, ' ')
  # Extract cosm numbers
  cosm_nrs <- unlist(file_name)[6]
  cosm_nrs <- strsplit(cosm_nrs, '&')
  cosm_nrs <- as.integer(unlist(cosm_nrs))
  # Extract species
  test_species <- unlist(file_name)[3]
  # Extract test date
  test_date <- unlist(strsplit(unlist(file_name)[1], '/'))[2]
  
  # Convert location to numeric vector
  data_loc$location_temp <- as.integer(strsplit(data_loc$location, 'Loc')[[1]][2])
  # Add cosm nr to dataframe
  data_loc$cosm_nr <- ifelse(data_loc$location_temp <= 10, cosm_nrs[1], cosm_nrs[2])
  ## Add animal nr per cosm
  #data_loc$location_temp <- ifelse(data_loc$location_temp <= 10, seq(1,10,1), seq(1,10,1))
  data_loc$location_temp <- ifelse(data_loc$location_temp <= 10, data_loc$location_temp,
                                   (data_loc$location_temp - 10))
  # Add species to dataframe
  data_loc$test_species <- test_species
  # Add test date to dataframe
  data_loc$test_date <- test_date
  
  # Return extended data
  return(data_loc)
}