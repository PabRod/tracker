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
  curv <- curvature(data_loc$time, data_loc$x, data_loc$y)
  curv_radius <- curvature_radius(data_loc$time, data_loc$x, data_loc$y)

  # Paste everything together
  data <- cbind(data_loc, speeds, aspeed, accels, aaccel, curv, curv_radius)
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
#' @return The local curvature
#' @export
#'
#' @seealso \code{\link{speed}, \link{accel}, \link{curvature_radius}}
#'
curvature <- function(t, x, y) {
  # First get speeds and accelerations
  speeds <- speed(t, x, y)
  aspeed <- sqrt(speeds$vx^2 + speeds$vy^2)
  accels <- accel(t, x, y)

  # Calculate the cross product
  cross_prod <- speeds$vx*accels$ay - speeds$vy*accels$ax

  # Apply the definition of curvature
  curv <- abs(cross_prod)/abs(aspeed^3)
}

#' Return curvature radius
#'
#' @param t The times vector
#' @param x The x positions
#' @param y The y positions
#'
#' @return The local curvature radius
#' @export
#'
#' @seealso \code{\link{speed}, \link{accel}, \link{curvature}}
#'
curvature_radius <- function(t, x, y) {
  # The curvatre radius is just the inverse of the local curvature
  curvatures <- curvature(t, x, y)
  curv_radius <- 1 / curvatures
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
append_polar_coordinates <- function(data_loc) {

  # Load data on arenas
  # Load coordinates of gammarus or snail protocol
  arenas <- read.csv(paste('data/arenas_', data_loc$test_species[1], '.csv', sep = ''))
  # Convert scale
  scale_value <- 0.46 # This value is extracted from the protocol
  arenas[2:5] <- apply(arenas[2:5], 2, function(x) x*scale_value)
  # Combination of cosm and ind determines absolute position on board
  data_loc$ind_abd <- ifelse(is_even(data_loc$cosm_nr[[1]]), data_loc$ind+10, data_loc$ind)
  # Merge with other data
  data_loc <- merge(data_loc, arenas, by.x = 'ind_abd', by.y = 'id')

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
append_time_bins <- function(data_loc) {
  data_loc$light_interval <- ifelse(data_loc$time < 1.2e8, 1,
                              ifelse(data_loc > 1.2e8 & data_loc$time < 2.4e8, 2,
                                     ifelse(data_loc$time > 2.4e8 & data_loc$time < 3.6e8, 3, 4)))
  ## Create a binary variable indicating when light is on or off
  data_loc$light_on_off <- ifelse(data_loc$light_interval == 1 |
                                    data_loc$light_interval == 3, 0, 1)
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
append_exp_info <- function(data_loc, file_name){
  # Split filename so that experimental information can be extracted
  file_name <- strsplit(file_name, ' ')
  # Extract cosm numbers
  cosm_nrs <- unlist(file_name)[7]
  cosm_nrs <- strsplit(cosm_nrs, '&')
  cosm_nrs <- as.integer(unlist(cosm_nrs))
  # Extract species
  test_species <- unlist(file_name)[4]
  # Extract test date
  test_date <- unlist(strsplit(unlist(file_name)[2], '/'))[2]

  # Convert location to numeric vector
  data_loc$ind <- as.integer(strsplit(data_loc$location, 'Loc')[[1]][2])
  # Add cosm nr to dataframe
  data_loc$cosm_nr <- ifelse(data_loc$ind <= 10, cosm_nrs[1], cosm_nrs[2])
  ## Add animal nr per cosm
  if(test_species == 'Gammarus'){
    data_loc$ind <- ifelse(data_loc$ind <= 10, data_loc$ind,
                           (data_loc$ind - 10))
  }
  #data_loc$ind <- ifelse(data_loc$ind <= 10, seq(1,10,1), seq(1,10,1)
  # Add species to dataframe
  data_loc$test_species <- test_species
  # Add test date to dataframe
  data_loc$test_date <- test_date

  # Return extended data
  return(data_loc)
}


#' Returns a dataframe with data summarised to the defined timebin
#'
#' @param input_data The clean data
#' @param chemical The name of the chemical that is going to be summarised
#' @param exp_dur The duration of the total experiment
#' @param timebin The timebin over which the data is going to be summarised
#' @expsr_dur The exposure duration of the experiment
#'
#' @return A data frame that gives the mean and SD of the velocity over the timebins
#' @export
#'
#' @seealso \code{\link{speed}, \link{accel}}
#'
## Make combined heatmap for Gammarus Acute and Chronic
summarise_data <- function(input_data, chemical, exp_dur, timebin, expsr_dur){
  # Only focus on one chemical
  data <- filter(input_data, Treatment_chem == chemical | Treatment_chem == 'C')

  # Create timebins
  bins <- seq(0, exp_dur, timebin)
  # Add timebins to data
  data$group <- cut(data$time, bins, labels = FALSE)
  # Remove NAs (time > 480)
  data <- data[!is.na(data$group),]
  # Add interaction between timebin and treatment group
  data$combined_group <- interaction(data$group, data$Treatment_conc)

  # Summarise by group
  data_summarised <- data %>% group_by(combined_group) %>%
    summarise(avaspeed = mean(aspeed),
              sdaspeed = sd(aspeed),
              group = mean(group),
              Treatment_conc = mean(Treatment_conc))
  data_summarised$expsr_dur <- expsr_dur
  return(data_summarised)
}

is_even <- function(x) x %% 2 == 0
