# Plotting functions


#' Plot positions
#'
#' @param data_loc Clean dataframe corresponding to a location
#' @param type Type of plot (available types: 'scatter', 'density')
#'
#' @return The plot
#' @export
#'
plot_positions <- function(data_loc, type='scatter') {

  if (type=='scatter') {
    p <- plot(data_loc$x, data_loc$y, pch=19, col=rgb(0, 0, 0, 0.15))
  } else if (type == 'density') {
    k <- kde2d(data$x, data$y, n=100)
    p <- image(k, col = topo.colors(100))
  }

  return(p)
}
