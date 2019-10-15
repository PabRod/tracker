# Load packages
library(plotrix)

# Load coordinates of gammarus or snail protocol
test <- read.csv('data/arenas_snails.csv')

# Determine the x- and y-axis limits
min_x <- min(test$cx)-max(c(test$rx, test$ry))
max_x <- max(test$cx)+max(c(test$rx, test$ry))
min_y <- min(test$cy)-max(c(test$rx, test$ry))
max_y <- max(test$cy)+max(c(test$rx, test$ry))

# Plot the centers of the arenas
plot(test$cx, test$cy, xlim = c(min_x, max_x), ylim = c(min_y, max_y))
# And add the ellipses
draw.ellipse(x = test$cx, y = test$cy, a = test$rx, b = test$ry)
