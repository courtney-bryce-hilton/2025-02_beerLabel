#
#   Want black background with lines shooting out from middle??
#
# libraries ---------------------------------------------------------------


library(tidyverse)
library(here)


# generate data -----------------------------------------------------------


set.seed(123)

# number of lines to generate
n <- 40

# define the centre of the canvas and the radius
center_x <- 0
center_y <- 0
radius <- 10

# generate n random angles (in radians) between 0 and 2*pi
angles <- runif(n, 0, 2*pi)

# calculate the line start and end points
lines_df <- tibble(
  x  = rep(center_x, n),
  y  = rep(center_y, n),
  xend = center_x + radius * cos(angles),
  yend = center_y + radius * sin(angles),
  thickness = rnorm(n, mean = 1, sd = 0.5)
)


# plot --------------------------------------------------------------------


ggplot(lines_df) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend, linewidth = thickness), colour = "white") +
  scale_linewidth_identity() +
  coord_equal(xlim = c(-5,5), ylim = c(-5, 5)) +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = "black")
  )

ggsave(filename = "design.png", width = 6, height = 6, dpi = 300)


# -------------------------------------------------------------------------