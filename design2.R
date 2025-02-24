#
#   Kinda like design 1 but wanna make the lines jaggedier??
#
# libraries ---------------------------------------------------------------


library(tidyverse)
library(here)


# generate data -----------------------------------------------------------


set.seed(1052)
n_lines <- 69
radius <- 10

min_segments <- 15
max_segments <- 30

all_lines <- imap(1:n_lines, \(.line, .i) {
  # randomly choose number of segments (between 3 and 6)
  n_seg <- sample(min_segments:max_segments, 1)
  
  # base angle for this line (in radians)
  theta <- runif(1, 0, 2*pi)
  
  # how jaggedy is it?
  max_dev <- rnorm(1, 2, 1)
  
  # create a sequence from 0 (centre) to 1 (edge)
  f <- seq(0, 1, length.out = n_seg + 1)
  
  # deviation factor is 0 at the start and end, peaking in the middle.
  angle_dev <- max_dev * f * (1 - f)
  
  # angle at each point is the base angle plus the deviation.
  angles <- theta + angle_dev
  
  # make x and y coordinates.
  x <- f * radius * cos(angles)
  y <- f * radius * sin(angles)
  
  line_df <- tibble(
    id   = .i,
    step = 1:length(f), 
    x    = x,
    y    = y,
    linewidth = runif(1, 0.4, 1.5)
  )
  
  return(line_df)
}) |> 
  list_c()


# plot --------------------------------------------------------------------


ggplot(all_lines, aes(x = x, y = y, group = id, linewidth = linewidth)) +
  geom_path(colour = "white") +
  scale_linewidth_identity() +
  coord_equal(xlim = c(-5,5), ylim = c(-5, 5)) +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = "black")
  )

ggsave(filename = "design2.png", path = here(), width = 6, height = 6, dpi = 300)


# -------------------------------------------------------------------------