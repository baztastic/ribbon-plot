library(ggplot2)
library(plotly)
# library(rgl) # doesn't work with rstudio server

jupiterRad <- 71492  # radius of Jupiter (km): 71492
moons <- read.csv('galilean_moons.csv')
planets <- read.csv('planets.csv')
