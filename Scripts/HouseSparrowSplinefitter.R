
# --------------------------------------
# spline functions for curve fitting
# to Jesse Krause's data on house sparrow
# growth. This model creates a function
# based on physiological estimates of
# minimum, maximum, and optimal temperatures
# for performance from 

# Kendeigh, S.C 1976. Latitudinal trends in the metabolic adjustmets of the house sparrow. Ecology 57: 509-519.

# The maximum r is set at 0.50, slightly higher than the largest value I calculated from the spreadsheets that Jesse sent. However, for this particular model, the actual value is arbitrary and does not affect the appearance of the map, just the scaling. The optimum temperature does affect the appearance of the contours, but the most important issue is the max and min values because these determine the areas where dN/dt > 0.

# 04 Oct 2020
# NJG
# --------------------------------------
library(tidyverse)
library(stats)

# create spline function
# x = min, optimal, and max temperature (C)
# y = estimated r at each temperature
r_function <- splinefun(x=c(8,21,28),
          y=c(0,0.6,0),
          method="fmm")

# create a vector of temperatures
Temperature <- seq(0,30)


# get estimated growth rates from function
# Matt will use this for creating the map
r <- r_function(Temperature)

# plot results
qplot(x=Temperature,y=r)
