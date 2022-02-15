# Update from the original HouseSparrowSplineFitter.R script to use a new function for the spline, and to create a graph illustrating the function and the points.

# 17 June 2021
# NJG
# --------------------------------------
# spline functions for curve fitting
# to Jesse Krause's data on house sparrow
# growth. This model creates a function
# based on physiological estimates of
# minimum, maximum, and optimal temperatures
# for performance from 

# Kendeigh, S.C 1976. Latitudinal trends in the metabolic adjustmets of the house sparrow. Ecology 57: 509-519.

# The maximum r is set at 0.60, slightly higher than the largest value I calculated from the spreadsheets that Jesse sent. However, for this particular model, the actual value is arbitrary and does not affect the appearance of the map, just the scaling. The optimum temperature does affect the appearance of the contours, but the most important issue is the max and min values because these determine the areas where dN/dt > 0.

# 04 Oct 2020
# NJG
# --------------------------------------
library(tidyverse)
library(stats)

# create spline function
# x = min, optimal, and max temperature (C)
# y = estimated r at each temperature

# original limits used from Jesse
# r_function <- splinefun(x=c(8,21,28),

# Updated limits from Jesse (2 July 2021)
# to better get performance at extremes
r_function <- splinefun(x=c(8,17,24),
          y=c(0,0.6,0),
          method="monoH.FC")

# create a vector of temperatures
Temperature <- seq(0,30)


# get estimated growth rates from function
# Matt will use this for creating the map
r <- r_function(Temperature)

# plot results
qplot(x=Temperature,y=r)

d_frame <- data.frame(Temperature,r)
head(d_frame)

Sparrow_Plot <- ggplot(d_frame) +
                aes(x=Temperature,y=r) +
                geom_line() +
                theme_bw(base_size=25) +
                geom_hline(yintercept=0,linetype="dashed") +
                ylim(c(-0.1,0.65)) + 
                labs(x = expression("Temperature " ( degree*C))) +
                annotate(geom="point",
                         x=c(8,17,24),
                         y=c(0,0.6,0),
                         size=5,
                         shape=21,
                         fill="white") +
                xlim(c(5,32))

plot(Sparrow_Plot)
ggsave(filename="Graphics/SparrowSpline.jpeg",
       plot=Sparrow_Plot,
       device="jpg",
       width=7,
       height=5,
       units="in")
