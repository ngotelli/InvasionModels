
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


### make maps of predictions
library(raster)
library(gtools)
library(colorRamps)

# march to june
# max temp
tmx <- mixedsort(list.files(path="/Volumes/dataSSD/climateData/current/2.5min/maxTemp",
           pattern="tmax_",
           full.names = T))[3:6]
tmx <- stack(tmx)
tmx <- crop(tmx, extent(-200,-55,10,80))

# min temp
tmn <- mixedsort(list.files(path="/Volumes/dataSSD/climateData/current/2.5min/minTemp",
                            pattern="tmin_",
                            full.names = T))[3:6]
tmn <- stack(tmn)
tmn <- crop(tmn, extent(-200,-55,10,80))

# mean temp during breeding season
tmean_bs <- mean(mean(tmn[[1]], tmx[[1]]), 
     mean(tmn[[2]], tmx[[2]]), 
     mean(tmn[[3]], tmx[[3]]),
     mean(tmn[[4]], tmx[[4]]))/10

# now apply Nick's function
tmean_bs.vect <- tmean_bs[]
growthPred <- r_function(tmean_bs.vect)

# make a map
growthPred.r <- tmean_bs
growthPred.r[] <- growthPred
pos <- growthPred.r
pos <- pos>0
growthPred.r <- growthPred.r*pos
plot(growthPred.r, col=rgb.tables(1000), main="House sparrow population growth")
