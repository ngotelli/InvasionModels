
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
           full.names = T))[3:7]
tmx <- stack(tmx)
tmx <- crop(tmx, extent(-200,-55,10,80))

# min temp
tmn <- mixedsort(list.files(path="/Volumes/dataSSD/climateData/current/2.5min/minTemp",
                            pattern="tmin_",
                            full.names = T))[3:7]
tmn <- stack(tmn)
tmn <- crop(tmn, extent(-200,-55,10,80))

# mean temp during breeding season (march-june)
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
growthPred.r[growthPred.r[]==0] <- NA

# plot
png(filename="/Users/mfitzpatrick/code/InvasionModels/Graphics/houseSparrow_tempMean.png",
     width=6, height=6, units="in", res=300)
plot(pos, main="House sparrow population growth (mean temp)", legend=F, col="gray80")
plot(growthPred.r, col=rgb.tables(1000), 
     main="House sparrow population growth (mean temp)",
     add=T)
dev.off()


# individual months within the breeding season
tmean_mar <- mean(tmn[[1]], tmx[[1]])/10
tmean_apr <- mean(tmn[[2]], tmx[[2]])/10
tmean_may <- mean(tmn[[3]], tmx[[3]])/10
tmean_jun <- mean(tmn[[4]], tmx[[4]])/10
tmean_jul <- mean(tmn[[5]], tmx[[5]])/10

tmean_months <- stack(tmean_mar, tmean_apr, tmean_may,
                      tmean_jun, tmean_jul)

growthPred_months <- lapply(1:nlayers(tmean_months), function(x, rasts){
        # now apply Nick's function
        tmean_month.vect <- rasts[[x]][]
        growthPred <- r_function(tmean_month.vect)
        # make a map
        growthPred.r <- rasts[[x]]
        growthPred.r[] <- growthPred
        return(growthPred.r)
}, rasts = tmean_months)

growthPred_months <- do.call(stack, growthPred_months)
names(growthPred_months) <- month.name[3:7]

growthPred_months_mean <- mean(growthPred_months)
pos <- growthPred_months_mean
pos <- pos>0
growthPred_months_mean <- growthPred_months_mean*pos
growthPred_months_mean[growthPred_months_mean[]==0] <- NA

# plot
png(filename="/Users/mfitzpatrick/code/InvasionModels/Graphics/houseSparrow_monthlyR_Mean.png",
    width=6, height=6, units="in", res=300)
plot(pos, main="House sparrow population growth (monthly r mean)", legend=F, col="gray80")
plot(growthPred.r, col=rgb.tables(1000), 
     main="House sparrow population growth (monthly r mean)",
     add=T)
dev.off()

# plot
for(i in 1:nlayers(growthPred_months)){
        pos <- growthPred_months[[i]]
        pos <- pos>0
        growthPred_months[[i]] <- growthPred_months[[i]]*pos
        growthPred_months[[i]][growthPred_months[[i]][]==0] <- NA
        fName <- paste0("/Users/mfitzpatrick/code/InvasionModels/Graphics/",
                        "houseSparrow_", month.name[i+2], ".png")
        png(filename=fName, width=6, height=6, units="in", res=300)
        plot(pos, main="House sparrow population growth", legend=F, col="gray80")
        plot(growthPred_months[[i]], col=rgb.tables(1000), 
             main=paste0("House sparrow population growth: ", 
                         names(growthPred_months)[i]),
             add=T)
        dev.off()
}
