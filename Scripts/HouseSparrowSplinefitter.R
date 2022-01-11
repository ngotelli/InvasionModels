
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

# 02 July 2021 - updated spline function to include 
# new limits after discussion with Jesse

# NJG
# --------------------------------------
library(tidyverse)
library(stats)

# create spline function
# x = min, optimal, and max temperature (C)
# y = estimated r at each temperature

# this function updated with new values from Jesse 2 July 2021
#r_function <- splinefun(x=c(8,21,28),
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


### make maps of predictions
library(raster)
library(gtools)
library(colorRamps)
library(viridisLite)

# march to june
# max temp
tmx <- mixedsort(list.files(path="/Volumes/generalData/climateData/current/2.5min/wc2.1_2.5m_tmax/",
           pattern="tmax_",
           full.names = T))[3:7]
tmx <- stack(tmx)
tmx <- crop(tmx, extent(-200,-55,10,80))

# min temp
tmn <- mixedsort(list.files(path="/Volumes/generalData/climateData/current/2.5min/wc2.1_2.5m_tmin/",
                            pattern="tmin_",
                            full.names = T))[3:7]
tmn <- stack(tmn)
tmn <- crop(tmn, extent(-200,-55,10,80))

# mean temp during breeding season (march-june)
tmean_bs <- mean(mean(tmn[[1]], tmx[[1]]), 
     mean(tmn[[2]], tmx[[2]]), 
     mean(tmn[[3]], tmx[[3]]),
     mean(tmn[[4]], tmx[[4]]))

# get NA IDs, etc for calcs and mapping
mask <- tmean_bs
mask <- mask>-1000
nas <- which(is.na(mask[]))
getEm <- which(mask[]==1)

# now apply Nick's function
tmean_bs.vect <- tmean_bs[getEm]
growthPred <- r_function(tmean_bs.vect)

# make a map
growthPred.r <- tmean_bs
growthPred.r[getEm] <- growthPred
pos <- growthPred.r
neg <- pos<0
neg[neg[]==0] <- NA
pos <- pos>0
growthPred.r <- growthPred.r*pos
growthPred.r[growthPred.r[]==0] <- NA


tooCold <- tmean_bs<=8 & neg==1
tooCold[tooCold[]==0] <- NA

tooWarm <- tmean_bs>=24 & neg==1
tooWarm[tooWarm[]==0] <- NA

# plot
png(filename="/Users/mfitzpatrick/code/InvasionModels/Graphics/houseSparrow_tempMean_TEST.png",
     width=6, height=6, units="in", res=300)
#plot(pos, main="House sparrow population growth (mean temp)", legend=F, col="gray80")
plot(growthPred.r, col=viridis(1000, direction = -1), 
     main="House sparrow population growth (mean temp)")
plot(tooCold, col=rgb(0,181/255,226/255,0.25), legend=F, add=T)
plot(tooWarm, col=rgb(255/255,134/255,116/255, 0.5), legend=F, add=T)
dev.off()


# individual months within the breeding season
tmean_mar <- mean(tmn[[1]], tmx[[1]])
tmean_apr <- mean(tmn[[2]], tmx[[2]])
tmean_may <- mean(tmn[[3]], tmx[[3]])
tmean_jun <- mean(tmn[[4]], tmx[[4]])
tmean_jul <- mean(tmn[[5]], tmx[[5]])

tmean_months <- stack(tmean_mar, tmean_apr, tmean_may,
                      tmean_jun, tmean_jul)

growthPred_months <- lapply(1:nlayers(tmean_months), function(x, rasts, datCells){
    # now apply Nick's function
    tmean_month.vect <- rasts[[x]][datCells]
    growthPred <- r_function(tmean_month.vect)
    # make a map
    growthPred.r <- rasts[[x]]
    growthPred.r[datCells] <- growthPred
    growthPred.r[growthPred.r[]<0] <- 0
    return(growthPred.r)
}, rasts = tmean_months, datCells = getEm)

growthPred_months <- do.call(stack, growthPred_months)
names(growthPred_months) <- month.name[3:7]

growthPred_months_mean_mar_june <- mean(growthPred_months[[1:4]])
pos <- growthPred_months_mean_mar_june
pos <- pos>0
growthPred_months_mean_mar_june <- growthPred_months_mean_mar_june*pos
growthPred_months_mean_mar_june[growthPred_months_mean_mar_june[]==0] <- NA

# plot
png(filename="/Users/mfitzpatrick/code/InvasionModels/Graphics/houseSparrow_monthlyR_Mean_mar_jun.png",
    width=6, height=6, units="in", res=300)
plot(pos, main="Population growth (monthly r mean: Mar-June)", legend=F, col="gray80")
plot(growthPred_months_mean_mar_june, col=rgb.tables(1000), 
     main="Population growth (monthly r mean: Mar-June)",
     add=T)
dev.off()

growthPred_months_mean <- mean(growthPred_months)
pos <- growthPred_months_mean
pos <- pos>0
growthPred_months_mean <- growthPred_months_mean*pos
growthPred_months_mean[growthPred_months_mean[]==0] <- NA

# plot
png(filename="/Users/mfitzpatrick/code/InvasionModels/Graphics/houseSparrow_monthlyR_Mean_mar_july.png",
    width=6, height=6, units="in", res=300)
plot(pos, main="Population growth (monthly r mean: Mar-July)", legend=F, col="gray80")
plot(growthPred_months_mean, col=rgb.tables(1000), 
     main="Population growth (monthly r mean: Mar-July)",
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
