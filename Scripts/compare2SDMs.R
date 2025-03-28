library(terra)
library(geodata)
library(sf)
library(dplyr)

# load brte cover data from Larson & Tuor (2021) Remote Sensing
# Downs et al. [15] compiled over 24,000 field vegetation measurements in the 
# historic range of sage-grouse that were collected on multiple unrelated field 
# campaigns between 2001 to 2014 (Figure 1). Of these observations, 6418 are 
# deemed useful based on geographic accuracy and overlap with the study area, 
# completeness, and rigor of collection methods. This was further reduced to 5973 
# after removing observations that had incomplete satellite data and were less 60-m 
# apart (i.e., the distance of at least two pixels). Nearest-neighbor spacing of 
# field observations ranged from 61 to 79,421-m with a mean distance of 1837-m. 
# Most of the excluded observations are from the U.S. Department of Agriculture 
# (USDA) Forest Inventory and Analysis program, which does not provide the true 
# geographic location of the publicly available version of its data. All field 
# data were collected from transects ranging from 25 m to 100 m in length using 
# point intercept or standardized plot frame techniques.
vegPlot <- read.csv("/Users/mfitzpatrick/Desktop/fieryfuture/clean_jan_field_data.csv")

vegPlot <- vegPlot %>%
  # select columns of interest
  select(brte_cov, longitude_30m, latitude_30m) %>%
  # remove plots with zero cover
  #filter(brte_cov > 0) %>%
  # convert to sf object
  st_as_sf(coords = c("longitude_30m", "latitude_30m"), crs = 4326)

# download GBIF records for brte - takes several minutes to run
### DATA USED IN THE PAPER ARE HERE: https://doi.org/10.15468/dl.fk2zuc
# bromusGBIF <- sp_occurrence(genus = "Bromus",
#                             species = "tectorum",
#                             download=T,
#                             geo=T,
#                             ext=ext(c(-130, -95, 0, 90)), #western NA
#                             removeZeros = T) %>%
#   # select columns of interest
#   select(species, country, lon, lat, year, coordinateUncertaintyInMeters) %>%
#   #remove records outside of Australia
#   filter(country %in% c("United States", "Canada", "Mexico")) %>%
#   # remove high uncertainty records
#   filter(coordinateUncertaintyInMeters <= 1000) %>%
#   # remove records before 2000
#   filter(year >= 2000) %>%
#   # remove duplicates
#   distinct(lon, lat, .keep_all = TRUE) %>%
#   # convert to sf object
#   st_as_sf(coords = c("lon", "lat"), crs = 4326)

# load prediction rasters
# A typical SDM from McMahon et al. (2021) Diversity & Distributions
bromusSDM <- rast("/Users/mfitzpatrick/Desktop/BRTE_WUS.tif")
names(bromusSDM) <- "bromus_SDM"
# A remote sensing-based map of Bromus from Larson & Tuor (2021) Remote Sensing
bromusRS <- rast("/Users/mfitzpatrick/Desktop/fieryfuture/spatial/kfold_consensus_masked.tif")
names(bromusRS) <- "bromus_remoteSensing"
# demographic model prediction from Gotelli et al.
#bromusLambda <- rast("/Users/mfitzpatrick/Desktop/bromus_lambdaPred.tif") # old version with lambda < 0 ==NA
bromusLambda <- rast("/Users/mfitzpatrick/Desktop/bromus_lambdaPred.v2.tif") # new version with lambda < 0 == 0

# project rasters to common coord system (EPSG:4326, same as the veg plot & GBIF records)
bromusSDM <- project(bromusSDM, bromusLambda)
bromusRS <- project(bromusRS, bromusLambda, method="near")

# stack the rasters
bromusStack <- c(bromusLambda, # Gotelli et al demographic model
                 bromusSDM, # McMahon et al. SDM
                 bromusRS) # Larson & Tuor remote sensing

# extract raster values from the stack @ GBIF points
# also get the x-y coords of the extracted locations
bromusStackGBIF <- extract(bromusStack, bromusGBIF, cells=F, xy=T, ID=F)
head(bromusStackGBIF)
write.csv(bromusStackGBIF, "/Users/mfitzpatrick/code/InvasionModels/Output/bromusStackGBIF.csv")

# extract values from the stack @ veg plots
bromusStackVegPlot <- extract(bromusStack, vegPlot, cells=F, xy=T, ID=F)
head(bromusStackVegPlot)
bromusStackVegPlot <- cbind(bromusStackVegPlot, bromusCover=vegPlot$brte_cov)

# use group_by to calculate mean values of bromus cover for each unique x-y pair
# using .groups argument to avoid warning message
bromusStackVegPlot <- bromusStackVegPlot %>%
  group_by(bromus_lambdaPred.v2, bromus_SDM, bromus_remoteSensing, x, y,) %>%
  summarise(bromusCover=mean(bromusCover, na.rm=T)) %>%
  data.frame()

write.csv(bromusStackVegPlot, "/Users/mfitzpatrick/code/InvasionModels/Output/bromusStackVegPlot.csv")

par(mfrow=c(3,2))
hist(bromusStackVegPlot$bromus_lambdaPred, main="lambda @ veg plots")
hist(bromusStackGBIF$bromus_lambdaPred, main="lambda @ GBIF")
hist(bromusStackVegPlot$bromus_SDM, main="SDM @ veg plots")
hist(bromusStackGBIF$bromus_SDM, main="SDM @ GBIF")
hist(bromusStackVegPlot$bromus_remoteSensing, main="RS @ veg plots")
hist(bromusStackGBIF$bromus_remoteSensing, main="RS @ GBIF")



# crop to the same extent
bromusLambda <- (bromusSDM>=0) * bromusLambda
names(bromusLambda) <- "bromusLambda"

bromusLambda[which(is.na(bromusLambda[]))] <- 0

# make bromusSDM stackable with bromusLambda
bromusSDM <- project(bromusSDM, bromusLambda)
bromusRS <- project(bromusRS, bromusLambda, method="near")




# extract values from the stack
cheatgrass <- extract(bromusStack, bromusGBIF, cells=T)
cheatgrass <- cheatgrass[,-1]

# remove duplicated rows
cheatgrass <- unique(cheatgrass)

# remove NAs
cheatgrass <- cheatgrass[complete.cases(cheatgrass),]

# plot cheatgrass
plot(cheatgrass$BRTE_WUS, cheatgrass$bromusLambda)

cor(cheatgrass$BRTE_WUS, cheatgrass$bromusLambda)

layerCor(bromusStack, fun=cor(bromusStack[[1]], bromusStack[[2]], method="spearman"), use="complete.obs")

sss <- spatSample(bromusStack,
                  size=10000,
                  method="random",
                  na.rm=TRUE)

cor(sss$bromusLambda, sss$BRTE_WUS, method="spearman", use="complete.obs")

plot(sss$bromusLambda, sss$BRTE_WUS)
