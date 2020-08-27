library(rgdal)
library(caTools)
library(raster)
library(parallel)
library(usdm)
library(daymetr)
library(rgeos)
library(scales)
library(colorRamps)


# BROMUS -----------------------------------------------------------------------
# create snow water equivalent raster (% days above 0.15 cm)
# See https://nsidc.org/data/g02158 for data download and details
# 
# 'Envi' data files, need to load these and convert to rasters
# enviDats <- list.files(path="/Volumes/localDrobo/Projects/activeProjects/misc/RoL",
#                       pattern=".dat",
#                       full.names = T,
#                       recursive=T)

# run in parallel to convert envi to raster (tif)
# mclapply(enviDats, function(x){
#   from <- "/Volumes/localDrobo/Projects/activeProjects/misc/RoL/us_ssmv11038wS__A0024TTNATS2018013105DP001.hdr"
#   to <- gsub(".dat", ".hdr", x)
#   file.copy(from, to, overwrite = T)
#   ddd <- read.ENVI(x, headerfile = to)
#   rrr <- raster(ddd)
#   projection(rrr) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#   rrr[rrr[]==-9999] <- NA
#   extent(rrr) <- c(-124.73333333333, -66.9416666666666, 24.94, 52.875)
#   writeRaster(rrr/100, gsub(".dat", "_swe_in_cm.tif", x),
#               datatype="FLT4S", overwrite=T)}, mc.cores=10)

# list of newly created tifs, need to stack & calc % days > 0.15 cm
# sweRasts <- list.files(path="/Volumes/localDrobo/Projects/activeProjects/misc/RoL",
#                        pattern=".tif",
#                        full.names = T,
#                        recursive=T)
# sweStack <- stack(sweRasts)

# check if swe is > 0.15 cm
# sweStack <- mclapply(1:nlayers(sweStack), function(x, rStack){
#   return(rStack[[x]]>0.15)}, rStack=sweStack, mc.cores=10)
# sweStack <- do.call(stack, sweStack)

# calc snow cover as % of days > 0.15 cm, for period 12/1/2009-4/14/2010
# swe <- sum(sweStack)/nlayers(sweStack)

# crop out portions in Canada & write to file
# extCrop <- extent(swe)
# extCrop@ymax <- 49
# swe <- crop(swe, extCrop)
# writeRaster(swe, "/Volumes/localDrobo/Projects/activeProjects/misc/RoL/sweProportion.tif",
#             overwrite=T) 


# Calculate winter rain --------------------------------------------------------
# Using daymet data for 12.1.2009-4.14.2010
# read in netCDFs as rasters & subset to 12/1/2009-4/14/2010
# precipitation
# precRast2009 <- brick("/Volumes/localDrobo/generalData/climateData/current/daymet/daily/daymet_v3_prcp_2009_na.nc4")
# precRast2009 <- precRast2009[[grep("X2009.12.01", names(precRast2009)):grep("X2009.12.31", names(precRast2009))]]
# precRast2010 <- brick("/Volumes/localDrobo/generalData/climateData/current/daymet/daily/daymet_v3_prcp_2010_na.nc4")
# precRast2010 <- precRast2010[[grep("X2010.01.01", names(precRast2010)):grep("X2010.04.14", names(precRast2010))]]
# precRast <- stack(precRast2009, precRast2010)

# tmax
# tmaxRast2009 <- brick("/Volumes/localDrobo/generalData/climateData/current/daymet/daily/daymet_v3_tmax_2009_na.nc4")
# tmaxRast2009 <- tmaxRast2009[[grep("X2009.12.01", names(tmaxRast2009)):grep("X2009.12.31", names(tmaxRast2009))]]
# tmaxRast2010 <- brick("/Volumes/localDrobo/generalData/climateData/current/daymet/daily/daymet_v3_tmax_2010_na.nc4")
# tmaxRast2010 <- tmaxRast2010[[grep("X2010.01.01", names(tmaxRast2010)):grep("X2010.04.14", names(tmaxRast2010))]]
# tmaxRast <- stack(tmaxRast2009, tmaxRast2010)

# loop through each day and calculate whether precipitation fell as snow
# or rain (if tmax > 0 deg C in that location)
# for(r in 1:nlayers(precRast)){
#   print(r)
#   tmax <- tmaxRast[[r]]>0
#   if(r==1){prec <- precRast[[r]]*tmax} else {
#     prec <- prec+precRast[[r]]*tmax
#   }
# }

# reproject winter rain raster to match swe
# swe <- raster("/Volumes/localDrobo/Projects/activeProjects/misc/RoL/sweProportion.tif")
# precTrans <- projectRaster(from=prec, to=swe, crs = projection(swe))
# writeRaster(precTrans, "/Volumes/localDrobo/Projects/activeProjects/misc/RoL/winterRain.tif",
#             overwrite=T)


# Bromus predict function & fit model ------------------------------------------
# FUNCTION pred_brom_lam
# predict lambda based on snow cover, precip, and density
# input: f=fitted model, new= data frame for prediction
# output: new model coefficient
pred_brom_lam <- function(new=NULL,f=model_reduced2){
  if (is.null(new)){
    x1 <- runif(10,0,1)
    x2 <- runif(10,0,200)
    x3 <- rep(0,10)
    new<- data.frame(snow_cover=x1,
                     win_precip_rain=x2,
                     density=x3)}
  
  z <- predict(object=f, newdata=new)
  return(z)}

# load & prep data 
df <- read.csv("/Users/mfitzpatrick/code/RoL/Bromus_Clean_V3_Oct-08-19.csv", stringsAsFactors=FALSE)
vifstep(df[,9:18])
df <- df[,-ncol(df)]                 # remove empty final column
df_clean <- df[complete.cases(df),]  # remove 4 missing lambda

df_preds <- df_clean[,8:18]           # create dataframe of predictor variables
df_clean$r <- log(df_clean$Lambda)    # create response variable r from log(lambda)

# best variables are:
# snow.cover (6)
# winter precip snow (7)
# winter precip total (11)
# winter precip rain (8)
# growing season precip rain (3)
# density (1)
best_preds <- df_preds[,c(6,7,11,8,3,1)]

y <- df_clean$r
x_1 <- best_preds[,1]
x_2 <- best_preds[,2]
x_3 <- best_preds[,3]
x_4 <- best_preds[,4]
x_5 <- best_preds[,5]
x_6 <- best_preds[,6]

final_df <- data.frame(r=y,
                       snow_cover=x_1,
                       win_precip_snow=x_2,
                       win_precip_tot=x_3,
                       win_precip_rain=x_4,
                       grow_seas_precip_rain=x_5,
                       density=x_6)

# fit model
model_reduced2 <- lm(data=final_df,r~poly(snow_cover,2, raw=TRUE) +
                       poly(win_precip_rain,2, raw=TRUE) +
                       poly(density,1, raw=TRUE)) 


# Extract raster data, predict model and map output ----------------------------
snow_cover <- raster("/Volumes/Samsung_T5/Projects/misc/RoL/sweProportion.tif")
win_precip_rain <- raster("/Volumes/Samsung_T5/Projects/misc/RoL/winterRain.tif")

# deal with NA values in rasters
NAs <- unique(c(which(is.na(snow_cover[])), which(is.na(win_precip_rain[]))))
snow_cover[NAs] <- NA
win_precip_rain[NAs] <- NA

# stack rasters and extract values
envPreds <- stack(snow_cover, win_precip_rain)
newData <- data.frame(extract(envPreds, 1:ncell(snow_cover)), density=0, 
                      cell=1:ncell(snow_cover))
newData <- na.omit(newData)
names(newData)[1:2] <- c("snow_cover", "win_precip_rain")

# predict bromus model & assign values to raster
pred <- pred_brom_lam(new=newData[,1:3])
predRast <- snow_cover
predRast[newData$cell] <- pred
predRast[predRast[]<=0] <- NA

# plot bromus prediction -------------------------------------------------------
# North America, with political borders
NAstates <- shapefile("/Volumes/Samsung_T5/Projects/plantGenome/NA_wIslands_states.shp")
NAstates.simp <- gSimplify(NAstates, 0.01)

# lakes
NAlakes <- shapefile("/Volumes/Samsung_T5/Projects/plantGenome/NA_lakes.shp")
NAlakes <- NAlakes[12:16,]

tiff(filename="bromusMap.tif", width=12, height=9, units="in", res=300, 
     compression="lzw", type="cairo")
plot(NAstates.simp, col="gray50", bg=NA, border="grey60", 
     xlim=c(-130, -80), ylim=c(30, 50))
plot(NAlakes, col="white", border="gray50", add=T)
#plot(hillShade, col=grey(0:1000/1000), legend=FALSE, add=T)#maxpixels=143520052, add=T)
plot(predRast, col=rgb.tables(1000), add=T, legend=F)
plot(predRast, legend.only=T, col=rgb.tables(1000), legend.width=2,
     legend.args=list(side=3, text='r'),
     smallplot=c(0.10,0.15, 0.2,0.7))
box()
dev.off()


# MEDFLY -----------------------------------------------------------------------
# Matrix projection model for population growth
# with an environmental driver.
#
# Simplified implementation of model from 
# Gotelli & Ellison (2002,2006)
#
# Life table analysis for Med Fly reared
# in 4 warm:cool temperature regimes. We use
# the average temperature in these 4 treatments.
# "Age-based" model constructed by using median
# lx values for each stage. Therefore, stage persistence
# probabilities are not incorporated. lx values are
# converted to stage-specific survivorship as l(x+1)/l(x)
# 
# Reproduction is 0.5*average daily fecundity.
# 
# Data from Tables 3 and 4 in:
# Vargas, R.I., W.A. Walsh, D. Kanehisa, 
# J.D. Stark, and T. Nishida. 2000. 
# Annals of the Entomologial Society of America 93: 75-81.

#NJG
# 14 May 2019

source("/Users/mfitzpatrick/code/RoL/MatrixFunctions.R")
#-------------------------------------
library(popbio)
library(tidyverse)
library(raster)
library(parallel)
#-------------------------------------
# Global variables
# set up transition matrices (data from Vargas et al.)
fly_18 <- matrix(c(0,0.97,0,0,0,0,0.887,0,0,0,0,1.0,1.35,0,0,0),nrow=4)
fly_24 <- matrix(c(0,0.92,0,0,0,0,0.913,0,0,0,0,0.964,3.4,0,0,0),nrow=4)
fly_23 <- matrix(c(0,0.92,0,0,0,0,0.962,0,0,0,0,0.870,5.6,0,0,0),nrow=4)
fly_29 <- matrix(c(0,0.92,0,0,0,0,0.847,0,0,0,0,0.782,4.0,0,0,0),nrow=4)

# fold into a list
fly_dat <- list(fly_18,fly_24,fly_23,fly_29)

# set up experimental temperature vector
exp_tmp <- c(18.5,24,23.5,29.5)

# set up temperature input vector for generating continental map
con_temp <- 32

# for lam2_gen function set up vector of empirical reported little_r:
emp_lam <- c(0.051,0.120,0.137,0.092)
# emp_lam <- exp(emp_lam)
################################################################################
# download monthly worldclim temp, crop to North America, calc mean temp
# tmin <- getData(name="worldclim",
#                 var='tmin',
#                 res=2.5, 
#                 download=F,
#                 path=getwd())
# tmin <- crop(tmin, extent(-170, -40, 20, 80))
# 
# tmax <- getData(name="worldclim",
#                 var='tmax',
#                 res=2.5,
#                 download=F,
#                 path=getwd())
# tmax <- crop(tmax, extent(-170, -40, 20, 80))
# tmean <- (tmax/10+tmin/10)/2 # rasters are T*10, so div by 20 to get correct scale
# names(tmean) <- paste0("tmean", 1:12)
#writeRaster(tmean, "/Volumes/Samsung_T5/Projects/misc/RoL/tmean_12monthStack.tif")
tmean <- stack("/Volumes/Samsung_T5/Projects/misc/RoL/tmean_12monthStack.tif")
names(tmean) <- paste0("tmean", 1:12)
################################################################################

################################################################################
# run in parallel to speed calcs
lambda_rasts <- mclapply(1:12, function(x, tempRasts){
  rast <- tempRasts[[x]]
  keep <- !is.na(rast[]) # index NAs
  #-------------------------------------
  # original function based on transition matrices
  # lamb <- lam_gen(rast[keep]) # calc lamba for each temp
  # lamb[lamb<1] <-0 # clip non-increasing values to 0
  #-------------------------------------
  # use lambda2_gen, which is based on little_r, not transition matrices
  lamb <- lam2_gen(x=exp_tmp,
                   y=emp_lam,
                   z=rast[keep]) # calc little_r for each temp
  lamb[lamb<=0] <-NA # clip non-increasing values to 0
  #-------------------------------------
  rast[which(keep)] <- unlist(lamb) # assign lamba back to raster
  return(rast)}, tempRasts=tmean, mc.cores=10)
lambda_rasts <- do.call(stack, lambda_rasts)
names(lambda_rasts) <-paste("month", 1:12,sep=" ")
plot(lambda_rasts)
# calc geometric mean of lambda across the 12 months
# lambdaMap <- calc(lambda_rasts, fun=function(x){prod(exp(x))^(1/length(x))})
# lambdaMap <- calc(lambda_rasts, fun=function(x){mean(x)})
lambdaMap <- calc(lambda_rasts, fun=function(x){ifelse(mean(x)>0,mean(x),NA)})
plot(lambdaMap, main="mean r")


# plot medfly predictions ------------------------------------------------------
# North America, with political borders
NAstates <- shapefile("/Volumes/Samsung_T5/Projects/plantGenome/NA_wIslands_states.shp")
NAstates.simp <- gSimplify(NAstates, 0.01)

# 12 month mean
tiff(filename="medflyMean.tif", width=12, height=12, units="in", res=300, 
     compression="lzw", type="cairo")
plot(NAstates.simp, col="gray50", bg=NA, border="grey60", 
     xlim=c(-85, -75), ylim=c(24, 31))
plot(lambdaMap, col=rgb.tables(1000), add=T, legend=F)
plot(lambdaMap, legend.only=T, col=rgb.tables(1000), legend.width=2,
     legend.args=list(side=3, text='lambda'),
     smallplot=c(0.83,0.88, 0.5,0.9))
box()
dev.off()

# individual month panels (combined later in Photoshop)
for(m in 1:12){
  print(array(month.name)[m])
  fName <- paste0(array(month.name)[m], "_medfly.tif")
  tiff(filename=fName, width=12, height=8, units="in", res=300, 
       compression="lzw", type="cairo")
  plot(lambda_rasts[[m]], col="white", legend=F,
       xlim=c(-170, -50), ylim=c(19, 70), axes=F, box=F,
       main=array(month.name)[m], cex.main=4)
  plot(NAstates.simp, col="gray50", bg=NA, border="grey60", 
       add=T, 
       xlim=c(-180, -50), ylim=c(20, 75))
  plot(lambda_rasts[[m]], col=rgb.tables(1000), add=T, legend=F)
  if(m==1){
  plot(lambda_rasts[[m]], legend.only=T, col=rgb.tables(1000), legend.width=2,
       legend.args=list(side=3, text='lambda', cex=3, font=2),
       smallplot=c(0.2,0.24, 0.16,0.59),
       axis.args=list(cex.axis=2.5))
  }
  box()
  dev.off()
}

