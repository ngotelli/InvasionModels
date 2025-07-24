# Load Libraries ---------------------------------------------------------------
library(ggplot2)
library(sf) # for spatial data
library(geodata) # admin boundaries
library(spatialEco) # dissolve polygons
library(raster)
library(terra)
library(colorRamps)
library(geodata)
library(dplyr)
library(popbio)
library(tidyverse)
library(parallel)

# MEDFLY (Ceratitis capitata) --------------------------------------------------
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

#### source matrix projection model functions from file ####
try(source("/Users/mfitzpatrick/code/InvasionModels/Scripts/MatrixFunctions.R"))
try(source("Scripts/MatrixFunctions.R"))

#### Global variables ####
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

#### download mean monthly temperature ####
tmean <- worldclim_global(var = "tavg", res = 2.5, path = getwd())
# crop to Florida
tmean <- crop(tmean, extent(-85, -75, 22, 31)) 
# crop to North America
# tmean <- crop(tmean, extent(-170, -50, 20, 75))
names(tmean) <- paste0("tmean", 1:12)

#### predict population growth from temperature ####
# run in parallel to speed calcs
tmean.x <- as(tmean, "Raster") # convert from spatRaster
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
                   z=rast[keep])
  lamb[lamb<=0] <- NA # clip non-increasing values to 0
  #-------------------------------------
  rast[which(keep)] <- unlist(lamb) # assign lamba back to raster
  return(rast)}, tempRasts=tmean.x, mc.cores=6)
lambda_rasts <- do.call(stack, lambda_rasts)
names(lambda_rasts) <-paste("month", 1:12,sep=" ")
plot(lambda_rasts)
# calc geometric mean of lambda across the 12 months
# lambdaMap <- calc(lambda_rasts, fun=function(x){prod(exp(x))^(1/length(x))})
# lambdaMap <- calc(lambda_rasts, fun=function(x){mean(x)})
lambdaMap <- calc(lambda_rasts, fun=function(x){ifelse(mean(x)>0,mean(x),NA)})

# convert lambda to little r
medfly_r_raster <- log(lambdaMap) # convert to little r
names(medfly_r_raster) <- "status"
plot(medfly_r_raster, main="mean r")

# save the raster
writeRaster(medfly_r_raster, "/Users/mfitzpatrick/Desktop/invasionModels/medfly_little_r_Pred.tif", 
            overwrite=T)

#### load GBIF data for medfly ####
# data DOI: https://doi.org/10.15468/dl.r4ttzx
medflyGBIF <- read.table("/Users/mfitzpatrick/Desktop/invasionModels/medFly_GBIF_07232025.csv", 
                         header=T, sep="\t", stringsAsFactors=F) %>%
  # select records with coordinatesUncertaintyInMeters <= 5000
  # to match climate data
  dplyr::filter(coordinateUncertaintyInMeters <= 5000) %>%
  # select columns of interest
  dplyr::select(lon=decimalLongitude, lat=decimalLatitude) %>%
  # remove rows with NA values
  na.omit() %>%
  # convert to sf object
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) # Assuming WGS84 projection

# extract medfly r values at GBIF points
medfly_r_values <- data.frame(terra::extract(rast(medfly_r_raster), medflyGBIF, cells=T, xy=T, ID=F))
medfly_r_values <- unique(medfly_r_values) # remove duplicate rows
medfly_r_values <- medfly_r_values[-1,] # remove first row (all NAs)
names(medfly_r_values)[c(3,4)] <- c("lon", "lat") # rename the columns
# convert to binary status
medfly_r_values$status <- ifelse(is.na(medfly_r_values$status), "negative", "positive") 
medfly_r_values$status <- factor(medfly_r_values$status, levels = c("negative", "positive"))
# convert to sf object
medfly_r_values <- sf::st_as_sf(medfly_r_values, coords = c("lon", "lat"), 
                                crs = 4326)

#### create medfly maps (Figure 3 in mansucript) ####
# boundary of USA + states from geodata package
usa <- geodata::gadm("USA", level = 1, path=getwd())
usa <- usa[which(usa$NAME_1 %in% c("Florida", "Georgia")),] 
# remove Alaska and Hawaii
#usaStates <- usaStates[usaStates$NAME_1 != "Hawaii", ]
#usaStates <- usaStates[usaStates$NAME_1 != "Alaska", ]
# convert to sf object
usa <- sf::st_as_sf(usa)
usa <- usa[,c("GID_0", "COUNTRY")]
# dissolve the states into a single polygon
#usa <- spatialEco::sf_dissolve(usaStates, "COUNTRY")

# boundaries of Caribbean islands
cuba <- geodata::gadm("Cuba", level = 0, path=getwd())
cuba <- sf::st_as_sf(cuba)
dominican_republic <- geodata::gadm("Dominican Republic", level = 0, path=getwd())
dominican_republic <- sf::st_as_sf(dominican_republic)
puerto_rico <- geodata::gadm("Puerto Rico", level = 0, path=getwd())
puerto_rico <- sf::st_as_sf(puerto_rico)
jamaica <- geodata::gadm("Jamaica", level = 0, path=getwd())
jamaica <- sf::st_as_sf(jamaica)
trinidad_and_tobago <- geodata::gadm("Trinidad and Tobago", level = 0, path=getwd())
trinidad_and_tobago <- sf::st_as_sf(trinidad_and_tobago)
bahamas <- geodata::gadm("Bahamas", level = 0, path=getwd())
bahamas <- sf::st_as_sf(bahamas)
haiti <- geodata::gadm("Haiti", level = 0, path=getwd())
haiti <- sf::st_as_sf(haiti)

# combine all Caribbean islands into a single sf object
caribbean_islands <- rbind(cuba, haiti, dominican_republic, puerto_rico, jamaica, trinidad_and_tobago, bahamas)

# combine canada and usa into a single sf object
polys <- rbind(usa, caribbean_islands)

# simplify the geometry for plotting
polys <- sf::st_simplify(polys, dTolerance = 1000)

#### plot medfly prediction map (Figure 3s) ####
# raster prediction plot
#predPlotRaster <- medfly_r
#predPlotRaster[predPlotRaster[]<=0] <- NA

# --- Step 1: Convert your raster to a data frame ---
# Assuming predPlotRaster is a SpatRaster object from the 'terra' package
# If it's a RasterLayer from 'raster', use raster::as.data.frame()
predPlot_df <- as.data.frame(medfly_r_raster, xy = TRUE)

my_rgb_colors <- rgb.tables(1000)

# --- Step 2: Plot the raster and sf object ---
ggplot() +
  # Add the raster using geom_raster (more efficient for rasters)
  # Ensure 'prediction_value' is the correct column name from your data frame conversion
  geom_tile(data = predPlot_df, aes(x = x, y = y, fill = status)) +
  
  # Add the sf object as an overlay
  geom_sf(data = polys,
          fill = NA,
          color = "black",
          linewidth = 0.5) + # No fill, black border
  
  annotate("text",
           x = -84.5, # X-coordinate (longitude) for the text position
           y = 31.2,  # Y-coordinate (latitude) for the text position
           label = "a", # The actual text you want to display
           size = 16, # Font size of the text
           color = "black", # Color of the text
           fontface = "bold" # Optional: Make the text bold
  ) +
  
  # 4. Set the coordinate system
  coord_sf(
    xlim = c(-85, -75),
    ylim = c(24, 31.5),
    expand = FALSE # Set to FALSE to remove the small padding around the limits
  ) +
  
  # Customize theme for white background and no grid lines
  theme_minimal() + # Start with a minimal theme
  theme(
    panel.background = element_rect(fill = "white", color = NA), # White background, no border
    plot.background = element_rect(fill = "white", color = NA),   # White plot area background
    panel.grid = element_blank(), # No grid lines
    axis.title = element_blank(), # Optional: Remove axis titles
    axis.text = element_blank(), # Optional: Remove axis text for a cleaner map
    # Optional: Adjust legend position or remove if not needed
    legend.position.inside = c(0, 0), # Or "none" if you don't want a legend
    legend.justification = c("left", "top"),
    legend.title = element_text(size = 24, face = "bold"), 
    legend.text = element_text(size = 12)   
  ) +
  # Add a fill scale for your raster values (adjust as needed for your data type)
  scale_fill_gradientn(colors = my_rgb_colors,
                       name = "r",
                       breaks = seq(0.05, 0.15, by=0.01)) # Customize legend title

ggsave("/Users/mfitzpatrick/code/InvasionModels/Graphics/revision/forManuscript/FIGURE3a_medfly.png", 
       width = 10, 
       height = 8, 
       dpi = 300)

#### plot medfly occurrence map (Figure 3b) ####
ggplot() +
  # 1. Plot the polygon first (bottom layer)
  geom_sf(
    data = polys,
    fill = NA,    # Fill the polygon
    color = "black",       # Border color of the polygon
    linewidth = 0.5        # Border thickness
  ) +
  
  # 2. Plot the points on top of the polygon
  geom_sf(
    data = medfly_r_values,
    aes(fill = status), # Map the binary variable to color
    shape = 21,                 # Use shape 21 for filled circles with border
    color="black",
    size = 3,                       # Size of the points
    alpha = 1                     # Transparency of the points
  ) +
  
  # 3. Add text annotation
  annotate("text",
           x = -84.5, # X-coordinate (longitude) for the text position
           y = 31.2,  # Y-coordinate (latitude) for the text position
           label = "b", # The actual text you want to display
           size = 16, # Font size of the text
           color = "black", # Color of the text
           fontface = "bold" # Optional: Make the text bold
  ) +
  
  # 4. Set the coordinate system
  coord_sf(
    xlim = c(-85, -75),
    ylim = c(24, 31.5),
    expand = FALSE # Set to FALSE to remove the small padding around the limits
  ) +
  
  # Customize theme for white background and no grid lines
  theme_minimal() + # Start with a minimal theme
  theme(
    panel.background = element_rect(fill = "white", color = NA), # White background, no border
    plot.background = element_rect(fill = "white", color = NA),   # White plot area background
    panel.grid = element_blank(), # No grid lines
    axis.title = element_blank(), # Optional: Remove axis titles
    axis.text = element_blank(), # Optional: Remove axis text for a cleaner map
    # Optional: Adjust legend position or remove if not needed
    legend.position.inside = c(0, 0), # Or "none" if you don't want a legend
    legend.justification = c("left", "top"),
    legend.title = element_text(size = 24, face = "bold"),
    legend.text = element_text(size = 12),
    # Adjust size of legend keys
    legend.key.size = unit(0.5, "cm") # You can adjust this value (e.g., 0.5, 1, 2 cm)
  ) +
  # This section is for making legend points larger
  guides(fill = guide_legend(override.aes = list(size = 3))) +  # Adjust 'size' as needed for legend points
  # Reverse the color scheme for categorical 'status'
  scale_fill_manual(
    values = c("positive" = "#F8766D", "negative" = "#00BFC4")
  )

# edited in Photoshop to move legend
ggsave("/Users/mfitzpatrick/code/InvasionModels/Graphics/revision/forManuscript/FIGURE3b_medfly.png", 
       width = 10, 
       height = 8, 
       dpi = 300)



