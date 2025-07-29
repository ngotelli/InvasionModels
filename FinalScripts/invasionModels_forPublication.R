# Load Libraries ---------------------------------------------------------------
library(ggplot2)
library(sf) # for spatial data
library(geodata) # admin boundaries
library(spatialEco) # dissolve polygons
library(raster)
library(terra)
library(colorRamps)
library(dplyr)
library(popbio)
library(tidyverse) # Includes ggplot2, dplyr, and others
library(parallel)
library(gtools)
library(viridisLite)
library(stats)
library(usdm) # for variance inflation factor


# CHUNK 1: MEDFLY (Ceratitis capitata) --------------------------------------------------
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

#### Matrix projection model  ####
# generates lambda values 
# input: x=vector of experimental temperatures
#        y=vector of measured lambdas
#        z=vector of temperatures for prediction
lam2_gen <- function(x,y,z){
  . <- est_trans(x,y)
  . <- pred_trans(z,.)
  # .[.<0] <- 0  # rescale any values with lambda <0 to 0
  # .[.<0] <- 0  # rescale any values with lambda <1 to 0
  names(.) <- paste0("V",seq_along(z))
  . <- as.list(.)
  return(.)
}

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
  
  # original function based on transition matrices
  # lamb <- lam_gen(rast[keep]) # calc lamba for each temp
  # lamb[lamb<1] <-0 # clip non-increasing values to 0
  
  # use lambda2_gen, which is based on little_r, not transition matrices
  lamb <- lam2_gen(x=exp_tmp,
                   y=emp_lam,
                   z=rast[keep])
  lamb[lamb<=0] <- NA # clip non-increasing values to 0
  
  rast[which(keep)] <- unlist(lamb) # assign lamba back to raster
  return(rast)}, tempRasts=tmean.x, mc.cores=6)
lambda_rasts <- do.call(stack, lambda_rasts)
names(lambda_rasts) <-paste("month", 1:12,sep=" ")
plot(lambda_rasts)
# calc mean of lambda across the 12 months
lambdaMap <- calc(lambda_rasts, fun=function(x){ifelse(mean(x)>0,mean(x),NA)})

# convert lambda to little r???
medfly_r_raster <- lambdaMap#log(lambdaMap) # convert to little r???
names(medfly_r_raster) <- "status"
plot(medfly_r_raster, main="mean r")

# save the raster
writeRaster(medfly_r_raster, "/Users/mfitzpatrick/Desktop/invasionModels/medFly_little_r_Pred.tif", 
            overwrite=T)

#### load GBIF data for medfly ####
# data DOI: https://doi.org/10.15468/dl.r4ttzx
# downloaded from GBIF on 7/23/2025
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

#### create medfly maps (Figure 3 in manuscript) ####
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
caribbean_islands <- rbind(cuba, haiti, dominican_republic, puerto_rico, 
                           jamaica, trinidad_and_tobago, bahamas)

# combine canada and usa into a single sf object
polys <- rbind(usa, caribbean_islands)

# simplify the geometry for plotting
polys <- sf::st_simplify(polys, dTolerance = 1000)

#### plot medfly prediction map (Figure 3a) ####
# raster prediction plot
predPlotRaster <- medfly_r_raster
predPlotRaster[predPlotRaster[]<=0] <- NA

# --- Step 1: Convert your raster to a data frame ---
# Assuming predPlotRaster is a SpatRaster object from the 'terra' package
# If it's a RasterLayer from 'raster', use raster::as.data.frame()
predPlot_df <- as.data.frame(predPlotRaster, xy = TRUE)

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
                       na.value = "white",
                       breaks = seq(0.05, 0.15, by=0.01)) # Customize legend title

# legend is moved is photoshop
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


# CHUNK 2: HOUSE SPARROW (Passer domesticus) --------------------------------------------
#### spline functions for curve fitting ####
# to Jesse Krause's data on house sparrow growth. This model creates a function
# based on physiological estimates of minimum, maximum, and optimal temperatures
# for performance from:

# Kendeigh, S.C 1976. Latitudinal trends in the metabolic adjustmets of the 
# house sparrow. Ecology 57: 509-519.

# The maximum r is set at 0.50, slightly higher than the largest value 
# I calculated from the spreadsheets that Jesse sent. However, for this 
# particular model, the actual value is arbitrary and does not affect the 
# appearance of the map, just the scaling. The optimum temperature does affect 
# the appearance of the contours, but the most important issue is the max and 
# min values because these determine the areas where dN/dt > 0.

# 02 July 2021 - updated spline function to include 
# new limits after discussion with Jesse

# create spline function
# x = min, optimal, and max temperature (C)
# y = estimated r at each temperature

# this function updated with new values from Jesse 2 July 2021
r_function <- splinefun(x=c(8,17,24),
                        y=c(0,0.6,0),
                        method="monoH.FC")

# create a vector of temperatures
Temperature <- seq(0,30)

# get estimated growth rates from function
r <- r_function(Temperature)

# plot results
plot(x=Temperature,y=r)

#### download mean monthly temperature ####
tmean <- worldclim_global(var = "tavg", res = 2.5, path = getwd())
# crop to North America
tmean <- crop(tmean, extent(-200,-55,10,80))
names(tmean) <- paste0("tmean", 1:12)
# mean temp during breeding season (march-june)
tmean_bs <- mean(tmean[[3]], tmean[[4]], tmean[[5]], tmean[[6]])
names(tmean_bs) <- "tmean_bs"

# get NA IDs, etc for calcs and mapping
mask <- tmean_bs
mask <- mask>-1000
nas <- which(is.na(mask[]))
getEm <- which(mask[]==1)

#### apply r function to tmean ####
tmean_bs.vect <- tmean_bs[getEm]
growthPred <- r_function(tmean_bs.vect$tmean_bs)

#### create and save raster prediction ####
housesparrow_r_raster <- tmean_bs
housesparrow_r_raster[getEm] <- growthPred
pos <- housesparrow_r_raster
neg <- pos<0
neg[neg[]==0] <- NA
pos <- pos>0
housesparrow_r_raster <- housesparrow_r_raster*pos
housesparrow_r_raster[housesparrow_r_raster[]==0] <- NA
names(housesparrow_r_raster) <- "status"
writeRaster(housesparrow_r_raster, 
            filename="/Users/mfitzpatrick/Desktop/invasionModels/houseSparrow_little_r_Pred.tif",
            overwrite=TRUE)

#### load eBird EBD data for house sparrow ####
houseSparrowEBD <- read.csv("/Users/mfitzpatrick/Desktop/invasionModels/houseSparrow_eBirdEBD_03282025.csv")
names(houseSparrowEBD) <- c("lon", "lat")
houseSparrowEBD <- data.frame(houseSparrowEBD) %>%
  # convert to sf object
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

#### extract house sparrow r values at eBird points ####
housesparrow_r_values <- data.frame(terra::extract(housesparrow_r_raster, 
                                                   houseSparrowEBD, 
                                                   cells=T, 
                                                   xy=T, ID=F))
housesparrow_r_values <- unique(housesparrow_r_values) # remove duplicate rows
names(housesparrow_r_values)[c(3,4)] <- c("lon", "lat") # rename the columns
# convert to binary status
housesparrow_r_values$status <- ifelse(is.na(housesparrow_r_values$status), "negative", "positive") 
housesparrow_r_values$status <- factor(housesparrow_r_values$status, levels = c("negative", "positive"))
# convert to sf object
housesparrow_r_values <- sf::st_as_sf(housesparrow_r_values, coords = c("lon", "lat"), 
                                crs = 4326)

#### Create house sparrow maps (Figure 5 in manuscript) ####
# boundary of USA + states from geodata package
usa <- geodata::gadm("USA", level = 0, path=getwd())
usa <- sf::st_as_sf(usa)

# boundary of Canada
canada <- geodata::gadm("Canada", level = 0, path=getwd())
canada <- sf::st_as_sf(canada)

# boundary of central american countries
mexico <- geodata::gadm("Mexico", level = 0, path=getwd())
mexico <- sf::st_as_sf(mexico)
guatemala <- geodata::gadm("Guatemala", level = 0, path=getwd())
guatemala <- sf::st_as_sf(guatemala)
honduras <- geodata::gadm("Honduras", level = 0, path=getwd())
honduras <- sf::st_as_sf(honduras)
el_salvador <- geodata::gadm("El Salvador", level = 0, path=getwd())
el_salvador <- sf::st_as_sf(el_salvador)
nicaragua <- geodata::gadm("Nicaragua", level = 0, path=getwd())
nicaragua <- sf::st_as_sf(nicaragua)
costa_rica <- geodata::gadm("Costa Rica", level = 0, path=getwd())
costa_rica <- sf::st_as_sf(costa_rica)
panama <- geodata::gadm("Panama", level = 0, path=getwd())
panama <- sf::st_as_sf(panama)
# combine all central american countries into a single sf object 
central_america <- rbind(mexico, guatemala, honduras, el_salvador, nicaragua, 
                         costa_rica, panama)

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
caribbean_islands <- rbind(cuba, haiti, dominican_republic, puerto_rico, 
                           jamaica, trinidad_and_tobago, bahamas)

# combine canada and usa into a single sf object
polys <- rbind(usa, canada, central_america, caribbean_islands)

# simplify the geometry
polys <- sf::st_simplify(polys, dTolerance = 1000)

#### plot house sparrow prediction map (Figure 5a) ####
# raster prediction plot
predPlotRaster <- housesparrow_r_raster
predPlotRaster[predPlotRaster[]<=0] <- NA

# --- Step 1: Convert your raster to a data frame ---
# Assuming predPlotRaster is a SpatRaster object from the 'terra' package
# If it's a RasterLayer from 'raster', use raster::as.data.frame()
predPlot_df <- as.data.frame(predPlotRaster, xy = TRUE)

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
          linewidth = 0.25) + # No fill, black border
  
  annotate("text",
           x = -125, # X-coordinate (longitude) for the text position
           y = 58,  # Y-coordinate (latitude) for the text position
           label = "a", # The actual text you want to display
           size = 16, # Font size of the text
           color = "black", # Color of the text
           fontface = "bold" # Optional: Make the text bold
  ) +
  
  # This ensures correct aspect ratio and alignment.
  coord_sf(
    xlim = c(-130, -40),
    ylim = c(0, 60),
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
    legend.text = element_text(size = 10)   
  ) +
  # Add a fill scale for your raster values (adjust as needed for your data type)
  scale_fill_gradientn(colors = my_rgb_colors,
                       name = "r") # Customize legend title

# remove legend & remove some areas in the Caribbean using Photoshop
ggsave("/Users/mfitzpatrick/code/InvasionModels/Graphics/revision/forManuscript/FIGURE5a_houseSparrow.png", width = 10, height = 8, dpi = 300)

#### plot house sparrow occurrence map (Figure 5b) ####
# create data frame for plotting with geometry
housesparrow_r_values <- housesparrow_r_values %>%
  mutate(
    x = st_coordinates(.)[, "X"], # Extract X coordinate
    y = st_coordinates(.)[, "Y"]  # Extract Y coordinate
  ) %>%
  st_drop_geometry()

# plot
ggplot() +
  # Add the raster using geom_tile
  geom_tile(data=housesparrow_r_values, aes(x = x, y = y, fill = status)) +
  
  # Add the sf object as an overlay
  geom_sf(data = polys,
          fill = NA,
          color = "black",
          linewidth = 0.25) + # No fill, black border
  
  annotate("text",
           x = -125, # X-coordinate (longitude) for the text position
           y = 57,  # Y-coordinate (latitude) for the text position
           label = "b", # The actual text you want to display
           size = 16, # Font size of the text
           color = "black", # Color of the text
           fontface = "bold" # Optional: Make the text bold
  ) +
  
  # This ensures correct aspect ratio and alignment.
  coord_sf(
    xlim = c(-130, -40),
    ylim = c(0, 60),
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
    legend.text = element_text(size = 10)   
  ) + 
  # Reverse the color scheme for categorical 'status'
  scale_fill_manual(
    values = c("positive" = "#F8766D", "negative" = "#00BFC4")
  )

# edited in Photoshop to move legend
ggsave("/Users/mfitzpatrick/code/InvasionModels/Graphics/revision/forManuscript/FIGURE5b_houseSparrow.png", width = 10, height = 8, dpi = 300)


# CHUNK 3: CHEAT GRASS (Bromus tectorum) ---------------------------------------
#### Prepare climate covariates from cheatGrass ####
##### create snow water equivalent raster (% days above 0.15 cm) #####
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

##### Calculate winter rain #####
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

#### Bromus predict function & fit model ####
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

#### Load & prep cheatgrass field data #### 
df <- read.csv("/Users/mfitzpatrick/Desktop/invasionModels/Bromus_Clean_V3_Oct-08-19.csv", 
               stringsAsFactors=FALSE)
df <- df[,-ncol(df)]                 # remove "notes" column
df_clean <- df[complete.cases(df),]  # remove 4 missing lambda
# create data frame of predictor variables
df_preds <- df_clean[,8:18]           
# create response variable r from log(lambda)
df_clean$r <- log(df_clean$Lambda)

# best variables are:
# snow.cover (6), winter precip snow (7), winter precip total (11),
# winter precip rain (8), growing season precip rain (3), starting density (1)
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

#### Fit model ####
model_reduced2 <- lm(data=final_df,r~poly(snow_cover, 2, raw=TRUE) +
                       poly(win_precip_rain, 2, raw=TRUE) +
                       poly(density, 1, raw=TRUE)) 

#### Extract raster data, predict model and map output ####
snow_cover <- raster("/Users/mfitzpatrick/Desktop/invasionModels/sweProportion.tif")
win_precip_rain <- raster("/Users/mfitzpatrick/Desktop/invasionModels/winterRain.tif")

# deal with NA values in rasters
NAs <- unique(c(which(is.na(snow_cover[])), which(is.na(win_precip_rain[]))))
snow_cover[NAs] <- NA
win_precip_rain[NAs] <- NA

# stack rasters and extract values
envPreds <- stack(snow_cover, win_precip_rain)
newData <- data.frame(raster::extract(envPreds, 1:ncell(snow_cover)), density=0, 
                      cell=1:ncell(snow_cover))
newData <- na.omit(newData)
names(newData)[1:2] <- c("snow_cover", "win_precip_rain")

##### predict bromus model & assign values to raster #####
pred <- pred_brom_lam(new=newData[,1:3])
cheatgrass_r_raster <- snow_cover
cheatgrass_r_raster[newData$cell] <- pred
cheatgrass_r_raster[cheatgrass_r_raster[]<=0] <- 0#NA
names(cheatgrass_r_raster) <- "status"
writeRaster(cheatgrass_r_raster, 
            filename="/Users/mfitzpatrick/Desktop/invasionModels/cheatGrass_little_r_Pred.tif",
            overwrite=TRUE)

#### Load veg plot data for cheat grass from Larson & Tuor 2021  ####
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
cheatGrass_occ <- read.csv("/Users/mfitzpatrick/Desktop/invasionModels/cheatGrass_larsonTuor_2021.csv")

cheatGrass_occ <- cheatGrass_occ %>%
  # select columns of interest
  select(brte_cov, longitude_30m, latitude_30m) %>%
  # remove plots with zero cover
  #filter(brte_cov > 0) %>%
  # convert to sf object
  st_as_sf(coords = c("longitude_30m", "latitude_30m"), crs = 4326)

# extract cheatgrass r values at veg plot points
cheatgrass_r_values <- data.frame(terra::extract(rast(cheatgrass_r_raster), 
                                                 cheatGrass_occ, 
                                                 cells=T, xy=T, ID=F))
cheatgrass_r_values <- unique(cheatgrass_r_values) # remove duplicate rows
# remove rows with NA values
cheatgrass_r_values <- na.omit(cheatgrass_r_values)
names(cheatgrass_r_values)[c(3,4)] <- c("lon", "lat") # rename the columns
cheatgrass_r_values$r <- cheatgrass_r_values$status # rename r column
# convert to binary status
cheatgrass_r_values$status <- ifelse(cheatgrass_r_values$status<=0, "negative", "positive") 
cheatgrass_r_values$status <- factor(cheatgrass_r_values$status, levels = c("negative", "positive"))
# convert to sf object
cheatgrass_r_values <- sf::st_as_sf(cheatgrass_r_values, coords = c("lon", "lat"), 
                                crs = 4326)

#### Create cheat grass maps (Figure 7 in manuscript) ####
# boundary of USA + states from geodata package
usaStates <- geodata::gadm("USA", level = 1, path=getwd())
# remove Alaska and Hawaii
usaStates <- usaStates[usaStates$NAME_1 != "Hawaii", ]
usaStates <- usaStates[usaStates$NAME_1 != "Alaska", ]
# convert to sf object
usaStates <- sf::st_as_sf(usaStates)
# simplify the geometry
usaStates <- sf::st_simplify(usaStates, dTolerance = 100)

# raster prediction plot
predPlotRaster <- cheatgrass_r_raster
predPlotRaster[predPlotRaster[]<=0] <- NA

# --- Step 1: Convert your raster to a data frame ---
# Assuming predPlotRaster is a SpatRaster object from the 'terra' package
# If it's a RasterLayer from 'raster', use raster::as.data.frame()
predPlot_df <- as.data.frame(predPlotRaster, xy = TRUE)

my_rgb_colors <- rgb.tables(1000)

#### Plot cheatgrass raster map (Figure 7a) ####
ggplot() +
  geom_tile(data = predPlot_df, aes(x = x, y = y, fill = status)) +
  
  # Add the sf object as an overlay
  geom_sf(data = usaStates,
          fill = NA,
          color = "black",
          linewidth = 0.25) + # No fill, black border
  
  annotate("text",
           x = -122, # X-coordinate (longitude) for the text position
           y = 50.5,  # Y-coordinate (latitude) for the text position
           label = "a", # text to display
           size = 16, # Font size of the text
           color = "black", # Color of the text
           fontface = "bold" # Make the text bold
  ) +
  
  # Ensure correct aspect ratio and alignment.
  coord_sf(
    xlim = c(-130, -80),
    ylim = c(30, 53),
    expand = FALSE # Set to FALSE to remove the small padding around the limits
  ) +
  
  # White background and no grid lines
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), # White background, no border
    plot.background = element_rect(fill = "white", color = NA),   # White plot area background
    panel.grid = element_blank(), # No grid lines
    axis.title = element_blank(), # Remove axis titles
    axis.text = element_blank(), # Remove axis text for a cleaner map
    # Adjust legend position or remove if not needed
    legend.position.inside = c(0, 0),
    legend.justification = c("left", "top"),
    legend.title = element_text(size = 24, face = "bold"), 
    legend.text = element_text(size = 10)   
  ) +
  # Add a fill scale 
  scale_fill_gradientn(colors = my_rgb_colors,
                       name = "r",
                       na.value = "white", # Make NA values transparent
                       breaks = seq(0.2, 1.0, by=0.2)) # Legend title

ggsave("/Users/mfitzpatrick/code/InvasionModels/Graphics/revision/forManuscript/FIGURE7a_bromus.png", 
       width = 10, 
       height = 8, 
       dpi = 300)

#### Plot cheatgrass occurrence map (Figure 7b) ####
ggplot() +
  # 1. Plot the polygon first (bottom layer)
  geom_sf(
    data = usaStates,
    fill = NA,    # Fill the polygon
    color = "black",       # Border color of the polygon
    linewidth = 0.25        # Border thickness
  ) +
  
  # 2. Plot the points on top of the polygon
  geom_sf(
    data = cheatgrass_r_values,
    aes(fill = status), # Map the factor variable to FILL for shape 21
    shape = 21,                 # Use shape 21 for filled circles with border
    color = "black",            # Set the border color of the points to black
    size = 1.5,                # Size of the points
    alpha = 1                   # Transparency of the points
  ) +
  
  annotate("text",
           x = -122, # X-coordinate (longitude) for the text position
           y = 50.3,  # Y-coordinate (latitude) for the text position
           label = "b", # The actual text you want to display
           size = 16, # Font size of the text
           color = "black", # Color of the text
           fontface = "bold" # Optional: Make the text bold
  ) +
  
  # 4. Set the coordinate system
  coord_sf(
    xlim = c(-125, -100),
    ylim = c(25, 52),
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

ggsave("/Users/mfitzpatrick/code/InvasionModels/Graphics/revision/forManuscript/FIGURE7b_bromus.png", 
       width = 10, 
       height = 8, 
       dpi = 300)


