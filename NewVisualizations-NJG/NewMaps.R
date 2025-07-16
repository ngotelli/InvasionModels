# New visualizations of data layers
# created by Matt for ebird records of house sparrows and veg survey data for cheatgrass. 
# GBIF data for medfly (https://doi.org/10.15468/dl.fk2zuc)
# Graphs show demographic growth rate as a function of spatial position.

# 14 April 2025
# NJG

library(ggplot2)
library(sf) # for spatial data
library(geodata) # admin boundaries
library(spatialEco) # dissolve polygons
library(raster)
library(terra)
library(colorRamps)


# house sparrow -----------------------------------------------------------
# This code ran outside of the repo to create the rds file:

# z<- read.table(file="HouseSparrowGrowthPredictions_at_eBird_points.csv",
#                header=TRUE,sep=",")
# saveRDS(z,"HouseSparrow.rds")

# house sparrow plots
try(z <- readRDS("/Users/mfitzpatrick/code/InvasionModels/Output/HouseSparrowGrowthPredictions_at_eBird_points.rds"))
try(z <- readRDS("Output/HouseSparrowGrowthPredictions_at_eBird_points.rds"))

# boundary of USA + states from geodata package
usa <- geodata::gadm("USA", level = 0, path=getwd())
# remove Alaska and Hawaii
#usaStates <- usaStates[usaStates$NAME_1 != "Hawaii", ]
#usaStates <- usaStates[usaStates$NAME_1 != "Alaska", ]
# convert to sf object
usa <- sf::st_as_sf(usa)
# dissolve the states into a single polygon
#usa <- spatialEco::sf_dissolve(usaStates, "COUNTRY")

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
central_america <- rbind(mexico, guatemala, honduras, el_salvador, nicaragua, costa_rica, panama)
# dissolve into a single polygon
#central_america$continent <- "centralAmerica"
#central_america <- spatialEco::sf_dissolve(central_america, "continent")

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
polys <- rbind(usa, canada, central_america, caribbean_islands)

# simplify the geometry
polys <- sf::st_simplify(polys, dTolerance = 1000)

z$status <- "positive"
z$status[z$layer<=0] <- "negative"

# plot
ggplot() +
  # Add the raster using geom_tile
  geom_tile(data = z, aes(x = x, y = y, fill = status)) +
  
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
  )

# --- Exporting High-Resolution (Optional) ---
ggsave("/Users/mfitzpatrick/code/InvasionModels/Graphics/revision/forManuscript/FIGURE5b_houseSparrow.png", width = 10, height = 8, dpi = 300)

# raster prediction plot
predPlotRaster <- rast("/Users/mfitzpatrick/Desktop/houseSparrow_growthPred.tif")

# --- Step 1: Convert your raster to a data frame ---
# Assuming predPlotRaster is a SpatRaster object from the 'terra' package
# If it's a RasterLayer from 'raster', use raster::as.data.frame()
predPlot_df <- as.data.frame(predPlotRaster, xy = TRUE)

my_rgb_colors <- rgb.tables(1000)

# --- Step 2: Plot the raster and sf object ---
ggplot() +
  # Add the raster using geom_raster (more efficient for rasters)
  # Ensure 'prediction_value' is the correct column name from your data frame conversion
  geom_tile(data = predPlot_df, aes(x = x, y = y, fill = houseSparrow_growthPred)) +
  
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

ggsave("/Users/mfitzpatrick/code/InvasionModels/Graphics/revision/forManuscript/FIGURE5a_houseSparrow.png", width = 10, height = 8, dpi = 300)



# cheatgrass -------------------------------------------------------------------
# Run outside of repo to create data object
 # z<- read.table(file="../../UTF-8BromusStackVegPlot.csv",
 #                header=TRUE,sep=",")
 # saveRDS(z,"DataObjects/BromusStackVegPlot.rds")
try(z <- readRDS("/Users/mfitzpatrick/code/InvasionModels/DataObjects/BromusStackVegPlot.rds"))
try(z <- readRDS("DataObjects/BromusStackVegPlot.rds"))
z <- na.omit(z)
z$status <- "positive"
z$status[z$bromus_lambdaPred.v2<=1.0] <- "negative"
 # z <-z[z$bromus_lambdaPred.v2>0,]

# convert z to an sf object
z <- sf::st_as_sf(z, coords = c("x", "y"), crs = 4326) # Assuming WGS84 projection

# boundary of USA + states from geodata package
usaStates <- geodata::gadm("USA", level = 1, path=getwd())
# remove Alaska and Hawaii
usaStates <- usaStates[usaStates$NAME_1 != "Hawaii", ]
usaStates <- usaStates[usaStates$NAME_1 != "Alaska", ]
# convert to sf object
usaStates <- sf::st_as_sf(usaStates)
# simplify the geometry
usaStates <- sf::st_simplify(usaStates, dTolerance = 100)

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
    data = z,
    aes(color = factor(status)), # Map the binary variable to color
    size = 0.65,                       # Size of the points
    alpha = 1                     # Transparency of the points
  ) +
  
  # 3. Manually define colors for the binary variable
  # It's good practice to explicitly choose colors for binary variables
  scale_color_manual(
    name = "Status",          # Title for the legend
    values = c("negative" = "#F8766D", "positive" = "#00BFC4"), # Assign specific colors to 0 and 1
    labels = c("negative", "positive") # Labels for the legend
  ) +
  
  annotate("text",
           x = -122, # X-coordinate (longitude) for the text position
           y = 50,  # Y-coordinate (latitude) for the text position
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
    legend.text = element_text(size = 12)   
  )

ggsave("/Users/mfitzpatrick/code/InvasionModels/Graphics/revision/forManuscript/FIGURE7b_bromus.png", 
       width = 10, 
       height = 8, 
       dpi = 300)


# raster prediction plot
predPlotRaster <- rast("/Users/mfitzpatrick/Desktop/bromus_little_r_Pred.v2.tif")
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
  geom_tile(data = predPlot_df, aes(x = x, y = y, fill = bromus_little_r_Pred.v2)) +
  
  # Add the sf object as an overlay
  geom_sf(data = usaStates,
          fill = NA,
          color = "black",
          linewidth = 0.25) + # No fill, black border
  
  annotate("text",
           x = -122, # X-coordinate (longitude) for the text position
           y = 50.5,  # Y-coordinate (latitude) for the text position
           label = "a", # The actual text you want to display
           size = 16, # Font size of the text
           color = "black", # Color of the text
           fontface = "bold" # Optional: Make the text bold
  ) +
  
  # This ensures correct aspect ratio and alignment.
  coord_sf(
    xlim = c(-130, -80),
    ylim = c(30, 53),
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
                       name = "r",
                       breaks = seq(0.2, 1.0, by=0.2)) # Customize legend title

ggsave("/Users/mfitzpatrick/code/InvasionModels/Graphics/revision/forManuscript/FIGURE7a_bromus.png", 
       width = 10, 
       height = 8, 
       dpi = 300)



# medfly ------------------------------------------------------------------
medFlyrast <- rast("/Users/mfitzpatrick/Desktop/medfly_little_r_Pred.tif")
names(medFlyrast) <- "status"

z <- readRDS("/Users/mfitzpatrick/code/InvasionModels/DataObjects/medfly.rds")
# select lon and lat columns
z <- z[, c("lon", "lat")]
z <- na.omit(z) # remove rows with NA values
status <- terra::extract(medFlyrast, z)[,2]
z <- cbind(z, status) # combine lon, lat, and status columns
z <- na.omit(z) # remove rows with NA values
z$status[z$status<=1] <- "negative" 
# convert z to an sf object
z <- sf::st_as_sf(z, coords = c("lon", "lat"), crs = 4326) # Assuming WGS84 projection

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

# simplify the geometry
polys <- sf::st_simplify(polys, dTolerance = 1000)


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
    data = z,
    aes(color = factor(status)), # Map the binary variable to color
    size = 2,                       # Size of the points
    alpha = 1                     # Transparency of the points
  ) +
  
  # 3. Manually define colors for the binary variable
  # It's good practice to explicitly choose colors for binary variables
  scale_color_manual(
    name = "Status",          # Title for the legend
    values = c("negative" = "#F8766D", "positive" = "#00BFC4"), # Assign specific colors to 0 and 1
    labels = c("negative", "positive") # Labels for the legend
  ) +
  
  annotate("text",
           x = -84.5, # X-coordinate (longitude) for the text position
           y = 31,  # Y-coordinate (latitude) for the text position
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
    legend.text = element_text(size = 12)   
  )

ggsave("/Users/mfitzpatrick/code/InvasionModels/Graphics/revision/forManuscript/FIGURE3b_medfly.png", 
       width = 10, 
       height = 8, 
       dpi = 300)

# raster prediction plot
predPlotRaster <- medFlyrast
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
           y = 31,  # Y-coordinate (latitude) for the text position
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
                       breaks = seq(0.02, 0.1, by=0.02)) # Customize legend title

ggsave("/Users/mfitzpatrick/code/InvasionModels/Graphics/revision/forManuscript/FIGURE3a_medfly.png", 
       width = 10, 
       height = 8, 
       dpi = 300)
