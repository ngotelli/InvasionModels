# New visualizations of data layers
# created by Matt for ebird records of house sparrows and veg survey data for cheatgrass. Graphs show demographic growth rate as a function of spatial position.

# 14 April 2025
# NJG

library(ggplot2)
library(sf) # for spatial data
library(geodata) # admin boundaries
library(spatialEco) # dissolve polygons
library(raster)

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

# combine all Caribbean islands into a single sf object
caribbean_islands <- rbind(cuba, dominican_republic, puerto_rico, jamaica, trinidad_and_tobago, bahamas)

# combine canada and usa into a single sf object
polys <- rbind(usa, canada, central_america, caribbean_islands)

# simplify the geometry
polys <- sf::st_simplify(polys, dTolerance = 1000)

# ggplot(data=z,aes(x=x, y=y)) +
#   geom_raster(aes(fill = layer)) +
#   coord_fixed() +  # Maintain aspect ratio
#   xlim(-130,-65) +
#   ylim(25,55) +
#   labs(x="",y="") +
#    theme_grey(base_size=18) + 
#   scale_fill_gradientn(colors = hcl.colors(10, "Viridis"))
# 
# # version that adds USA boundary
# ggplot() +
#   geom_raster(data = z, aes(x = x, y = y, fill = layer)) +
#   geom_sf(data = usa_canada, color = "black", fill = NA) + # add USA polygon
#   coord_sf() +  # seems to be required is plotting an sf object...
#   xlim(-130,-65) +
#   ylim(25,55) +
#   labs(x="",y="") +
#   theme_grey(base_size=18) + 
#   scale_fill_gradientn(colors = hcl.colors(10, "Viridis"))

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
          linewidth = 0.5) + # No fill, black border
  
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
    plot.background = element_rect(fill = "white", color = NA),  # White plot area background
    panel.grid = element_blank(), # No grid lines
    axis.title = element_blank(), # Optional: Remove axis titles
    axis.text = element_blank(), # Keep axis text visible if desired
    # Optional: Adjust legend position or remove if not needed
    legend.position = "right" # Or "none" if you don't want a legend
  )

# --- Exporting High-Resolution (Optional) ---
# If you need to save this plot as a high-resolution image, use ggsave:
ggsave("/Users/mfitzpatrick/code/InvasionModels/Graphics/revision/forManuscript/FIGURE5b_houseSparrow.png", width = 10, height = 8, dpi = 300)
# ggsave("my_high_res_plot.tiff", width = 8, height = 7, dpi = 300) # For TIFF output


ggplot(data=z,aes(x=x, y=y)) +
  geom_raster(aes(fill = status)) +
  coord_fixed() +  # Maintain aspect ratio
  xlim(-130,-65) +
  ylim(25,55) +
  labs(x="",y="") +
  theme_grey(base_size=18) + 
  #scale_fill_gradientn(colors = hcl.colors(10, "Viridis"))
  scale_fill_discrete(c("coral","cornflowerblue"))



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
    linewidth = 0.5        # Border thickness
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
  
  # 4. Set the coordinate system
  coord_sf(
    xlim = c(-125, -100),
    ylim = c(25, 50),
    expand = FALSE # Set to FALSE to remove the small padding around the limits
  ) +
  
  # 5. Customize theme (white background, no grid lines, no axis labels)
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(), # Remove axis titles
    axis.text = element_blank(),  # Remove axis tick labels (coordinates)
    axis.ticks = element_blank(), # Remove axis tick marks
    legend.position = "right",    # Position the legend
    plot.title = element_text(hjust = 0.5) # Center plot title
  ) 

ggsave("/Users/mfitzpatrick/code/InvasionModels/Graphics/revision/forManuscript/FIGURE7b_bromus.png", 
       width = 10, 
       height = 8, 
       dpi = 300)



# version that adds USA states boundaries
# can't seem to get the point colors to work...
ggplot() +
  geom_sf(data = usaStates, color = "black", fill = NA) +
  geom_point(data = z, aes(x = x, y = y, fill = status), size=0.65) +
  xlim(-125, -100) +
  ylim(25,50) +
  labs(x="",y="") +
  theme_grey(base_size=16) + 
  scale_fill_discrete(c("coral","cornflowerblue"))

ggplot(data=z,aes(x=x, y=y,color=status)) +
  geom_point(size=0.65) +
  coord_fixed() +  # Maintain aspect ratio
  
  # Customize theme for white background and no grid lines
  theme_minimal() + # Start with a minimal theme
  theme(
    panel.background = element_rect(fill = "white", color = NA), # White background, no border
    plot.background = element_rect(fill = "white", color = NA),  # White plot area background
    panel.grid = element_blank(), # No grid lines
    axis.title = element_blank(), # Optional: Remove axis titles
    axis.text = element_blank(), # Keep axis text visible if desired
    # Optional: Adjust legend position or remove if not needed
    legend.position = "right" # Or "none" if you don't want a legend
  ) + 
  
  labs(x="",y="") +
  theme_minimal(base_size=16) + 
  scale_fill_discrete(c("gray","red"))

