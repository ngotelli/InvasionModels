# New visualizations of data layers
# created by Matt for ebird records of house sparrows and veg survey data for cheatgrass. Graphs show demographic growth rate as a function of spatial position.

# 14 April 2025
# NJG

library(ggplot2)

# This code ran outside of the repo to create the rds file:

# z<- read.table(file="HouseSparrowGrowthPredictions_at_eBird_points.csv",
#                header=TRUE,sep=",")
# saveRDS(z,"HouseSparrow.rds")

z <- readRDS("NewVisualizations-NJG/HouseSparrow.rds")
ggplot(data=z,aes(x=x, y=y)) +
  geom_raster(aes(fill = layer)) +
  coord_fixed() +  # Maintain aspect ratio
  xlim(-130,-65) +
  ylim(25,55) +
  labs(x="",y="") +
   theme_grey(base_size=18) + 
  scale_fill_gradientn(colors = hcl.colors(10, "Viridis"))
