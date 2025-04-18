library(dismo)
library(mapdata)
# other libraries for alternative code
 library(rgbif)
# library(taxize)
# library(tidyverse)
# library(rgdal)
# library(rgeos)

# medfly
medfly <- gbif(genus="Ceratitis", 
               species="capitata", 
               geo=T, 
               removeZeros=T,
               ext=extent(c(-180, -30, 0, 90)))

medfly <- subset(medfly, country %in% c("United States", "Canada", "Mexico"))


# medfly <- gbif(genus="Ceratitis", 
  #             species="capitata", 
  #             geo=T)
# medfly <- subset(medfly, country %in% c("United States", "Canada", "Mexico"))
# saveRDS(medfly,file="DataObjects/medfly.RDS")
medfly <- readRDS(file="DataObjects/medfly.RDS")

# plot
map('worldHires',  c('USA', 'Canada', 'Mexico'), xlim=c(-87,-78),ylim=c(22,31))
points(medfly$lon, medfly$lat, pch=20, col=rgb(0,0,1,0.5))


# cheat grass
cheatgrass <- gbif(genus="Bromus", 
               species="tectorum", 
               geo=T,
               removeZeros=T,
               ext=extent(c(-180, -30, 0, 90)))

cheatgrass <- subset(cheatgrass, country %in% c("United States", "Canada", "Mexico"))


map('worldHires',  c('USA', 'Canada', 'Mexico'), xlim=c(-150,-51),ylim=c(22,55))
points(medfly$lon, medfly$lat, pch=20, col=rgb(0,0,1,0.5))
# cheatgrass
# cheatgrass <- gbif(genus="Bromus", 
#                species="tectorum", 
#                geo=T)
# 
# cheatgrass <- subset(cheatgrass, country %in% c("United States", "Canada", "Mexico"))
# saveRDS(cheatgrass,file="DataObjects/cheatgrass.RDS")
cheatgrass <- readRDS(file="DataObjects/cheatgrass.RDS")

# plot
map('worldHires',  c('USA', 'Canada', 'Mexico'), xlim=c(-180,-30))
points(cheatgrass$lon, cheatgrass$lat, pch=20, col=rgb(0,0,1,0.5))



# house sparrow
housesparrow <- gbif(genus="Passer", 
                   species="domesticus", 
                   geo=T,
                   removeZeros=T,
                   args=c("year"==2021),
                   ext=extent(c(-70, -50, 30, 40)))

housesparrow <- subset(housesparrow, country %in% c("United States", "Canada", "Mexico"))


# housesparrow
# housesparrow <- gbif(genus="Passer", 
#                    species="domesticus", 
#                    geo=T)
# 
# housesparrow <- subset(housesparrow, country %in% c("United States", "Canada", "Mexico"))

# plot
# map('worldHires',  c('USA', 'Canada', 'Mexico'), xlim=c(-180,-30))
# points(housesparrow$lon, housesparrow$lat, pch=20, col=rgb(0,0,1,0.5))


# dat <- occ_search(scientificName = "Passer domesticus", 
#                                     continent = "north_america",limit = 250000)$data
# saveRDS(dat,file="DataObjects/housesparrow.RDS")

house_sparrow <- readRDS("DataObjects/housesparrow.RDS")
map('worldHires',  c('USA', 'Canada', 'Mexico'), xlim=c(-180,-30))
points(house_sparrow$decimalLongitude, house_sparrow$decimalLatitude, pch=20, col=rgb(0,0,1,0.5))
