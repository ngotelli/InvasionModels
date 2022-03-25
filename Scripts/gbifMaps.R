library(dismo)
library(mapdata)

# medfly
medfly <- gbif(genus="Ceratitis", 
               species="capitata", 
               geo=T)

medfly <- subset(medfly, country %in% c("United States", "Canada", "Mexico"))

# plot
map('worldHires',  c('USA', 'Canada', 'Mexico'), xlim=c(-180,-30))
points(medfly$lon, medfly$lat, pch=20, col=rgb(0,0,1,0.5))


# cheatgrass
cheatgrass <- gbif(genus="Bromus", 
               species="tectorum", 
               geo=T)

cheatgrass <- subset(cheatgrass, country %in% c("United States", "Canada", "Mexico"))

# plot
map('worldHires',  c('USA', 'Canada', 'Mexico'), xlim=c(-180,-30))
points(cheatgrass$lon, cheatgrass$lat, pch=20, col=rgb(0,0,1,0.5))




# housesparrow
housesparrow <- gbif(genus="Passer", 
                   species="domesticus", 
                   geo=T)

housesparrow <- subset(housesparrow, country %in% c("United States", "Canada", "Mexico"))

# plot
map('worldHires',  c('USA', 'Canada', 'Mexico'), xlim=c(-180,-30))
points(housesparrow$lon, housesparrow$lat, pch=20, col=rgb(0,0,1,0.5))
