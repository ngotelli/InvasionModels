library(dismo)
library(mapdata)

# medfly
medfly <- gbif(genus="Ceratitis", 
               species="capitata", 
               geo=T, 
               removeZeros=T)#,
#ext=extent(c(-180, -30, 0, 90)))

#medfly <- subset(medfly, country %in% c("United States", "Canada", "Mexico"))

# plot
map('worldHires')#,  c('USA', 'Canada', 'Mexico'), xlim=c(-180,-30))
points(medfly$lon, medfly$lat, pch=20, col=rgb(0,0,1,0.5))


# cheat grass
cheatgrass <- gbif(genus="Bromus", 
                   species="tectorum", 
                   geo=T,
                   removeZeros=T)#,
#ext=extent(c(-180, -30, 0, 90)))

# plot
map('worldHires')#,  c('USA', 'Canada', 'Mexico'), xlim=c(-180,-30))
points(cheatgrass$lon, cheatgrass$lat, pch=20, col=rgb(0,0,1,0.5))



# house sparrow
housesparrow <- gbif(genus="Passer", 
                     species="domesticus", 
                     geo=T,
                     removeZeros=T,
                     args=c("year"==2021))#,
                     #ext=extent(c(-70, -50, 30, 40)))

#housesparrow <- subset(housesparrow, country %in% c("United States", "Canada", "Mexico"))

# plot
map('worldHires')#,  c('USA', 'Canada', 'Mexico'), xlim=c(-180,-30))
points(housesparrow$lon, housesparrow$lat, pch=20, col=rgb(0,0,1,0.5))
