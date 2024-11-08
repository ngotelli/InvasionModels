library(dismo)
library(rnaturalearth)
library(terra)
library(sf)
library(geodata)
library(dplyr)
library(ggplot2)
library(ncdf4)
library(QBMS)


# medfly -----------------------------------------------------------------------
# download worldclim monthly temperature data
tavg <- worldclim_global(var='tavg', res=2.5, path="/home/mfitzpatrick/Projects/InvasionModels/")

# medfly occurrence data from GBIF
medfly <- sp_occurrence(genus='Ceratitis', species='capitata',
                          download=TRUE, geom=TRUE, 
                          removeZeros = TRUE) %>%
  # select columns of interest
  select(species, country, lon, lat, year, month, coordinateUncertaintyInMeters) %>%
  # remove high uncertainty records
  filter(coordinateUncertaintyInMeters < 5000) %>% # 2.5 arc-min = ~ 5km
  # remove records before 2000
  #filter(year >= 1970 & year <= 2000) %>%
  # remove duplicates
  distinct(lon, lat, .keep_all = TRUE) %>%
  # convert to sf object
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# ggplot of records 
ggplot() +
  geom_sf(data = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")) +
  geom_sf(data = medfly, aes(color = year)) +
  theme_minimal()

# function to extract temperature data from terraclimate

get_terraclimate(
  -1.59,
  -77.71,
  from = "1958-01-01",
  to = "1960-01-01",
  clim_vars = "tmax",
  month_mask = 1,
  offline = FALSE)


# read in the full period of record using aggregated files

data <- as.numeric(ncvar_get(nc, varid = var, start = start, count))


#remove spatial duplicates:  
mf_cells <- cellFromXY(tavg, st_coordinates(medfly)) #identify cells from raster for all occurrences
mf_dup <- duplicated(mf_cells) #which cells have multiple occurrences (ie spatial duplicates)
medfly <- medfly[!mf_dup,] #remove spatial duplicates


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
