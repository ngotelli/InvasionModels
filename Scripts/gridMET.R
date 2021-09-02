library(raster)

years <- 1981:2000
  
for(i in 1:length(years){
  year <- years[i]
  ncs <- list.files(path="/Volumes/generalData/climateData/current/gridMET", pattern=".nc", full.names = T)
  nc <- ncs[grep(year, ncs)]
  climYear <- brick(nc[1])
  dayDates <-  seq.Date(from=as.Date(paste0(year, "/1/1")), to=as.Date(paste0(year, "/12/31")), "day")
  monthDates <- dayDates[grep(paste0(year, "-", "02"), dayDates)]
  dateIndex <- which(dayDates %in% monthDates)
  climMonth <- climYear[[dateIndex]]
  climMonth[which(is.na(climMonth[]))] <- NA
  ccc <- calc(climMonth, function(x){
    mean(sort(x, decreasing=T)[1:10])
  })