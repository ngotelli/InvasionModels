library(raster)

years <- 1981:2000
tmps <- c("tmmn", "tmmx")[t]
ncs <- list.files(path="/Volumes/generalData/climateData/current/gridMET", pattern=".nc", full.names = T)

for(y in 1:length(years){
  year <- years[y]
  yearDates <-  seq.Date(from=as.Date(paste0(year, "/1/1")), to=as.Date(paste0(year, "/12/31")), "day")
  for(t in 1:2){
    tmp <- tmps[t]
    nc <- ncs[grep(year, ncs)]
    nc <- nc[grep(tmp, nc)]
    climYear <- brick(nc)
    for(m in 1:12){
      if(m>10){
        m <- as.character(m)
      } else{
        m <- as.character(paste0(0, m))
      }
      monthDates <- yearDates[grep(paste0(year, "-", m), yearDates)]
      dateIndex <- which(yearDates %in% monthDates)
      climMonth <- climYear[[dateIndex]]
      climMonth[which(is.na(climMonth[]))] <- NA
      extTempMonth <- calc(climMonth, function(x){
        mean(sort(x, decreasing=T)[1:10])
      })