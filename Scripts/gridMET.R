library(raster)
library(parallel)

years <- 1981:2000
tmps <- c("tmmn", "tmmx")
ncs <- list.files(path="/Volumes/generalData/climateData/current/gridMET", pattern=".nc", full.names = T)

for(y in 1:length(years)){
  year <- years[y]
  yearDates <-  seq.Date(from=as.Date(paste0(year, "/1/1")), to=as.Date(paste0(year, "/12/31")), "day")
  for(t in 1:2){
    tmp <- tmps[t]
    nc <- ncs[grep(year, ncs)]
    nc <- nc[grep(tmp, nc)]
    climYear <- brick(nc)
    
    #mclapply(1:12, function(x, year, yearDates, rastStack, tmp){
    for(m in 1:12){
      if(m>9){
        mn <- as.character(m)
      } else{
        mn <- as.character(paste0(0, m))
      }
      monthDates <- yearDates[grep(paste0(year, "-", mn), yearDates)]
      dateIndex <- which(yearDates %in% monthDates)
      
      print(paste(year, mn, tmp))
      
      climMonth <- climYear[[dateIndex]]
      #climMonth[which(is.na(climMonth[]))] <- NA
      #NAvalue(climMonth) <- NA
      climMonth <- climMonth-273.15
      
      if(tmp=="tmmn"){
        dcr <- FALSE
      }
      if(tmp=="tmmx"){
        dcr <- TRUE
      }
      
      extTempMonth <- calc(climMonth, function(x){
        mean(sort(x, decreasing=dcr)[1:10])
      })
      #return(extTempMonth)
      writeRaster(extTempMonth, 
      paste0("/Volumes/Projects/activeProjects/misc/RoL/",
             "ext_", tmp, "_", year, "_", mn, ".tif"),
      overwrite=TRUE)
    #}, mc.cores=12, year=year, yearDates=yearDates, rastStack=climYear, tmp=tmp)
    }
  }
}

      