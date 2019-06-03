# Matrix projection model for population growth
# with an environmental driver.
#
# Simplified implementation of model from 
# Gotelli & Ellison (2002,2006)
#
# Life table analysis for Med Fly reared
# in 4 warm:cool temperature regimes. We use
# the average temperature in these 4 treatments.
# "Age-based" model constructed by using median
# lx values for each stage. Therefore, stage persistence
# probabilities are not incorporated. lx values are
# converted to stage-specific survivorship as l(x+1)/l(x)
# 
# Reproduction is 0.5*average daily fecundity.
# 
# Data from Tables 3 and 4 in:
# Vargas, R.I., W.A. Walsh, D. Kanehisa, 
# J.D. Stark, and T. Nishida. 2000. 
# Annals of the Entomologial Society of America 93: 75-81.

#NJG
# 14 May 2019
#-------------------------------------
source("Scripts/MatrixFunctions.R")
#-------------------------------------
# Global variables
# set up transition matrices (data from Vargas et al.)
fly_18 <- matrix(c(0,0.97,0,0,0,0,0.887,0,0,0,0,1.0,1.35,0,0,0),nrow=4)
fly_24 <- matrix(c(0,0.92,0,0,0,0,0.913,0,0,0,0,0.964,3.4,0,0,0),nrow=4)
fly_23 <- matrix(c(0,0.92,0,0,0,0,0.962,0,0,0,0,0.870,5.6,0,0,0),nrow=4)
fly_29 <- matrix(c(0,0.92,0,0,0,0,0.847,0,0,0,0,0.782,4.0,0,0,0),nrow=4)

# fold into a list
fly_dat <- list(fly_18,fly_24,fly_23,fly_29)
 
# set up experimental temperature vector
exp_tmp <- c(18.5,24,23.5,29.5)

# set up temperature input vector for generating continental map
con_temp <- 32

# for lam2_gen function set up vector of empirical reported lambdas:
emp_lam <- c(0.051,0.120,0.137,0.092)
emp_lam <- exp(emp_lam)
################################################################################
# download monthly worldclim temp, crop to North America, calc mean temp
tmin <- getData(name="worldclim",
                     var='tmin',
                     res=10, # coarse rez for now
                     download=T,
                     path=getwd())
tmin <- crop(tmin, extent(-170, -40, 20, 80))

tmax <- getData(name="worldclim",
                var='tmax',
                res=10,
                download=T,
                path=getwd())
tmax <- crop(tmax, extent(-170, -40, 20, 80))
tmean <- (tmax+tmin)/20 # rasters are T*10, so div by 20 to get correct scale
names(tmean) <- paste0("tmean", 1:12)
################################################################################
# run in parallel to speed calcs
lambda_rasts <- mclapply(1:12, function(x, tempRasts){
  rast <- tempRasts[[x]]
  keep <- !is.na(rast[]) # index NAs
#-------------------------------------
  lamb <- lam_gen(rast[keep]) # calc lamba for each temp
  lamb[lamb<1] <-0 # clip non-increasing values to 0
#-------------------------------------
  # lamb <- lam2_gen(x=exp_tmp,
  # 								 y=emp_lam,
  # 								 z=rast[keep]) # calc lamba for each temp
  # lamb[lamb<1] <-0 # clip non-increasing values to 0
#-------------------------------------
  rast[which(keep)] <- unlist(lamb) # assign lamba back to raster
  return(rast)}, tempRasts=tmean, mc.cores=2)
lambda_rasts <- do.call(stack, lambda_rasts)
names(lambda_rasts) <-paste0("medfly", 1:12)
plot(lambda_rasts)

# calc geometric mean of lambda across the 12 months
lambdaMap <- calc(lambda_rasts, fun=function(x){prod(x)^(1/length(x))})
plot(lambdaMap, main="geoMean_of_lambda")
