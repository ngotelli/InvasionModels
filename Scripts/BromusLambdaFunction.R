# Demographic SDM predictor for Bromus
# Uses updated data from Alden Griffiths version 3
# Version 3 makes an improved conversion from snow cover to snow depth
#
## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
# Preliminaries -----------------------------------------------------------
library(tidyverse)

# Get input data ----------------------------------------------------------

# adjust path for grabbing input file
# df <- read.table(file="CleanData/Bromus_Clean_V2_Aug-28-19.csv",
#                  sep=",",header=TRUE,stringsAsFactors=FALSE)
df <- read.table(file="../CleanData/Bromus_Clean_V3_Oct-08-19.csv",
                 sep=",",header=TRUE,stringsAsFactors=FALSE)
df <- df[,-ncol(df)]                 # remove empty final column
df_clean <- df[complete.cases(df),]  # remove 4 missing lambda

df_preds <- df_clean[,8:18]           # create dataframe of predictor variables
df_clean$r <- log(df_clean$Lambda)    # create response variable r from log(lambda)



## ------------------------------------------------------------------------
# best variables are:
# snow.cover (6)
# winter precip snow (7)
# winter precip total (11)
# winter precip rain (8)
# growing season precip rain (3)
# density (1)
best_preds <- df_preds[,c(6,7,11,8,3,1)]
  
y <- df_clean$r
x_1 <- best_preds[,1]
x_2 <- best_preds[,2]
x_3 <- best_preds[,3]
x_4 <- best_preds[,4]
x_5 <- best_preds[,5]
x_6 <- best_preds[,6]

final_df <- data.frame(r=y,
                       snow_cover=x_1,
                       win_precip_snow=x_2,
                       win_precip_tot=x_3,
                       win_precip_rain=x_4,
                       grow_seas_precip_rain=x_5,
                       density=x_6)






## ------------------------------------------------------------------------
model_reduced2 <- lm(data=final_df,r~poly(snow_cover,2, raw=TRUE) +
                                              poly(win_precip_rain,2, raw=TRUE) +
                                              poly(density,1, raw=TRUE)) 
                                              
## ------------------------------------------------------------------------
#-------------------------------------
# FUNCTION pred_brom_lam
# predict lambda based on snow cover, precip, and density
# input: f=fitted model, new= data frame for prediction
# output: new model coefficient
pred_brom_lam <- function(new=NULL,f=model_reduced2){
            if (is.null(new)){
            	x1 <- runif(10,0,1)
            	x2 <- runif(10,0,200)
            	x3 <- rep(0,10)
            	new<- data.frame(snow_cover=x1,
            	                win_precip_rain=x2,
            	                density=x3)}
           
	z <- predict(object=f, newdata=new)
	return(z)
	
}
#-------------------------------------


## ------------------------------------------------------------------------
# construct data frame for sweeping parameter space of snow cover and winter precip
# density is held constant at N = 0
# snow cover ranges [0,1]
# winter precip rain ranges [0,200]
vec_len <- 30
snow_vec <- seq(0,1,length.out=vec_len)
precip_vec <- seq(0,200,length.out=vec_len)
hutchinson_df <- expand.grid(snow_cover=snow_vec,win_precip_rain=precip_vec)
hutchinson_df$density <- 0 # density fixed at 0 regardless of snow and rain values

# this data frame has the structure needed as input for the function pred_brom_lam
str(hutchinson_df)

# you would then call it as
# pred_brom_lam(hutchinson_df)


# Code for heat map construction ------------------------------------------

# now run the pred_brom_lam() function to predict r and bind to input data frame
# heat_map_data <- cbind(hutchinson_df,r=pred_brom_lam(new=hutchinson_df))
# str(heat_map_data)

# construct heat map
# heat_map <- ggplot(data=heat_map_data,mapping=aes(x=snow_cover,y=win_precip_rain,fill=r))
# heat_map + geom_tile() + scale_fill_viridis_c()


# Code for estimating carrying capacity -----------------------------------

# cf <- summary(model_reduced2)$coefficients[,1]    # grab the coefficients
# d <- heat_map_data # rename the data frame

# now create new column with K values
# heat_map_data$K <- (cf[1] + cf[2]*d[[1]] + cf[3]*d[[1]]^2 +
#                    cf[4]*d[[2]] + cf[5]*d[[2]]^2)/(-1*cf[6])

# convert negatives to 0
# heat_map_data$K[heat_map_data$K<=0] <- 0

# create heat map
# heat_map_k <- ggplot(data=heat_map_data,mapping=aes(x=snow_cover,y=win_precip_rain,fill=K))
# heat_map_k + geom_tile() + scale_fill_viridis_c()

# summarize distribution of K values greater than 0
# real_k <- heat_map_data$K[heat_map_data$K>0]
# summary(real_k)

# get snow and precip for smallest positive K
# heat_map_data$snow_cover[which(heat_map_data$K==min(heat_map_data$K[heat_map_data$K>0]))]
# heat_map_data$win_precip_rain[which(heat_map_data$K==min(heat_map_data$K[heat_map_data$K>0]))]

# get snow and precip for largest K
# heat_map_data$snow_cover[which(heat_map_data$K==max(heat_map_data$K))]
# heat_map_data$win_precip_rain[which(heat_map_data$K==max(heat_map_data$K))]