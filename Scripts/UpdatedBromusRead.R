# Latest rendering of figures for best fitting model
# and final data for Bromus
# 29 June 2020

# New update of data with Alden's modification for conversion of % snow cover to snow depth

#11 October 2019
library(tidyverse)
library(patchwork)
# source("Scripts/BromusLambdaFunction.R")
# -------------------------------------------------------------------------

# Script for opening and cleaning new Bromus data
# 04 Sept 2019
# NJG

# Read in data ------------------------------------------------------------

df <- read.table("CleanData/Bromus_Clean_V3_Oct-08-19.csv",sep=",",
                stringsAsFactors=FALSE, header=TRUE)
str(df)

df <- df[,-ncol(df)] # remove empty final column 
df_clean <- df[complete.cases(df),] # remove 4 missing lambda
df_preds <- df_clean[,8:18] # create dataframe of predictor variables 
df_clean$r <- log(df_clean$Lambda) # create response variable r from log(lambda)

best_preds <- df_preds[,c(6,7,11,8,3,1)]
y <- df_clean$r
x_1 <- best_preds[,1] 
x_2 <- best_preds[,2] 
x_3 <- best_preds[,3] 
x_4 <- best_preds[,4] 
x_5 <- best_preds[,5] 
x_6 <- best_preds[,6]

final_df <- data.frame(r=y, snow_cover=x_1,
                       win_precip_snow=x_2,
                       win_precip_tot=x_3,
                       win_precip_rain=x_4,
                       grow_seas_precip_rain=x_5,
                       density=x_6)

# best fitting final model
model_reduced <- lm(data=final_df,
r~poly(snow_cover,2, raw=TRUE) + 
poly(win_precip_rain,2, raw=TRUE) + 
poly(density,1, raw=TRUE))
print(model_reduced)


print(summary(model_reduced))

#-------------------------------------
# FUNCTION pred_brom_lam
# predict lambda based on snow cover, precip, and density
# input: f=fitted model, new= data frame for prediction
# output: new model coefficient
pred_brom_lam <- function(new=NULL,f=model_reduced){
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

# Figure 4. r versus snow cover
model_snow_cover <- summary(lm(data=final_df,r~poly(snow_cover,2)))
print(model_snow_cover)
Fig4 <- ggplot(data=final_df,aes(x=snow_cover,y=r)) +
  geom_point() +
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
  xlab("Proportion Snow Cover") +
  theme_bw(base_size=25)

plot(Fig4)

# Figure 5 r versus winter rain precipitation
model_win_precip_rain <- summary(lm(data=final_df,r~poly(win_precip_rain,2)))
print(model_win_precip_rain)
ggplot(data=final_df,aes(x=win_precip_rain,y=r)) +
  geom_point() +
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 2, raw=TRUE),colour="red") 
Fig5 <- ggplot(data=final_df,aes(x=win_precip_rain,y=r)) +
  geom_point() +
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
  xlab("Winter Rain (mm)") +
  theme_bw(base_size=25)
plot(Fig5)




# Figure 6 r versus initial density
model_dd <- summary(lm(data=final_df,r~density))
print(model_dd)
ggplot(data=final_df,aes(x=density,y=r)) +
  geom_point() +
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 1, raw=TRUE),colour="red") 

Fig6 <- ggplot(data=final_df,aes(x=density,y=r)) +
  geom_point() +
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 1, raw=TRUE),colour="red") +
  xlab("Density (Plants per Plot)") +
    ylim(c(-4,4)) +
  theme_bw(base_size=25)
plot(Fig6)

Fig_Composite <- Fig4 / Fig5 / Fig6
plot(Fig_Composite)
ggsave(filename="Graphics/Composite.jpeg",
       plot=Fig_Composite,
       device="jpg",
       width=7,
       height=9,
       units="in")

## Heat map for visualizing lambda values


# construct data frame for sweeping parameter space of snow cover and winter precip
# density is held constant at N = 0
# snow cover ranges [0,1]
# winter precip rain ranges [0,200]
vec_len <- 30
snow_vec <- seq(0,1,length.out=vec_len)
precip_vec <- seq(0,200,length.out=vec_len)
hutchinson_df <- expand.grid(snow_cover=snow_vec,win_precip_rain=precip_vec)
hutchinson_df$density <- 0

# this data frame has the structure needed as input for creating a continental map
str(hutchinson_df)

# now run the pred_brom_lam() function to predict r and bind to input data frame
heat_map_data <- cbind(hutchinson_df,r=pred_brom_lam(new=hutchinson_df))
str(heat_map_data)

# construct heat map
heat_map <- ggplot(data=heat_map_data,mapping=aes(x=snow_cover,y=win_precip_rain,fill=r)) +
 geom_tile() + scale_fill_viridis_c() +
  xlab("Snow Cover") +
  ylab("Winter Rain") +
  theme_gray(base_size=18)
ggsave(filename="Graphics/HeatMap.jpeg",
       plot=heat_map,
       device="jpg",
       width=7,
       height=5,
       units="in")