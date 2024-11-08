# --------------------------------------
# Simple life history model for Passer domesticus
# Data from spreadsheet House Sparrow Lambda.xls provided by Jesse Krause 
# These data used to create simple measure of R0 and relate it to 
# mean annual temperature from sites where data came from
# for now, assume generation time is only 1 year
# 22 Sep 2020
# NJG
# --------------------------------------
library(tidyverse)
source("Scripts/MatrixFunctions.R")

con_temp <- 15:35 # temperature range for extrapolation


# Vector of total number of offspring fledged per season by a pair
fledge <- c(7.07, 7.00, 4.32, 6.12, 5.61, 2.31, 3.14, 3.30,
            5.07, 3.87, 8.09, 5.34, 5.28)

# Divide by 2 for per capita
fledge <- fledge/2

# multiply by 50% for survival through first season
R_0 <- fledge*0.50

# provide generation time (set to 1.0 for now)
gen_time <- 1.0

# estimate r
r <- log(R_0)/gen_time

# mean average temperature for sites with data
temp <- c(10.8, 10.0, 21.0, 21.5, NA, 20.3, 18.2,
          18.1, 20.1, 17.6, NA, 15.7, 19.4)

# remove missing values
r1 <- r[c(-5,-11)]
temp1 <- temp[complete.cases(temp)]
final_df <- data.frame(temp=temp1,r=r1)
# extrapolate lambda from reported lambdas
model <- lm(data=final_df,r1~poly(temp1,2, raw=TRUE))

summary(model)
# -------------------------------------------------------------------------
# Function quad Graph
# Function quad_plot ------------------------------------------------------
# inputs: response variable, predictor variable
# outputs: ggplot graph with data and fitted line

quad_plot <- function(x,y){
  df <- data.frame(x,y)
  ggplot(data=df,aes(x=x,y=y)) +
    geom_point(size=4) +
    stat_smooth(method="lm", se=TRUE, fill=NA,
                formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
    stat_smooth(method="lm", se=TRUE, fill=NA,
                formula=y ~ poly(x, 1, raw=TRUE),colour="blue") +
    ylab("r") +
    xlab("Temperature") +
    theme_bw(base_size = 20)
}
plot(quad_plot(x=temp1,y=r1))