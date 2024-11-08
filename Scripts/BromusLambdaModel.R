# Analyze cheatgrass data for regression model
# of lambda versus snow, precip, and temperature
# Original data from Alden Griffith and Michael Loic
#
# NJG
# 17 July 2019

# Preliminaries -----------------------------------------------------------
library(tidyverse)

# Get input data ----------------------------------------------------------
df <- read.csv(file="CleanData/CleanBromusData.csv",comment.char="#")
df <- df[,-ncol(df)]                 # remove empty final column
df_clean <- df[complete.cases(df),]  # remove 4 missing lambda

df_preds <- df_clean[,8:18]           # create dataframe of predictor variables
# Function Model Fit ------------------------------------------------------
# inputs: response variable, predictor variable
# outputs: adjusted r2 for linear and quadratic regression models

quad_fit <- function(x=df_clean$Snow.Cover, y=df_clean$Lambda){
  M1 <- summary(lm(y~x))
  M2 <- summary(lm(y~poly(x,2)))
  return(c(M1$adj.r.squared,M2$adj.r.squared))
}
# -------------------------------------------------------------------------
# map all predictor variables to quad_fit
summary_df <- map(df_preds,quad_fit,y=df_clean$Lambda)

# put into a summary data frame and add column labels
summary_df <- t(bind_rows(summary_df))
colnames(summary_df) <- c("Linear","Quadratic")
# -------------------------------------------------------------------------

print(summary_df)


# Initial multiple regression model fit ----------------------------------

# best variables are:
# Grow.Season.DD.all.days (4)
# Grow.Season.Precip.Total (5)
# Snow.Cover (6)
best_preds <- df_preds[,4:6]
  
y <- df_clean$Lambda
x_1 <- best_preds[,1]
x_2 <- best_preds[,2]
x_3 <- best_preds[,3]

model_1 <- summary(lm(y~poly(x_1,2) + poly(x_2,2) + poly(x_3,2)))
print(model_1)


# Simplified best fit model -----------------------------------------------

model_2 <- summary(lm(y~poly(x_1,2) + poly(x_3,2)))
print(model_2)


# Try adding an interaction term -----------------------------------------

model_3 <- summary(lm(y~poly(x_1,2) + poly(x_3,2) + x_1*x_3))
print(model_3)


# Function Lambda Model -----------------------------------------------
# inputs: lambda vector, and data frame of predictor variables
# outputs: linear model for forecasting

lambda_model <- function(y=df_clean$Lambda,z=best_preds){
  y <- df_clean$Lambda
  x_1 <- z[,1] # Grow.Season.DD.all.days
  x_2 <- z[,3]  # Snow.Cover
  forecast_model <- lm(y~poly(x_1,2) + poly(x_2,2))

}

# Function Lambda predict -------------------------------------------------
# inputs: linear model, degree days (vector), snow cover (vector)
# outputs: predicted lambda

lambda_predict <- function(m=my_model,degree_days=c(800,600),snow=c(0.83,.74)) {

z <- predict(object=m, newdata=data.frame(x_1=degree_days,x_2=snow))
z[z<0] <- 0   # reset to 0 any predicted negative values for lambda
return(z)


}

lambda_predict(m=lambda_model(),degree_days=sample(600:800,10),snow=runif(10))
