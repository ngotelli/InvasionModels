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
df <- df[,-ncol(df)]
df_clean <- df[complete.cases(df),]
qplot(df_clean$Lambda, color=I("black"), fill=I("tan"))
qplot(y=df_clean$Lambda,x=df_clean$Snow.Cover)


# Function Model Fit ------------------------------------------------------
# inputs: response variable, predictor variable
# outputs: adjusted r2 for linear and quadratic regression models

quad_fit <- function(x=df_clean$Snow.Cover, y=df_clean$Lambda){
  M1 <- summary(lm(y~x))
  M2 <- summary(lm(y~poly(x,2)))
  result <- list(M1,M2)
}
z <- quad_fit()
print(z)
