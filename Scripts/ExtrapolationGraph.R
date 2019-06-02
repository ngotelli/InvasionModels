# Simple graph plot for two different 
# estimation methods for lambda
source("Scripts/MatrixFunctions.R")
library(tidyverse)
# set up experimental temperature vector
exp_tmp <- c(18.5,24,23.5,29.5)

# set up temperature input vector for generating continental map
con_temp <- 10:40 # temperature range for extrapolation

# set up transition matrices for lam_gen (data from Vargas et al.)
fly_18 <- matrix(c(0,0.97,0,0,0,0,0.887,0,0,0,0,1.0,1.35,0,0,0),nrow=4)
fly_24 <- matrix(c(0,0.92,0,0,0,0,0.913,0,0,0,0,0.964,3.4,0,0,0),nrow=4)
fly_23 <- matrix(c(0,0.92,0,0,0,0,0.962,0,0,0,0,0.870,5.6,0,0,0),nrow=4)
fly_29 <- matrix(c(0,0.92,0,0,0,0,0.847,0,0,0,0,0.782,4.0,0,0,0),nrow=4)

# fold into a list
fly_dat <- list(fly_18,fly_24,fly_23,fly_29)

# for lam2_gen function set up vector of empirical reported lambdas:
emp_lam <- c(0.051,0.120,0.137,0.092)
emp_lam <- exp(emp_lam)

# extrapolate lambda from vital rates
t1 <- lam_gen(ct=con_temp,
							tm=exp_tmp,
							tr=fly_dat)
	
# extrapolate lambda from reported lambdas
t2 <- lam2_gen(x=exp_tmp,
							 y=emp_lam,
							 z=con_temp)

# combine into single response vector
lambda.vec <- c(unlist(t1),unlist(t2))
names(lambda.vec) <- NULL

# create temperature scale
temperature <- rep(con_temp,2)

# create labels for the two series
model <- rep(c("Vital Rates Prediction","Lambda Prediction"),each=length(con_temp))

# combine into data frame
df <- data.frame(lambda=lambda.vec,temperature=temperature, model=model)
df2 <- data.frame(lambda=emp_lam,temperature=exp_tmp)
# create extrapolation plot
xlab = expression("Temperature " ( degree*C))
plot_1 <- ggplot(data=df, aes(x=temperature,
															y=lambda,
															shape=model)) +
	geom_point() +
	geom_line() +
	geom_hline(yintercept=1.0,color=I("red")) +
	 geom_point(data=df2,color="blue",shape=21,size=4) +
	 labs(x=xlab) +
	theme_bw()

plot(plot_1)
ggsave(filename="Graphics/Figure1.jpeg",
			 plot=plot_1,
			 device="jpg",
			 width=7,
			 height=5,
			 units="in")