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
library(popbio)
library(tidyverse)
#-------------------------------------
# Global variables
# set up transition matrices (data from Vargas et al.)
fly_18 <- matrix(c(0,0.97,0,0,0,0,0.887,0,0,0,0,1.0,1.35,0,0,0),nrow=4)
fly_24 <- matrix(c(0,0.92,0,0,0,0,0.913,0,0,0,0,0.964,3.4,0,0,0),nrow=4)
fly_23 <- matrix(c(0,0.92,0,0,0,0,0.962,0,0,0,0,0.870,5.6,0,0,0),nrow=4)
fly_29 <- matrix(c(0,0.92,0,0,0,0,0.847,0,0,0,0,0.782,4.0,0,0,0),nrow=4)

# fold into a list
fly_dat <- list(fly_18,fly_24,fly_23,fly_29)
 print(fly_dat)
 
# set up experimental temperature vector
exp_tmp <- c(18.5,24,23.5,29.5)

# set up temperature input vector for generating continental map
con_temp <- 1:40
#-------------------------------------
# FUNCTION est_trans
# get model coefficients from a quadratic
# input: x=vector of temperatures, y=vector of transition elements
# output: fitted regression model
est_trans <- function(x=runif(4),y=runif(4)){
	fitted <- lm(y~poly(x,2))
	return(fitted)
	
}
#-------------------------------------
# FUNCTION pred_trans
# predict new model coefficient
# input: f=fitted model, tmp= new temperature
# output: new model coefficient
pred_trans <- function(new=runif(1,0,40),f=NULL){
            if (is.null(f)){
            	x <- runif(4)
            	y <- runif(4)
            	df<- data.frame(x,y)   
            	f<- lm(y~poly(x,2),data=df)}
           
	z <- predict(object=f, newdata=data.frame(x=new))
	return(z)
	
}
#-------------------------------------
# FUNCTION lam_gen
# generate lambda for a given temperature
# input: continental temperatures, experimental temperature, transition matrices
# output: lambda
lam_gen <- function(ct=con_temp, 
										tm=exp_tmp,
										tr=fly_dat){
	t1 <- fly_dat %>% map(~ .x[.x>0])	%>% # grab non-zero coefficients for modeling
	bind_cols() %>%
	t() %>%
	data.frame() %>%
 map(est_trans,x=tm) %>% # fit temperature function from quadratic regression
	t() %>%
 map(pred_trans,new=ct) %>% # predict new transition elements 
	bind_cols() %>%
	t() %>%
	as.data.frame()
#-------------------------------------
mat_fill <- function(z=1) { # place predicted coefficients in new transition matrix
	nm <- matrix(0,nrow=4,ncol=4)
	nm[1,4] <- z[4]
	nm[2,1] <- z[1]
	nm[3,2] <- z[2]
	nm[4,3] <- z[3]
	nm_sub <- nm[-1,] # pull out non-fecundity transitions for boundary check
	nm_sub[nm_sub>1] <- 1 # set upper bound on non-fecundity transition probabilities
	nm_sub[nm_sub<0] <- 0 # set lower bound on non-fecundity transition probabilities
	nm <- rbind(nm[1,],nm_sub)
	 if(nm[1,4] < 0) nm[1,4] <- 0 # check lower bound for fecundity
	return(nm)
	
}
#-------------------------------------
t2 <- map(t1,mat_fill) %>% # extract population growth rate from 
	map(lambda)
return(t2) 

}
#-------------------------------------
lam_gen() # run the model