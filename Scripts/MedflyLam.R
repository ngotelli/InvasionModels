# 23 May 2019
# Modification of original code in MedflyMatrix.R
# This is simpler and just uses directly the 4
# reported lambda values from Vargas et al.
# Output here will be ported over to Matt's code
# for graphics

#-------------------------------------
source("Scripts/MedflyMatrix.R")
# set up experimental temperature vector
exp_tmp <- c(18.5,24,23.5,29.5)

# set up temperature input vector for generating continental map
con_temp <- 1:40

# for lam2_gen function set up vector of empirical reported lambdas:
emp_lam <- c(0.051,0.120,0.137,0.092)
emp_lam <- exp(emp_lam)


#-------------------------------------
#FUNCTION lam2_gen
# generates lambda values 
# input: x=vector of experimental temperatures
#        y=vector of measured lambdas
#        z=vector of temperatures for prediction
lam2_gen <- function(x,y,z){
. <- est_trans(x,y)
. <- pred_trans(z,.)
.[.<0] <- 0  # rescale any negative values
. <- as.list(.)
return(.)
}
lam2_gen(x=exp_tmp,y=emp_lam,z=con_temp)