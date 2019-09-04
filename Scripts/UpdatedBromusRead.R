# Script for opening and cleaning new Bromus data
# 04 Sept 2019
# NJG

# Read in data ------------------------------------------------------------

z <- read.table("CleanData/Bromus_Clean_V2_Aug-28-19.csv",sep=",",
                stringsAsFactors=FALSE, header=TRUE)
str(z)

