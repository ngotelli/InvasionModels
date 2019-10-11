# New update of data with Alden's modification for conversion of % snow cover to snow depth

#11 October 2019

# -------------------------------------------------------------------------

# Script for opening and cleaning new Bromus data
# 04 Sept 2019
# NJG

# Read in data ------------------------------------------------------------

z <- read.table("CleanData/Bromus_Clean_V3_Oct-08-19.csv",sep=",",
                stringsAsFactors=FALSE, header=TRUE)
str(z)

