################################################
# Analysis of multispecies duckweed experiment #
################################################


############# 
# Importing #
#############

data <- read.csv("multispecies data 03-16-2014.csv",header=F) # import data matrix - do not include a header row 

library(reshape)

newdata <- melt(mydata, id=...)