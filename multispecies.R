################################################
# Analysis of multispecies duckweed experiment #
################################################
library(ggplot2)
library(plyr)

###################################### 
# Importing & cleaning up data frame #
######################################
data_area <- read.csv("multispecies_area.csv") # import area data 
data_rgr <- read.csv("multispecies_rgr.csv") # import relative growth rate data 

data_area <- data_area[complete.cases(data_area),]
data_rgr <- data_rgr[complete.cases(data_rgr),]

summary(data_area)
summary(data_rgr)

#######################
# Means and variances #
#######################


##################################
# Plot raw rgr data through time #
##################################
rgr_plot_raw <- ggplot(data_rgr, aes(x=day,y=rgr,colour=species)) + geom_point() 
rgr_plot_raw <- rgr_plot_raw + facet_grid(nutrients ~ treatment)
rgr_plot_raw 

###################################
# Plot raw area data through time #
##################################
area_plot_raw <- ggplot(data_area, aes(x=day,y=area_mm2,colour=species)) + geom_point() 
area_plot_raw <- area_plot_raw + facet_grid(nutrients ~ treatment)
area_plot_raw 
