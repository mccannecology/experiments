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

data_area$id2 <- paste(data_area$id,data_area$species,sep="")
data_rgr$id2 <- paste(data_rgr$id,data_rgr$species,sep="")

data_area <- data_area[,-9]

data_comp_rel <- data_area[,-8]
data_comp_rel <- data_comp_rel[!(data_comp_rel$species == "TOT" | data_comp_rel$treatment=="LM" | data_comp_rel$treatment=="SP" | data_comp_rel$treatment=="WB"),]
data_comp_rel$id2 <- paste(data_comp_rel$id,data_comp_rel$species,sep="")

summary(data_area)
summary(data_rgr)
summary(data_comp_rel)

##################################
# Plot raw rgr data through time #
##################################
rgr_plot_raw <- ggplot(data_rgr, aes(x=day,y=rgr,group=id2,colour=species)) + geom_line() + geom_point() 
rgr_plot_raw <- rgr_plot_raw + facet_grid(nutrients ~ treatment)
rgr_plot_raw 

###################################
# Plot raw area data through time #
###################################
area_plot_raw <- ggplot(data_area, aes(x=day,y=area_mm2,group=id2,colour=species)) + geom_line() + geom_point() 
area_plot_raw <- area_plot_raw + facet_grid(nutrients ~ treatment)
area_plot_raw 

#######################################
# Plot raw % composition through time #
#######################################
comp_rel_plot_raw <- ggplot(data_comp_rel, aes(x=day, y=comp_rel, group=id2, colour=species)) + geom_line() + geom_point()
comp_rel_plot_raw <- comp_rel_plot_raw + facet_grid(nutrients ~ treatment)
comp_rel_plot_raw

#######################
# Means and variances #
#######################
summary_data_area <- ddply(data_area, c("nutrients","treatment","species","day"), summarise, 
                           N = length(area_mm2),
                           mean = mean(area_mm2),
                           sd = sd(area_mm2),
                           se = sd / sqrt(N) )

colnames(summary_data_area)[6] <- "area"

summary_data_comp_rel <- ddply(data_comp_rel, c("nutrients","treatment","species","day"), summarise, 
                           N = length(comp_rel),
                           mean = mean(comp_rel),
                           sd = sd(comp_rel),
                           se = sd / sqrt(N) )

colnames(summary_data_comp_rel)[6] <- "comp_rel"


summary_data_rgr <- ddply(data_rgr, c("nutrients","treatment","species","day"), summarise, 
                          N = length(rgr),
                          mean = mean(rgr),
                          sd = sd(rgr),
                          se = sd / sqrt(N) )

colnames(summary_data_rgr)[6] <- "rgr"

##############################
# Plot mean rgr through time #
##############################
rgr_plot_avg <- ggplot(summary_data_rgr, aes(x=day, y=rgr,colour=species)) + geom_errorbar(aes(ymin=rgr-se, ymax=rgr+se), width=0.1)
rgr_plot_avg <- rgr_plot_avg + geom_line() + geom_point()
rgr_plot_avg <- rgr_plot_avg + facet_grid(nutrients ~ treatment)
rgr_plot_avg

###############################
# Plot mean area through time #
###############################
area_plot_avg <- ggplot(summary_data_area, aes(x=day, y=area,colour=species)) + geom_errorbar(aes(ymin=area-se, ymax=area+se), width=0.1)
area_plot_avg <- area_plot_avg + geom_line() + geom_point()
area_plot_avg <- area_plot_avg + facet_grid(nutrients ~ treatment)
area_plot_avg

########################################
# Plot mean % composition through time #
########################################
comp_rel_plot <- ggplot(summary_data_comp_rel, aes(x=day, y=comp_rel, colour=species)) + geom_errorbar(aes(ymin=comp_rel-se, ymax=comp_rel+se), width=0.1)
comp_rel_plot <- comp_rel_plot + geom_line() + geom_point()
comp_rel_plot <- comp_rel_plot + facet_grid(nutrients ~ treatment)
comp_rel_plot

