################################################
# Analysis of multispecies duckweed experiment #
#                                              #
# Plotting data                                #
# Day 10 area (standardized by initial area)   # 
################################################

library(ggplot2)
library(gridExtra)

##################################
# Plot mean area_stand at day 10 #
# Total species only             #
##################################
# subset some data for this plot 
summary_data_area_stand_10_TOT <- subset(summary_data_area_stand, summary_data_area_stand$day==10 & summary_data_area_stand$species=="TOT")

# re-order my treatments so they go from monocultures to polycultures in alphabetical order 
summary_data_area_stand_10_TOT$treatment <- factor(summary_data_area_stand_10_TOT$treatment, levels=c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB"))

# colour
area_stand_plot_10 <- ggplot(summary_data_area_stand_10_TOT, aes(x=treatment, y=area_stand,colour=factor(nutrients))) + geom_errorbar(aes(ymin=area_stand-se, ymax=area_stand+se), width=0.1)
area_stand_plot_10 <- area_stand_plot_10 + geom_point(size=3)
area_stand_plot_10 <- area_stand_plot_10 + ylab("final area / initial area")
area_stand_plot_10 <- area_stand_plot_10 + xlab("species treatment")
area_stand_plot_10 <- area_stand_plot_10 + labs(colour="Nutrients")
area_stand_plot_10

# black & white 
area_stand_plot_10 <- ggplot(summary_data_area_stand_10_TOT, aes(x=treatment, y=area_stand,shape=factor(nutrients))) + geom_errorbar(aes(ymin=area_stand-se, ymax=area_stand+se), width=0.1)
area_stand_plot_10 <- area_stand_plot_10 + geom_point(size=3)
area_stand_plot_10 <- area_stand_plot_10 + ylab("Final area / Initial area")
area_stand_plot_10 <- area_stand_plot_10 + xlab("Species treatment")
area_stand_plot_10 <- area_stand_plot_10 + theme_classic(base_size=18)
area_stand_plot_10 <- area_stand_plot_10 + labs(colour="Nutrients")
area_stand_plot_10


