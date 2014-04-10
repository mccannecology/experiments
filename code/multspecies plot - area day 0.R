################################################
# Analysis of multispecies duckweed experiment #
#                                              #
# Plotting data                                #
# Day 0 area                                   # 
################################################
library(ggplot2)
library(gridExtra)

##################################
# Plot mean area_stand at day 10 #
# Total species only             #
##################################
# subset some data for this plot 
summary_data_area_day_0<- subset(summary_data_area, summary_data_area$day==0 & summary_data_area_stand$species=="TOT")

# Still needs to be modified for area day 0 
# add post-hoc test labels to data frame (summary_data_maxrgr) for plotting posthoc test labels
# import posthoc label data 
# area_stand_10_posthoc <- read.csv("area_stand_posthoc.csv") 
# sort both data frames first
# summary_data_area_stand_10_TOT <- summary_data_area_stand_10_TOT[order(summary_data_area_stand_10_TOT$area_stand),]
# area_stand_10_posthoc <- area_stand_10_posthoc[order(area_stand_10_posthoc$area_stand),]
# add the labels column
# summary_data_area_stand_10_TOT$label <- area_stand_10_posthoc$label

# re-order my treatments so they go from monocultures to polycultures in alphabetical order 
summary_data_area_day_0$treatment <- factor(summary_data_area_day_0$treatment, levels=c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB"))

# colour
area_plot_0 <- ggplot(summary_data_area_day_0, aes(x=treatment, y=area,colour=factor(nutrients))) + geom_errorbar(aes(ymin=area-se, ymax=area+se), width=0.1)
area_plot_0 <- area_plot_0 + geom_point(size=3)
area_plot_0 <- area_plot_0 + ylab("initial area (sq. mm)")
area_plot_0 <- area_plot_0 + xlab("species treatment")
area_plot_0 <- area_plot_0 + labs(colour="Nutrients")
# area_plot_0 <- area_plot_0 + geom_text(data=summary_data_area_stand_10_TOT,aes(x=treatment, y=area_stand+se+0.5,label=label))
area_plot_0

# black & white 
area_plot_0 <- ggplot(summary_data_area_day_0, aes(x=treatment, y=area,shape=nutrients)) + geom_errorbar(aes(ymin=area-se, ymax=area+se), width=0.1)
area_plot_0 <- area_plot_0 + geom_point(size=3)
area_plot_0 <- area_plot_0 + ylab("initial area (sq. mm)")
area_plot_0 <- area_plot_0 + xlab("Species treatment")
area_plot_0 <- area_plot_0 + theme_classic(base_size=18)
# area_plot_0 <- area_plot_0 + geom_text(data=summary_data_area_stand_10_TOT,aes(x=treatment, y=area_stand+se+0.5,label=label))
area_plot_0

ggsave(file="area_plot_0.pdf", area_plot_0, height=8,width=11)
ggsave(file="area_plot_0.jpg", area_plot_0, height=8,width=11)
