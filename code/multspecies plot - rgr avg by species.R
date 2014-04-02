################################################
# Analysis of multispecies duckweed experiment #
#                                              #
# Plotting data                                #
# RGR, average by species                      #
################################################

library(ggplot2)
library(gridExtra)

################################################################
# Plot average average growth rate - by species - by treatment #
################################################################
# re-order my treatments so they go from monocultures to polycultures in alphabetical order 
summary_data_meanrgr$treatment <- factor(summary_data_meanrgr$treatment, levels=c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB"))

# black & white 
limits <- aes(ymax = average_RGR + se, ymin= average_RGR - se)
dodge <- position_dodge(width=0.9)
avgrgr_plot <- ggplot(summary_data_meanrgr, aes(x=species, y=average_RGR, fill=species)) + geom_bar(position="dodge",stat="identity")
avgrgr_plot <- avgrgr_plot + facet_grid(nutrients ~ treatment, scales="free_y")
avgrgr_plot <- avgrgr_plot + geom_bar(position=dodge) + geom_errorbar(limits, position=dodge, width=0.25)
avgrgr_plot <- avgrgr_plot + ylab("average RGR")
avgrgr_plot <- avgrgr_plot + scale_fill_grey() 
avgrgr_plot 

# colour
limits <- aes(ymax = average_RGR + se, ymin= average_RGR - se)
dodge <- position_dodge(width=0.9)
avgrgr_plot <- ggplot(summary_data_meanrgr, aes(x=species, y=average_RGR, fill=species)) + geom_bar(position="dodge",stat="identity")
avgrgr_plot <- avgrgr_plot + facet_grid(nutrients ~ treatment)
avgrgr_plot <- avgrgr_plot + geom_bar(position=dodge) + geom_errorbar(limits, position=dodge, width=0.25)
avgrgr_plot <- avgrgr_plot + ylab("average RGR")
avgrgr_plot

### make low & high nutrient plots separately, then arrange (this will make plots look nicer)
# just high nutrients 
limits <- aes(ymax = average_RGR + se, ymin = average_RGR - se)
dodge <- position_dodge(width=0.9)
avgrgr_plot_high <- ggplot(subset(summary_data_meanrgr, summary_data_meanrgr$nutrients=="high"), aes(x=species, y=average_RGR, fill=species)) + geom_bar(position="dodge",stat="identity")
avgrgr_plot_high <- avgrgr_plot_high + facet_grid(. ~ treatment)
avgrgr_plot_high <- avgrgr_plot_high + geom_bar(position=dodge) + geom_errorbar(limits, position=dodge, width=0.25)
avgrgr_plot_high <- avgrgr_plot_high + ylab("average RGR")
avgrgr_plot_high <- avgrgr_plot_high + scale_fill_grey()
avgrgr_plot_high <- avgrgr_plot_high + theme_bw()
avgrgr_plot_high <- avgrgr_plot_high + theme(legend.position = "none") 
avgrgr_plot_high 

# just low nutrients 
limits <- aes(ymax = average_RGR + se, ymin = average_RGR - se)
dodge <- position_dodge(width=0.9)
avgrgr_plot_low <- ggplot(subset(summary_data_meanrgr, summary_data_meanrgr$nutrients=="low"), aes(x=species, y=average_RGR, fill=species)) + geom_bar(position="dodge",stat="identity")
avgrgr_plot_low <- avgrgr_plot_low  + facet_grid(. ~ treatment)
avgrgr_plot_low <- avgrgr_plot_low + geom_bar(position=dodge) + geom_errorbar(limits, position=dodge, width=0.25)
avgrgr_plot_low <- avgrgr_plot_low + ylab("average RGR")
avgrgr_plot_low <- avgrgr_plot_low + scale_fill_grey()
avgrgr_plot_low <- avgrgr_plot_low + theme_bw()
avgrgr_plot_low <- avgrgr_plot_low + theme(legend.position = "none") 
avgrgr_plot_low

# combine thwo plots into one
Fig2temp <- arrangeGrob(avgrgr_plot_high,avgrgr_plot_low,ncol=1,nrow=2) 
Fig2temp

######################################################################################
# Plot average average growth rate - by treamtment - separate plots for each species #
######################################################################################
head(summary_data_meanrgr) 

# add post-hoc test labels to data frame (summary_data_maxrgr) for plotting posthoc test labels
# import posthoc label data 
rgr_avg_posthoc <- read.csv("rgr_avg_posthoc.csv") 
# sort both data frames first
summary_data_meanrgr <- summary_data_meanrgr[order(summary_data_meanrgr$average_RGR),]
rgr_avg_posthoc <- rgr_avg_posthoc[order(rgr_avg_posthoc$maximum_RGR),]
# add the labels column
summary_data_meanrgr$label <- rgr_avg_posthoc$label

# LEMNA
avgrgr_plot_LM <- ggplot(subset(summary_data_meanrgr, summary_data_meanrgr$species=="LM"), aes(x=treatment, y=average_RGR, shape=nutrients)) + geom_errorbar(aes(ymin=average_RGR-se, ymax=average_RGR+se), width=0.1)
avgrgr_plot_LM <- avgrgr_plot_LM + geom_point(size=3)
avgrgr_plot_LM <- avgrgr_plot_LM + ylim(-0.05,0.25)
avgrgr_plot_LM <- avgrgr_plot_LM + ylab("average RGR (day 0-10)")
avgrgr_plot_LM <- avgrgr_plot_LM + ggtitle("Lemna minor")
avgrgr_plot_LM <- avgrgr_plot_LM + theme_classic()
avgrgr_plot_LM <- avgrgr_plot_LM + theme(legend.position = "none") 
avgrgr_plot_LM <- avgrgr_plot_LM + geom_text(data=subset(summary_data_meanrgr, summary_data_meanrgr$species=="LM"),aes(x=treatment, y=average_RGR,label=label),hjust=2)
avgrgr_plot_LM 

# SPIRODELA
avgrgr_plot_SP <- ggplot(subset(summary_data_meanrgr, summary_data_meanrgr$species=="SP"), aes(x=treatment, y=average_RGR, shape=nutrients)) + geom_errorbar(aes(ymin=average_RGR-se, ymax=average_RGR+se), width=0.1)
avgrgr_plot_SP <- avgrgr_plot_SP + geom_point(size=3)
avgrgr_plot_SP <- avgrgr_plot_SP + ylim(-0.05,0.25)
avgrgr_plot_SP <- avgrgr_plot_SP + ylab("average RGR (day 0-10)")
avgrgr_plot_SP <- avgrgr_plot_SP + ggtitle("Spirodela polyrhiza")
avgrgr_plot_SP <- avgrgr_plot_SP + theme_classic()
avgrgr_plot_SP <- avgrgr_plot_SP + theme(legend.position = "none") 
avgrgr_plot_SP <- avgrgr_plot_SP + geom_text(data=subset(summary_data_meanrgr, summary_data_meanrgr$species=="SP"),aes(x=treatment, y=average_RGR,label=label),hjust=2)
avgrgr_plot_SP 

# WOLLFIA
avgrgr_plot_WB <- ggplot(subset(summary_data_meanrgr, summary_data_meanrgr$species=="WB"), aes(x=treatment, y=average_RGR, shape=nutrients)) + geom_errorbar(aes(ymin=average_RGR-se, ymax=average_RGR+se), width=0.1)
avgrgr_plot_WB <- avgrgr_plot_WB + geom_point(size=3)
avgrgr_plot_WB <- avgrgr_plot_WB + ylim(-0.05,0.25)
avgrgr_plot_WB <- avgrgr_plot_WB + ylab("average RGR (day 0-10)")
avgrgr_plot_WB <- avgrgr_plot_WB + ggtitle("Wolffia brasiliensis")
avgrgr_plot_WB <- avgrgr_plot_WB + theme_classic()
avgrgr_plot_WB <- avgrgr_plot_WB + geom_text(data=subset(summary_data_meanrgr, summary_data_meanrgr$species=="WB"),aes(x=treatment, y=average_RGR,label=label),hjust=2)
avgrgr_plot_WB 

# combine the plots into one
Fig2temp <- arrangeGrob(avgrgr_plot_LM,avgrgr_plot_SP,avgrgr_plot_WB,ncol=3,nrow=1) 
Fig2temp
