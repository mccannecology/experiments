################################################
# Analysis of multispecies duckweed experiment #
#                                              #
# Plotting data                                #
# RGR, maximum by species                      #
################################################

library(ggplot2)
library(gridExtra)

######################################################################################
# Plot average maximum growth rate - by treamtment - separate plots for each species #
######################################################################################
head(summary_data_maxrgr) 

# add post-hoc test labels to data frame (summary_data_maxrgr) for plotting posthoc test labels
# import posthoc label data 
rgr_max_posthoc <- read.csv("rgr_max_posthoc.csv") 
# sort both data frames first
summary_data_maxrgr <- summary_data_maxrgr[order(summary_data_maxrgr$maximum_RGR),]
rgr_max_posthoc <- rgr_max_posthoc[order(rgr_max_posthoc$maximum_RGR),]
# add the labels column
summary_data_maxrgr$label <- rgr_max_posthoc$label

# LEMNA
maxrgr_plot_LM <- ggplot(subset(summary_data_maxrgr, summary_data_maxrgr$species=="LM"), aes(x=treatment, y=maximum_RGR,shape=nutrients)) + geom_errorbar(aes(ymin=maximum_RGR-se, ymax=maximum_RGR+se), width=0.1)
maxrgr_plot_LM <- maxrgr_plot_LM + geom_point(size=3)
maxrgr_plot_LM <- maxrgr_plot_LM + ylab("maximum RGR (day 0-10)")
maxrgr_plot_LM <- maxrgr_plot_LM + ylim(0,0.4)
maxrgr_plot_LM <- maxrgr_plot_LM + ggtitle("Lemna minor")
maxrgr_plot_LM <- maxrgr_plot_LM + theme_classic(base_size=18)
maxrgr_plot_LM <- maxrgr_plot_LM + theme(legend.position = "none") 
maxrgr_plot_LM <- maxrgr_plot_LM + geom_text(data=subset(summary_data_maxrgr, summary_data_maxrgr$species=="LM"),aes(x=treatment, y=maximum_RGR+se+0.015,label=label))
maxrgr_plot_LM

# SPIRODELA
maxrgr_plot_SP <- ggplot(subset(summary_data_maxrgr, summary_data_maxrgr$species=="SP"), aes(x=treatment, y=maximum_RGR,shape=nutrients)) + geom_errorbar(aes(ymin=maximum_RGR-se, ymax=maximum_RGR+se), width=0.1)
maxrgr_plot_SP <- maxrgr_plot_SP + geom_point(size=3)
maxrgr_plot_SP <- maxrgr_plot_SP + ylab("maximum RGR (day 0-10)")
maxrgr_plot_SP <- maxrgr_plot_SP + ylim(0,0.4)
maxrgr_plot_SP <- maxrgr_plot_SP + ggtitle("Spirodela polyrhiza")
maxrgr_plot_SP <- maxrgr_plot_SP + theme_classic(base_size=18)
maxrgr_plot_SP <- maxrgr_plot_SP + theme(legend.position = "none") 
maxrgr_plot_SP <- maxrgr_plot_SP + geom_text(data=subset(summary_data_maxrgr, summary_data_maxrgr$species=="SP"),aes(x=treatment, y=maximum_RGR+se+0.015,label=label))
maxrgr_plot_SP 

# WOLLFIA
maxrgr_plot_WB <- ggplot(subset(summary_data_maxrgr, summary_data_maxrgr$species=="WB"), aes(x=treatment, y=maximum_RGR,shape=nutrients)) + geom_errorbar(aes(ymin=maximum_RGR-se, ymax=maximum_RGR+se), width=0.1)
maxrgr_plot_WB <- maxrgr_plot_WB + geom_point(size=3)
maxrgr_plot_WB <- maxrgr_plot_WB + ylab("maximum RGR (day 0-10)")
maxrgr_plot_WB <- maxrgr_plot_WB + ylim(0,0.4)
maxrgr_plot_WB <- maxrgr_plot_WB + ggtitle("Wolffia brasiliensis")
maxrgr_plot_WB <- maxrgr_plot_WB + theme_classic(base_size=18)
maxrgr_plot_WB <- maxrgr_plot_WB + geom_text(data=subset(summary_data_maxrgr, summary_data_maxrgr$species=="WB"),aes(x=treatment, y=maximum_RGR+se+0.015,label=label))
maxrgr_plot_WB 

Fig3temp <- arrangeGrob(maxrgr_plot_LM,maxrgr_plot_SP,maxrgr_plot_WB,ncol=3,nrow=1) 
Fig3temp

ggsave(file="Figure 03.pdf", Fig3temp, height=8,width=16)
ggsave(file="Figure 03.png", Fig3temp, height=8,width=16)

#################################################################
# Plot average maximum growth rate - by species - by treamtment #
#################################################################
# re-order my treatments so they go from monocultures to polycultures in alphabetical order 
summary_data_maxrgr$treatment <- factor(summary_data_maxrgr$treatment, levels=c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB"))

# black & white 
limits <- aes(ymax = maximum_RGR + se, ymin = maximum_RGR - se)
dodge <- position_dodge(width=0.9)
maxrgr_plot <- ggplot(summary_data_maxrgr, aes(x=species, y=maximum_RGR, fill=species)) + geom_bar(position="dodge",stat="identity")
maxrgr_plot <- maxrgr_plot + facet_grid(nutrients ~ treatment, scales="free_y")
maxrgr_plot <- maxrgr_plot + geom_bar(position=dodge) + geom_errorbar(limits, position=dodge, width=0.25)
maxrgr_plot <- maxrgr_plot + ylab("maximum RGR")
maxrgr_plot <- maxrgr_plot + scale_fill_grey()
maxrgr_plot

# colour
limits <- aes(ymax = maximum_RGR + se, ymin = maximum_RGR - se)
dodge <- position_dodge(width=0.9)
maxrgr_plot <- ggplot(summary_data_maxrgr, aes(x=species, y=maximum_RGR, fill=species)) + geom_bar(position="dodge",stat="identity")
maxrgr_plot <- maxrgr_plot + facet_grid(nutrients ~ treatment)
maxrgr_plot <- maxrgr_plot + geom_bar(position=dodge) + geom_errorbar(limits, position=dodge, width=0.25)
maxrgr_plot <- maxrgr_plot + ylab("maximum RGR")
maxrgr_plot

### make low & high nutrient plots separately, then arrange (this will make plots look nicer)
# just high nutrients 
limits <- aes(ymax = maximum_RGR + se, ymin = maximum_RGR - se)
dodge <- position_dodge(width=0.9)
maxrgr_plot_high <- ggplot(subset(summary_data_maxrgr, summary_data_maxrgr$nutrients=="high"), aes(x=species, y=maximum_RGR, fill=species)) + geom_bar(position="dodge",stat="identity")
maxrgr_plot_high <- maxrgr_plot_high + facet_grid(. ~ treatment)
maxrgr_plot_high <- maxrgr_plot_high + geom_bar(position=dodge) + geom_errorbar(limits, position=dodge, width=0.25)
maxrgr_plot_high <- maxrgr_plot_high + ylab("maximum RGR")
maxrgr_plot_high <- maxrgr_plot_high + scale_fill_grey()
maxrgr_plot_high <- maxrgr_plot_high + theme_bw()
maxrgr_plot_high <- maxrgr_plot_high + theme(legend.position = "none") 
maxrgr_plot_high

# just low nutrients 
limits <- aes(ymax = maximum_RGR + se, ymin = maximum_RGR - se)
dodge <- position_dodge(width=0.9)
maxrgr_plot_low <- ggplot(subset(summary_data_maxrgr, summary_data_maxrgr$nutrients=="low"), aes(x=species, y=maximum_RGR, fill=species)) + geom_bar(position="dodge",stat="identity")
maxrgr_plot_low <- maxrgr_plot_low  + facet_grid(. ~ treatment)
maxrgr_plot_low <- maxrgr_plot_low + geom_bar(position=dodge) + geom_errorbar(limits, position=dodge, width=0.25)
maxrgr_plot_low <- maxrgr_plot_low + ylab("maximum RGR")
maxrgr_plot_low <- maxrgr_plot_low + scale_fill_grey()
maxrgr_plot_low <- maxrgr_plot_low + theme_bw()
maxrgr_plot_low <- maxrgr_plot_low + theme(legend.position = "none") 
maxrgr_plot_low

# combine thwo plots into one
Fig3alt <- arrangeGrob(maxrgr_plot_high,maxrgr_plot_low,ncol=1,nrow=2) 
Fig3alt

