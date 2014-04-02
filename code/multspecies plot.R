################################################
# Analysis of multispecies duckweed experiment #
#                                              #
# Plotting data                                #
################################################

library(ggplot2)
library(gridExtra)

##################################
# Plot raw rgr data through time #
##################################
# black & white 
rgr_plot_raw <- ggplot(data_rgr, aes(x=day,y=rgr,group=id2,shape=species)) + geom_line() + geom_point() 
rgr_plot_raw <- rgr_plot_raw + facet_grid(nutrients ~ treatment)
rgr_plot_raw <- rgr_plot_raw + ylab("RGR")
rgr_plot_raw <- rgr_plot_raw + scale_x_discrete(breaks=c(1,3,5,7,9),labels=c(1,3,5,7,9))
rgr_plot_raw  

# colour
rgr_plot_raw <- ggplot(data_rgr, aes(x=day,y=rgr,group=id2,colour=species)) + geom_line() + geom_point() 
rgr_plot_raw <- rgr_plot_raw + facet_grid(nutrients ~ treatment)
rgr_plot_raw <- rgr_plot_raw + ylab("RGR")
rgr_plot_raw <- rgr_plot_raw + scale_x_discrete(breaks=c(1,3,5,7,9),labels=c(1,3,5,7,9))
rgr_plot_raw 

###################################
# Plot raw area data through time #
###################################
# black & white 
area_plot_raw <- ggplot(data_area, aes(x=day,y=area_mm2,group=id2,shape=species)) + geom_line() + geom_point() 
area_plot_raw <- area_plot_raw + facet_grid(nutrients ~ treatment)
area_plot_raw <- area_plot_raw + scale_x_discrete(breaks=c(0,2,4,6,8,10),labels=c(0,2,4,6,8,10))
area_plot_raw <- area_plot_raw + ylab("area (sq. mm")
area_plot_raw 

# colour
area_plot_raw <- ggplot(data_area, aes(x=day,y=area_mm2,group=id2,colour=species)) + geom_line() + geom_point() 
area_plot_raw <- area_plot_raw + facet_grid(nutrients ~ treatment)
area_plot_raw <- area_plot_raw + scale_x_discrete(breaks=c(0,2,4,6,8,10),labels=c(0,2,4,6,8,10))
area_plot_raw <- area_plot_raw + ylab("area (sq. mm")
area_plot_raw 

#########################################
# Plot raw area_stand data through time #
#########################################
# black & white 
area_stand_plot_raw <- ggplot(data_area, aes(x=day,y=area_stand,group=id2,shape=species)) + geom_line() + geom_point() 
area_stand_plot_raw <- area_stand_plot_raw + facet_grid(nutrients ~ treatment)
area_stand_plot_raw <- area_stand_plot_raw + scale_x_discrete(breaks=c(0,2,4,6,8,10),labels=c(0,2,4,6,8,10))
area_stand_plot_raw <- area_stand_plot_raw + ylab("current area / initial area")
area_stand_plot_raw 

# colour
area_stand_plot_raw <- ggplot(data_area, aes(x=day,y=area_stand,group=id2,colour=species)) + geom_line() + geom_point() 
area_stand_plot_raw <- area_stand_plot_raw + facet_grid(nutrients ~ treatment)
area_stand_plot_raw <- area_stand_plot_raw + scale_x_discrete(breaks=c(0,2,4,6,8,10),labels=c(0,2,4,6,8,10))
area_stand_plot_raw <- area_stand_plot_raw + ylab("current area / initial area")
area_stand_plot_raw 

#######################################
# Plot raw % composition through time #
#######################################
# black & white 
comp_rel_plot_raw <- ggplot(data_area, aes(x=day, y=comp_rel, group=id2, shape=species)) + geom_line() + geom_point(size=3)
comp_rel_plot_raw <- comp_rel_plot_raw + facet_grid(nutrients ~ treatment)
comp_rel_plot_raw <- comp_rel_plot_raw + scale_x_discrete(breaks=c(0,2,4,6,8,10),labels=c(0,2,4,6,8,10))
comp_rel_plot_raw

# colour
comp_rel_plot_raw <- ggplot(data_area, aes(x=day, y=comp_rel, group=id2, colour=species)) + geom_line() + geom_point()
comp_rel_plot_raw <- comp_rel_plot_raw + facet_grid(nutrients ~ treatment)
comp_rel_plot_raw <- comp_rel_plot_raw + scale_x_discrete(breaks=c(0,2,4,6,8,10),labels=c(0,2,4,6,8,10))
comp_rel_plot_raw

##############################
# Plot mean rgr through time #
##############################
# black & white 
rgr_plot_avg <- ggplot(summary_data_rgr, aes(x=day, y=rgr,shape=species)) + geom_errorbar(aes(ymin=rgr-se, ymax=rgr+se), width=0.1)
rgr_plot_avg <- rgr_plot_avg + geom_line() + geom_point(size=3)
rgr_plot_avg <- rgr_plot_avg + facet_grid(nutrients ~ treatment)
rgr_plot_avg <- rgr_plot_avg + scale_x_discrete(breaks=c(1,3,5,7,9),labels=c(1,3,5,7,9))
rgr_plot_avg <- rgr_plot_avg + ylab("RGR")
rgr_plot_avg

# colour
rgr_plot_avg <- ggplot(summary_data_rgr, aes(x=day, y=rgr,colour=species)) + geom_errorbar(aes(ymin=rgr-se, ymax=rgr+se), width=0.1)
rgr_plot_avg <- rgr_plot_avg + geom_line() + geom_point()
rgr_plot_avg <- rgr_plot_avg + facet_grid(nutrients ~ treatment)
rgr_plot_avg <- rgr_plot_avg + scale_x_discrete(breaks=c(1,3,5,7,9),labels=c(1,3,5,7,9))
rgr_plot_avg <- rgr_plot_avg + ylab("RGR")
rgr_plot_avg

# colour w/ unqie y-axis scales for each facet 
rgr_plot_avg <- ggplot(summary_data_rgr, aes(x=day, y=rgr,colour=species)) + geom_errorbar(aes(ymin=rgr-se, ymax=rgr+se), width=0.1)
rgr_plot_avg <- rgr_plot_avg + geom_line() + geom_point()
rgr_plot_avg <- rgr_plot_avg + facet_grid(nutrients ~ treatment, scales="free_y")
rgr_plot_avg <- rgr_plot_avg + scale_x_discrete(breaks=c(1,3,5,7,9),labels=c(1,3,5,7,9))
rgr_plot_avg <- rgr_plot_avg + ylab("RGR")
rgr_plot_avg

####################################
# Plot mean rgr_stand through time #
####################################
# this response variable may be DUMB 
# why should I divided RGR by density on previous time step??? 

# black & white 
rgr_stand_plot_avg <- ggplot(summary_data_rgr_stand, aes(x=day, y=rgr_stand,shape=species)) + geom_errorbar(aes(ymin=rgr_stand-se, rgr_stand=rgr+se), width=0.1)
rgr_stand_plot_avg <- rgr_stand_plot_avg + geom_line() + geom_point(size=3)
rgr_stand_plot_avg <- rgr_stand_plot_avg + facet_grid(nutrients ~ treatment)
rgr_stand_plot_avg <- rgr_stand_plot_avg + scale_x_discrete(breaks=c(1,3,5,7,9),labels=c(1,3,5,7,9))
rgr_stand_plot_avg <- rgr_stand_plot_avg + ylab("RGR / density ")
rgr_stand_plot_avg

# colour
rgr_stand_plot_avg <- ggplot(summary_data_rgr_stand, aes(x=day, y=rgr_stand,colour=species)) + geom_errorbar(aes(ymin=rgr_stand-se, ymax=rgr_stand+se), width=0.1)
rgr_stand_plot_avg <- rgr_stand_plot_avg + geom_line() + geom_point()
rgr_stand_plot_avg <- rgr_stand_plot_avg + facet_grid(nutrients ~ treatment)
rgr_stand_plot_avg <- rgr_stand_plot_avg + scale_x_discrete(breaks=c(1,3,5,7,9),labels=c(1,3,5,7,9))
rgr_stand_plot_avg <- rgr_stand_plot_avg + ylab("RGR / density ")
rgr_stand_plot_avg

# colour w/ unqie y-axis scales for each facet 
rgr_stand_plot_avg <- ggplot(summary_data_rgr_stand, aes(x=day, y=rgr_stand,colour=species)) + geom_errorbar(aes(ymin=rgr_stand-se, ymax=rgr_stand+se), width=0.1)
rgr_stand_plot_avg <- rgr_stand_plot_avg + geom_line() + geom_point()
rgr_stand_plot_avg <- rgr_stand_plot_avg + facet_grid(nutrients ~ treatment, scales="free_y")
rgr_stand_plot_avg <- rgr_stand_plot_avg + scale_x_discrete(breaks=c(1,3,5,7,9),labels=c(1,3,5,7,9))
rgr_stand_plot_avg <- rgr_stand_plot_avg + ylab("RGR / density ")
rgr_stand_plot_avg

###############################
# Plot mean area through time #
###############################
# re-order my treatments so they go from monocultures to polycultures in alphabetical order 
summary_data_area$treatment <- factor(summary_data_area$treatment, levels=c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB"))

# black & white 
area_plot_avg <- ggplot(summary_data_area, aes(x=day, y=area,shape=species)) + geom_errorbar(aes(ymin=area-se, ymax=area+se), width=0.1)
area_plot_avg <- area_plot_avg + geom_line() + geom_point(size=3)
area_plot_avg <- area_plot_avg + facet_grid(nutrients ~ treatment)
area_plot_avg <- area_plot_avg + scale_x_discrete(breaks=c(0,2,4,6,8,10),labels=c(0,2,4,6,8,10))
area_plot_avg <- area_plot_avg + ylab("area (sq. mm)")
area_plot_avg

# colour
area_plot_avg <- ggplot(summary_data_area, aes(x=day, y=area,colour=species)) + geom_errorbar(aes(ymin=area-se, ymax=area+se), width=0.1)
area_plot_avg <- area_plot_avg + geom_line() + geom_point()
area_plot_avg <- area_plot_avg + facet_grid(nutrients ~ treatment)
area_plot_avg <- area_plot_avg + scale_x_discrete(breaks=c(0,2,4,6,8,10),labels=c(0,2,4,6,8,10))
area_plot_avg <- area_plot_avg + ylab("area (sq. mm)")
area_plot_avg

# colour w/ unqie y-axis scales for each facet 
area_plot_avg <- ggplot(summary_data_area, aes(x=day, y=area,colour=species)) + geom_errorbar(aes(ymin=area-se, ymax=area+se), width=0.1)
area_plot_avg <- area_plot_avg + geom_line() + geom_point()
area_plot_avg <- area_plot_avg + facet_grid(nutrients ~ treatment, scales="free_y")
area_plot_avg <- area_plot_avg + scale_x_discrete(breaks=c(0,2,4,6,8,10),labels=c(0,2,4,6,8,10))
area_plot_avg <- area_plot_avg + ylab("area (sq. mm)")
area_plot_avg

# flip the faceting 
# now scale free doesn't work 
area_plot_avg <- ggplot(summary_data_area, aes(x=day, y=area,colour=species)) + geom_errorbar(aes(ymin=area-se, ymax=area+se), width=0.1)
area_plot_avg <- area_plot_avg + geom_line() + geom_point()
area_plot_avg <- area_plot_avg + facet_grid(treatment ~ nutrients, scales="free")
area_plot_avg <- area_plot_avg + scale_x_discrete(breaks=c(0,2,4,6,8,10),labels=c(0,2,4,6,8,10))
area_plot_avg <- area_plot_avg + ylab("area (sq. mm)")
area_plot_avg


#####################################
# Plot mean area_stand through time #
# total species only                #
#####################################
# area_stand for each speicies is done wrong 
# I dividied by each species' initial area 
# I should have divied by the total initial plant area in the well

# re-order my treatments so they go from monocultures to polycultures in alphabetical order 
summary_data_area_stand$treatment <- factor(summary_data_area_stand$treatment, levels=c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB"))

# subset some data for this plot 
summary_data_area_stand_TOT <- subset(summary_data_area_stand, summary_data_area_stand$species=="TOT")

# colour
area_stand_plot_avg <- ggplot(summary_data_area_stand_TOT, aes(x=day, y=area_stand)) + geom_errorbar(aes(ymin=area_stand-se, ymax=area_stand+se), width=0.1)
area_stand_plot_avg <- area_stand_plot_avg + geom_line() + geom_point()
area_stand_plot_avg <- area_stand_plot_avg + facet_grid(nutrients ~ treatment, scales="free_y")
area_stand_plot_avg <- area_stand_plot_avg + scale_x_discrete(breaks=c(0,2,4,6,8,10),labels=c(0,2,4,6,8,10))
area_stand_plot_avg <- area_stand_plot_avg + ylab("current area / initial area")
area_stand_plot_avg

# black & white 
# still need to add 

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

##################################
# Plot mean area at day 10       #
# Total species only             #
##################################
# subset some data for this plot 
summary_data_area_10_TOT <- subset(summary_data_area, summary_data_area$day==10 & summary_data_area$species=="TOT")

# re-order my treatments so they go from monocultures to polycultures in alphabetical order 
summary_data_area_10_TOT$treatment <- factor(summary_data_area_10_TOT$treatment, levels=c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB"))

# colour
area_plot_10 <- ggplot(summary_data_area_10_TOT, aes(x=treatment, y=area,colour=factor(nutrients))) + geom_errorbar(aes(ymin=area-se, ymax=area+se), width=0.1)
area_plot_10 <- area_plot_10 + geom_point(size=3)
area_plot_10 <- area_plot_10 + ylab("final area (sq. mm)")
area_plot_10 <- area_plot_10 + xlab("species treatment")
area_plot_10 <- area_plot_10 + labs(colour="Nutrients")
area_plot_10

# black & white 
# still need to add 


##################################
# Plot mean area_stand at day 10 #
# Bar plot                       #
# each species stacked           #
##################################
# This plot tells me that I calculated area_stand for each species incorrectly
# in polyculture treatments the areas should be standardized by the total initial area of all specie in the well, not just that specific species

# subset some data for this plot 
summary_data_area_stand_10_noTOT <- subset(summary_data_area_stand, summary_data_area_stand$day==10 & summary_data_area_stand$species !="TOT")

# black & white 
# still need to add a B&W version 

# colour - error bars come out in weird locations (where the bar originally would have been before stacking)
limits <- aes(ymax = area_stand + se, ymin= area_stand - se)
area_stand_plot_10_species <- ggplot(summary_data_area_stand_10_noTOT, aes(x=treatment, y=area_stand, fill=species)) + geom_bar(stat="identity")
area_stand_plot_10_species <- area_stand_plot_10_species + geom_errorbar(aes(ymin=area_stand-se, ymax=area_stand+se), width=0.1)
area_stand_plot_10_species <- area_stand_plot_10_species + facet_grid(nutrients~.)
area_stand_plot_10_species <- area_stand_plot_10_species + ylab("final area / initial area")
area_stand_plot_10_species <- area_stand_plot_10_species + xlab("species treatment")
area_stand_plot_10_species

########################################
# Plot mean % composition through time #
########################################
# black & white 
comp_rel_plot <- ggplot(summary_data_comp_rel, aes(x=day, y=comp_rel, shape=species)) + geom_errorbar(aes(ymin=comp_rel-se, ymax=comp_rel+se), width=0.1)
comp_rel_plot <- comp_rel_plot + geom_line() + geom_point(size=3)
comp_rel_plot <- comp_rel_plot + facet_grid(nutrients ~ treatment)
comp_rel_plot <- comp_rel_plot + ylab("relative % composition")
comp_rel_plot <- comp_rel_plot + scale_x_discrete(breaks=c(0,2,4,6,8,10),labels=c(0,2,4,6,8,10))
comp_rel_plot

# colour
comp_rel_plot <- ggplot(summary_data_comp_rel, aes(x=day, y=comp_rel, colour=species)) + geom_errorbar(aes(ymin=comp_rel-se, ymax=comp_rel+se), width=0.1)
comp_rel_plot <- comp_rel_plot + geom_line() + geom_point()
comp_rel_plot <- comp_rel_plot + facet_grid(nutrients ~ treatment)
comp_rel_plot <- comp_rel_plot + ylab("relative % composition")
comp_rel_plot <- comp_rel_plot + scale_x_discrete(breaks=c(0,2,4,6,8,10),labels=c(0,2,4,6,8,10))
comp_rel_plot

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
Fig3temp <- arrangeGrob(maxrgr_plot_high,maxrgr_plot_low,ncol=1,nrow=2) 
Fig3temp

######################################################################################
# Plot average maximum growth rate - by treamtment - separate plots for each species #
######################################################################################
head(summary_data_maxrgr) 

# add post-hoc test labels to data frame (summary_data_maxrgr) for plotting posthoc test labels


# LEMNA
maxrgr_plot_LM <- ggplot(subset(summary_data_maxrgr, summary_data_maxrgr$species=="LM"), aes(x=treatment, y=maximum_RGR,shape=nutrients)) + geom_errorbar(aes(ymin=maximum_RGR-se, ymax=maximum_RGR+se), width=0.1)
maxrgr_plot_LM <- maxrgr_plot_LM + geom_point(size=3)
maxrgr_plot_LM <- maxrgr_plot_LM + ylab("maximum RGR (day 0-10)")
maxrgr_plot_LM <- maxrgr_plot_LM + ylim(0,0.4)
maxrgr_plot_LM <- maxrgr_plot_LM + ggtitle("Lemna minor")
maxrgr_plot_LM <- maxrgr_plot_LM + theme_classic()
maxrgr_plot_LM <- maxrgr_plot_LM + theme(legend.position = "none") 
maxrgr_plot_LM

# SPIRODELA
maxrgr_plot_SP <- ggplot(subset(summary_data_maxrgr, summary_data_maxrgr$species=="SP"), aes(x=treatment, y=maximum_RGR,shape=nutrients)) + geom_errorbar(aes(ymin=maximum_RGR-se, ymax=maximum_RGR+se), width=0.1)
maxrgr_plot_SP <- maxrgr_plot_SP + geom_point(size=3)
maxrgr_plot_SP <- maxrgr_plot_SP + ylab("maximum RGR (day 0-10)")
maxrgr_plot_SP <- maxrgr_plot_SP + ylim(0,0.4)
maxrgr_plot_SP <- maxrgr_plot_SP + ggtitle("Spirodela polyrhiza")
maxrgr_plot_SP <- maxrgr_plot_SP + theme_classic()
maxrgr_plot_SP <- maxrgr_plot_SP + theme(legend.position = "none") 
maxrgr_plot_SP 

# WOLLFIA
maxrgr_plot_WB <- ggplot(subset(summary_data_maxrgr, summary_data_maxrgr$species=="WB"), aes(x=treatment, y=maximum_RGR,shape=nutrients)) + geom_errorbar(aes(ymin=maximum_RGR-se, ymax=maximum_RGR+se), width=0.1)
maxrgr_plot_WB <- maxrgr_plot_WB + geom_point(size=3)
maxrgr_plot_WB <- maxrgr_plot_WB + ylab("maximum RGR (day 0-10)")
maxrgr_plot_WB <- maxrgr_plot_WB + ylim(0,0.4)
maxrgr_plot_WB <- maxrgr_plot_WB + ggtitle("Wolffia brasiliensis")
maxrgr_plot_WB <- maxrgr_plot_WB + theme_classic()
maxrgr_plot_WB 

Fig3temp <- arrangeGrob(maxrgr_plot_LM,maxrgr_plot_SP,maxrgr_plot_WB,ncol=3,nrow=1) 
Fig3temp

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

# LEMNA
avgrgr_plot_LM <- ggplot(subset(summary_data_meanrgr, summary_data_meanrgr$species=="LM"), aes(x=treatment, y=average_RGR, shape=nutrients)) + geom_errorbar(aes(ymin=average_RGR-se, ymax=average_RGR+se), width=0.1)
avgrgr_plot_LM <- avgrgr_plot_LM + geom_point(size=3)
avgrgr_plot_LM <- avgrgr_plot_LM + ylim(-0.05,0.25)
avgrgr_plot_LM <- avgrgr_plot_LM + ylab("average RGR (day 0-10)")
avgrgr_plot_LM <- avgrgr_plot_LM + ggtitle("Lemna minor")
avgrgr_plot_LM <- avgrgr_plot_LM + theme(legend.position = "none") 
avgrgr_plot_LM 

# SPIRODELA
avgrgr_plot_SP <- ggplot(subset(summary_data_meanrgr, summary_data_meanrgr$species=="SP"), aes(x=treatment, y=average_RGR, shape=nutrients)) + geom_errorbar(aes(ymin=average_RGR-se, ymax=average_RGR+se), width=0.1)
avgrgr_plot_SP <- avgrgr_plot_SP + geom_point(size=3)
avgrgr_plot_SP <- avgrgr_plot_SP + ylim(-0.05,0.25)
avgrgr_plot_SP <- avgrgr_plot_SP + ylab("average RGR (day 0-10)")
avgrgr_plot_SP <- avgrgr_plot_SP + ggtitle("Spirodela polyrhiza")
avgrgr_plot_SP <- avgrgr_plot_SP + theme(legend.position = "none") 
avgrgr_plot_SP 

# WOLLFIA
avgrgr_plot_WB <- ggplot(subset(summary_data_meanrgr, summary_data_meanrgr$species=="WB"), aes(x=treatment, y=average_RGR, shape=nutrients)) + geom_errorbar(aes(ymin=average_RGR-se, ymax=average_RGR+se), width=0.1)
avgrgr_plot_WB <- avgrgr_plot_WB + geom_point(size=3)
avgrgr_plot_WB <- avgrgr_plot_WB + ylim(-0.05,0.25)
avgrgr_plot_WB <- avgrgr_plot_WB + ylab("average RGR (day 0-10)")
avgrgr_plot_WB <- avgrgr_plot_WB + ggtitle("Wolffia brasiliensis")
avgrgr_plot_WB 

# combine thwo plots into one
Fig2temp <- arrangeGrob(avgrgr_plot_LM,avgrgr_plot_SP,avgrgr_plot_WB,ncol=3,nrow=1) 
Fig2temp

#####################################################################
# Plot standardized average growth rate - by species - by treatment #
#####################################################################
# this response variable may be DUMB 
# why should I divided RGR by density on previous time step??? 

limits <- aes(ymax = rgr_stand + se, ymin= rgr_stand - se)
dodge <- position_dodge(width=0.9)
avgrgr_stand_plot <- ggplot(subset(summary_data_avg_rgr_stand, summary_data_avg_rgr_stand$species != "TOT"), aes(x=species, y=rgr_stand, fill=species)) 
avgrgr_stand_plot <- avgrgr_stand_plot + geom_bar(position="dodge",stat="identity")
avgrgr_stand_plot <- avgrgr_stand_plot + facet_grid(nutrients ~ treatment)
avgrgr_stand_plot <- avgrgr_stand_plot + geom_bar(position=dodge) + geom_errorbar(limits, position=dodge, width=0.25)
avgrgr_stand_plot <- avgrgr_stand_plot + ylab("standardized RGR")
avgrgr_stand_plot

##################################
# Plot species area on each axis #
##################################
# Lemna & Spirodela - w/ unqie axis scales for each facet & a line at slope=1, intercetp=0
area_area_LMSP_plot <- ggplot(subset(data_area_area, data_area_area$treatment=="LMSP"), aes(x=LM,y=SP,group=id)) + geom_point() + geom_line()
area_area_LMSP_plot <- area_area_LMSP_plot + facet_grid(.~nutrients, scales="free")
area_area_LMSP_plot <- area_area_LMSP_plot + xlab("area LM (sq.mm)") + ylab("area SP (sq.mm)")
area_area_LMSP_plot <- area_area_LMSP_plot + geom_abline(intercept=0,slope=1,colour="red",linetype="dashed")
area_area_LMSP_plot

# Lemna & Spirodela 
area_area_LMSP_plot <- ggplot(subset(data_area_area, data_area_area$treatment=="LMSP"), aes(x=LM,y=SP,group=id)) + geom_point() + geom_line()
area_area_LMSP_plot <- area_area_LMSP_plot + facet_grid(.~nutrients)
area_area_LMSP_plot <- area_area_LMSP_plot + xlab("area LM (sq.mm)") + ylab("area SP (sq.mm)")
area_area_LMSP_plot

# Lemna & Spirodela - just low nutrients 
area_area_LMSP_plot_low <- ggplot(subset(data_area_area, data_area_area$treatment=="LMSP" & data_area_area$nutrients=="low"), aes(x=LM,y=SP,group=id)) 
area_area_LMSP_plot_low <- area_area_LMSP_plot_low + geom_point() + geom_line()
area_area_LMSP_plot_low <- area_area_LMSP_plot_low + xlab("area LM (sq.mm)") + ylab("area SP (sq.mm)")
area_area_LMSP_plot_low

# Lemna & Wolffia - w/ unqie axis scales for each facet & a line at slope=1, intercetp=0
area_area_LMWB_plot <- ggplot(subset(data_area_area, data_area_area$treatment=="LMWB"), aes(x=LM,y=WB,group=id)) + geom_point() + geom_line()
area_area_LMWB_plot <- area_area_LMWB_plot + facet_grid(.~nutrients, scales="free")
area_area_LMWB_plot <- area_area_LMWB_plot + xlab("area LM (sq.mm)") + ylab("area WB (sq.mm)")
area_area_LMWB_plot <- area_area_LMWB_plot + geom_abline(intercept=0,slope=1,colour="red",linetype="dashed")
area_area_LMWB_plot

# Lemna & Wolffia 
area_area_LMWB_plot <- ggplot(subset(data_area_area, data_area_area$treatment=="LMWB"), aes(x=LM,y=WB,group=id)) + geom_point() + geom_line()
area_area_LMWB_plot <- area_area_LMWB_plot + facet_grid(.~nutrients)
area_area_LMWB_plot <- area_area_LMWB_plot + xlab("area LM (sq.mm)") + ylab("area WB (sq.mm)")
area_area_LMWB_plot

# Lemna & Spirodela - just low nutrients 
area_area_LMWB_plot_low <- ggplot(subset(data_area_area, data_area_area$treatment=="LMWB" & data_area_area$nutrients=="low"), aes(x=LM,y=WB,group=id)) 
area_area_LMWB_plot_low <- area_area_LMWB_plot_low + geom_point() + geom_line()
area_area_LMWB_plot_low <- area_area_LMWB_plot_low + xlab("area LM (sq.mm)") + ylab("area WB (sq.mm)")
area_area_LMWB_plot_low

# Spirodela & Wolffia - w/ unqie axis scales for each facet & a line at slope=1, intercetp=0 
area_area_SPWB_plot <- ggplot(subset(data_area_area, data_area_area$treatment=="SPWB"), aes(x=SP,y=WB,group=id)) + geom_point() + geom_line()
area_area_SPWB_plot <- area_area_SPWB_plot + facet_grid(.~nutrients, scales="free")
area_area_SPWB_plot <- area_area_SPWB_plot + xlab("area SP (sq.mm)") + ylab("area WB (sq.mm)")
area_area_SPWB_plot <- area_area_SPWB_plot + geom_abline(intercept=0,slope=1,colour="red",linetype="dashed")
area_area_SPWB_plot 

# Spirodela & Wolffia 
area_area_SPWB_plot <- ggplot(subset(data_area_area, data_area_area$treatment=="SPWB"), aes(x=SP,y=WB,group=id)) + geom_point() + geom_line()
area_area_SPWB_plot <- area_area_SPWB_plot + facet_grid(.~nutrients)
area_area_SPWB_plot <- area_area_SPWB_plot + xlab("area SP (sq.mm)") + ylab("area WB (sq.mm)")
area_area_SPWB_plot

# Spirodela & Wolffia  - just low nutrients 
area_area_SPWB_plot_low <- ggplot(subset(data_area_area, data_area_area$treatment=="SPWB" & data_area_area$nutrients=="low"), aes(x=SP,y=WB,group=id)) 
area_area_SPWB_plot_low <- area_area_SPWB_plot_low + geom_point() + geom_line()
area_area_SPWB_plot_low <- area_area_SPWB_plot_low + xlab("area SP (sq.mm)") + ylab("area WB (sq.mm)")
area_area_SPWB_plot_low

# Lemna & Spirodela & Wolffia - w/ unqie axis scales for each facet & a line at slope=1, intercetp=0  
# use color to display 3rd variable - WB
area_area_LMSPWB_plot <- ggplot(subset(data_area_area, data_area_area$treatment=="LMSPWB"), aes(x=LM,y=SP,group=id,colour=WB)) + geom_point() + geom_line()
area_area_LMSPWB_plot <- area_area_LMSPWB_plot + facet_grid(.~nutrients, scales="free")
area_area_LMSPWB_plot <- area_area_LMSPWB_plot + xlab("area LM (sq.mm)") + ylab("area SP (sq.mm)")
area_area_LMSPWB_plot <- area_area_LMSPWB_plot + geom_abline(intercept=0,slope=1,colour="red",linetype="dashed")
area_area_LMSPWB_plot

# Lemna & Spirodela & Wolffia 
# use color to display 3rd variable - WB
area_area_LMSPWB_plot <- ggplot(subset(data_area_area, data_area_area$treatment=="LMSPWB"), aes(x=LM,y=SP,group=id,colour=WB)) + geom_point() + geom_line()
area_area_LMSPWB_plot <- area_area_LMSPWB_plot + facet_grid(.~nutrients)
area_area_LMSPWB_plot <- area_area_LMSPWB_plot + xlab("area LM (sq.mm)") + ylab("area SP (sq.mm)")
area_area_LMSPWB_plot

# Lemna & Spirodela & Wolffia - just low nutrients 
# use color to display 3rd variable - WB
area_area_LMSPWB_plot_low <- ggplot(subset(data_area_area, data_area_area$treatment=="LMSPWB"& data_area_area$nutrients=="low"), aes(x=LM,y=SP,group=id,colour=WB)) 
area_area_LMSPWB_plot_low <- area_area_LMSPWB_plot_low + geom_point() + geom_line()
area_area_LMSPWB_plot_low <- area_area_LMSPWB_plot_low + facet_grid(.~nutrients)
area_area_LMSPWB_plot_low <- area_area_LMSPWB_plot_low + xlab("area LM (sq.mm)") + ylab("area SP (sq.mm)")
area_area_LMSPWB_plot_low
