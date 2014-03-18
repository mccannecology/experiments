################################################
# Analysis of multispecies duckweed experiment #
################################################
library(ggplot2)
library(plyr)
library(gridExtra)

###################################### 
# Importing & cleaning up data frame #
######################################
# make sure you remove any empty header rows from the .csv file 

data_area <- read.csv("multispecies_area.csv") # import area data 
data_rgr <- read.csv("multispecies_rgr.csv") # import relative growth rate data 
data_area_area <- read.csv("multispecies_area_area.csv") # import area*area data 

data_area <- data_area[complete.cases(data_area),]
data_rgr <- data_rgr[complete.cases(data_rgr),]

data_area$id2 <- paste(data_area$id,data_area$species,sep="")
data_rgr$id2 <- paste(data_rgr$id,data_rgr$species,sep="")

data_comp_rel <- data_area[,-8] # remove area_mm2 
data_comp_rel <- data_area[,-9] # remove area_stand 
data_comp_rel <- data_comp_rel[!(data_comp_rel$species == "TOT"),] # remove the total - will always be 100 
data_comp_rel <- data_comp_rel[!(data_comp_rel$treatment=="LM" | data_comp_rel$treatment=="SP" | data_comp_rel$treatment=="WB"),] # remove monocultures - will always be 100 

data_area <- data_area[,-10] # remove comp_rel from data_area

summary(data_area)
summary(data_rgr)
summary(data_comp_rel)

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
area_plot_raw 

# colour
area_plot_raw <- ggplot(data_area, aes(x=day,y=area_mm2,group=id2,colour=species)) + geom_line() + geom_point() 
area_plot_raw <- area_plot_raw + facet_grid(nutrients ~ treatment)
area_plot_raw <- area_plot_raw + scale_x_discrete(breaks=c(0,2,4,6,8,10),labels=c(0,2,4,6,8,10))
area_plot_raw 

#######################################
# Plot raw % composition through time #
#######################################
# black & white 
comp_rel_plot_raw <- ggplot(data_comp_rel, aes(x=day, y=comp_rel, group=id2, shape=species)) + geom_line() + geom_point(size=3)
comp_rel_plot_raw <- comp_rel_plot_raw + facet_grid(nutrients ~ treatment)
comp_rel_plot_raw <- comp_rel_plot_raw + scale_x_discrete(breaks=c(0,2,4,6,8,10),labels=c(0,2,4,6,8,10))
comp_rel_plot_raw

# colour
comp_rel_plot_raw <- ggplot(data_comp_rel, aes(x=day, y=comp_rel, group=id2, colour=species)) + geom_line() + geom_point()
comp_rel_plot_raw <- comp_rel_plot_raw + facet_grid(nutrients ~ treatment)
comp_rel_plot_raw <- comp_rel_plot_raw + scale_x_discrete(breaks=c(0,2,4,6,8,10),labels=c(0,2,4,6,8,10))
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

summary_data_rgr02 <- ddply(data_rgr, c("nutrients","treatment","species","id2"), summarise, 
                          N = length(rgr),
                          mean = mean(rgr),
                          max = max(rgr),
                          sd = sd(rgr),
                          se = sd / sqrt(N) )

summary_data_rgr02 <- subset(summary_data_rgr02, summary_data_rgr02$species != "TOT")

summary_data_meanrgr <- ddply(summary_data_rgr02, c("nutrients","treatment","species"), summarise,
                            N = length(mean),
                            mean2 = mean(mean),
                            sd = sd(mean),
                            se = sd / sqrt(N) )

colnames(summary_data_meanrgr)[5] <- "average_RGR"


summary_data_maxrgr <- ddply(summary_data_rgr02, c("nutrients","treatment","species"), summarise,
                              N = length(max),
                              max2 = mean(max),
                              sd = sd(max),
                              se = sd / sqrt(N) )

colnames(summary_data_maxrgr)[5] <- "maximum_RGR"


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

###############################
# Plot mean area through time #
###############################
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
# black & white 
limits <- aes(ymax = maximum_RGR + se, ymin = maximum_RGR - se)
dodge <- position_dodge(width=0.9)
maxrgr_plot <- ggplot(summary_data_maxrgr, aes(x=species, y=maximum_RGR, fill=species)) + geom_bar(position="dodge",stat="identity")
maxrgr_plot <- maxrgr_plot + facet_grid(nutrients ~ treatment)
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

################################################################
# Plot average average growth rate - by species - by treatment #
################################################################
# black & white 
limits <- aes(ymax = average_RGR + se, ymin= average_RGR - se)
dodge <- position_dodge(width=0.9)
avgrgr_plot <- ggplot(summary_data_meanrgr, aes(x=species, y=average_RGR, fill=species)) + geom_bar(position="dodge",stat="identity")
avgrgr_plot <- avgrgr_plot + facet_grid(nutrients ~ treatment)
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

##################################
# Plot species area on each axis #
##################################
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

# Lemna & Spirodela & Wolffia 
area_area_LMSPWB_plot <- ggplot(subset(data_area_area, data_area_area$treatment=="LMSPWB"), aes(x=LM,y=SP,group=id,colour=WB)) + geom_point() + geom_line()
area_area_LMSPWB_plot <- area_area_LMSPWB_plot + facet_grid(.~nutrients)
area_area_LMSPWB_plot <- area_area_LMSPWB_plot + xlab("area LM (sq.mm)") + ylab("area SP (sq.mm)")
area_area_LMSPWB_plot

# Lemna & Spirodela & Wolffia - just low nutrients 
area_area_LMSPWB_plot_low <- ggplot(subset(data_area_area, data_area_area$treatment=="LMSPWB"& data_area_area$nutrients=="low"), aes(x=LM,y=SP,group=id,colour=WB)) 
area_area_LMSPWB_plot_low <- area_area_LMSPWB_plot_low + geom_point() + geom_line()
area_area_LMSPWB_plot_low <- area_area_LMSPWB_plot_low + facet_grid(.~nutrients)
area_area_LMSPWB_plot_low <- area_area_LMSPWB_plot_low + xlab("area LM (sq.mm)") + ylab("area SP (sq.mm)")
area_area_LMSPWB_plot_low
