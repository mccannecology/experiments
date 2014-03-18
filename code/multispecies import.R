################################################
# Analysis of multispecies duckweed experiment #
#                                              #
# Importing and summarizing data               #
################################################
library(plyr)

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

#######################
# Means and variances #
# Use for plotting    #
#######################
# Area
summary_data_area <- ddply(data_area, c("nutrients","treatment","species","day"), summarise, 
                           N = length(area_mm2),
                           mean = mean(area_mm2),
                           sd = sd(area_mm2),
                           se = sd / sqrt(N) )
colnames(summary_data_area)[6] <- "area"

# Standardized area (Current/Initial)
summary_data_area_stand <- ddply(data_area, c("nutrients","treatment","species","day"), summarise, 
                           N = length(area_stand),
                           mean = mean(area_stand),
                           sd = sd(area_stand),
                           se = sd / sqrt(N) )
colnames(summary_data_area_stand)[6] <- "area_stand"

# Relative % composition
summary_data_comp_rel <- ddply(data_comp_rel, c("nutrients","treatment","species","day"), summarise, 
                           N = length(comp_rel),
                           mean = mean(comp_rel),
                           sd = sd(comp_rel),
                           se = sd / sqrt(N) )
colnames(summary_data_comp_rel)[6] <- "comp_rel"

# Relative growth rate 
summary_data_rgr <- ddply(data_rgr, c("nutrients","treatment","species","day"), summarise, 
                          N = length(rgr),
                          mean = mean(rgr),
                          sd = sd(rgr),
                          se = sd / sqrt(N) )
colnames(summary_data_rgr)[6] <- "rgr"

# Relative growth rate - same as above, but organized by each species in each well and finds the maximum too 
summary_data_rgr02 <- ddply(data_rgr, c("nutrients","treatment","species","id2"), summarise, 
                          N = length(rgr),
                          mean = mean(rgr),
                          max = max(rgr),
                          sd = sd(rgr),
                          se = sd / sqrt(N) )
summary_data_rgr02 <- subset(summary_data_rgr02, summary_data_rgr02$species != "TOT")

# Relative growth rate - takes the mean of the mean growth rates by species in each well 
summary_data_meanrgr <- ddply(summary_data_rgr02, c("nutrients","treatment","species"), summarise,
                            N = length(mean),
                            mean2 = mean(mean),
                            sd = sd(mean),
                            se = sd / sqrt(N) )
colnames(summary_data_meanrgr)[5] <- "average_RGR"

# Relative growth rate - the mean of the maximum growth rates by species in each well 
summary_data_maxrgr <- ddply(summary_data_rgr02, c("nutrients","treatment","species"), summarise,
                              N = length(max),
                              max2 = mean(max),
                              sd = sd(max),
                              se = sd / sqrt(N) )
colnames(summary_data_maxrgr)[5] <- "maximum_RGR"



