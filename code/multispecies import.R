################################################
# Analysis of multispecies duckweed experiment #
#                                              #
# Importing data                               #
# Attempting to do all calculations with R     #
################################################
library(plyr)

################################################
data_area_raw <- read.csv("multispecies_area_raw.csv") # import area data 

# add a new variable that combines plate and well to use as an ID 
data_area_raw$id <- paste(data_area_raw$plate,data_area_raw$well,sep="")

# reshape so repeated measures (days) occur as separate rows 
# new variable headings with be LM, SP, WB, and TOT (areas)
data_area <- reshape(data_area_raw, 
                        idvar="id",
                        varying = list(
                        c("LM0","LM2","LM4","LM6","LM8","LM10"),
                        c("SP0","SP2","SP4","SP6","SP8","SP10"),  
                        c("WB0","WB2","WB4","WB6","WB8","WB10"),
                        c("TOT0","TOT2","TOT4","TOT6","TOT8","TOT10")),
                        v.names = c("LM","SP","WB","TOT"),
                        times = c(0,2,4,6,8,10),
                        timevar = "day",
                        direction = "long")

data_area_area <- data_area[-10] # makes the data_area_area dataframe if I want it for later 

# calculate relative % species composition 
data_area$LM_comp <- data_area$LM / data_area$TOT
data_area$SP_comp <- data_area$SP / data_area$TOT
data_area$WB_comp <- data_area$WB / data_area$TOT
data_area$TOT_comp <- data_area$TOT / data_area$TOT

# re-shape again so each species within a well gets its own row
# new variable headings will be area_mm2 and comp_rel
data_area <- reshape(data_area, 
                         idvar = c("id","day"),
                         varying = list(
                           c("LM","SP","WB","TOT"),
                           c("LM_comp","SP_comp","WB_comp","TOT_comp")),
                         timevar = "species",
                         v.names = c("area_mm2","comp_rel"),
                         times = c("LM","SP","WB","TOT"),
                         direction = "long")

# clean up 
data_area <- subset(data_area, data_area$area != 0) # remove rows where a species is not present in the treatment 
data_area <- data_area[with(data_area, order(day,id)),] # re-order - by id and day
row.names(data_area) <- seq(nrow(data_area)) # Re-name the rows so they're not so ugly

# add standardized area 
initial <- subset(data_area$area_mm2,data_area$day == 0) # creates a vector of initial areas 
data_area$area_stand <- data_area$area_mm2 / initial # divide area_mm2 by that initial area & create a new variable
rm(initial) # cleanup your environment

# calculate rgr 
rgr1 <- (log(subset(data_area$area_mm2,data_area$day==2))-log(subset(data_area$area_mm2,data_area$day==0)))/2
rgr3 <- (log(subset(data_area$area_mm2,data_area$day==4))-log(subset(data_area$area_mm2,data_area$day==2)))/2
rgr5 <- (log(subset(data_area$area_mm2,data_area$day==6))-log(subset(data_area$area_mm2,data_area$day==4)))/2
rgr7 <- (log(subset(data_area$area_mm2,data_area$day==8))-log(subset(data_area$area_mm2,data_area$day==6)))/2
rgr9 <- (log(subset(data_area$area_mm2,data_area$day==10))-log(subset(data_area$area_mm2,data_area$day==8)))/2

# combine each day's rgr into a single vector
rgr <- c(rgr1,rgr3,rgr5,rgr7,rgr9)

# set-up the dataframe to hold the rgrs
data_rgr <- subset(data_area, data_area$day != 10) # exclude day 10
data_rgr$day <- data_rgr$day+1 # add one to day so it corresponds to the midpoint for RGR calculations 
data_rgr <- data_rgr[-10] # get rid of columns you don't need 
data_rgr <- data_rgr[-9]
data_rgr <- data_rgr[-8]

# add the rgr vector the the data frame 
data_rgr$rgr <- rgr

# cleanup your environment
rm(list = c("rgr","rgr1","rgr3","rgr5","rgr7","rgr9"))

# add a second id that is specific to a species in the well 
data_area$id2 <- paste(data_area$id,data_area$species,sep="")
data_rgr$id2 <- paste(data_rgr$id,data_rgr$species,sep="")

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
summary_data_comp_rel <- ddply(data_area, c("nutrients","treatment","species","day"), summarise, 
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



