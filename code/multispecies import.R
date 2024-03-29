################################################
# Analysis of multispecies duckweed experiment #
# Summer 2013                                  #
#                                              #
# Importing data                               #
# Do all calculations with R                   #
# Reads "multispecies_area_raw.csv"            #
################################################
library(plyr)

################################################
data <- read.csv("multispecies_area_and_total_rgr.csv")

data_area_raw <- read.csv("multispecies_area_raw.csv") # import area data 

# add a new variable that combines plate and well to use as an ID 
data_area_raw$id <- paste(data_area_raw$plate,data_area_raw$well,sep="")

# add new variables to data frame
data_area_raw$final_minus_initial <- data_area_raw$TOT10 - data_area_raw$TOT0
data_area_raw$final_divide_initial <- data_area_raw$TOT10 / data_area_raw$TOT0

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

# Calculate density-standardized RGR
# make a temporary data frame with all rows with species = TOT 
temptotals <- subset(data_area,data_area$species=="TOT")
temptotals <- temptotals[,c("id","day","area_mm2")]
colnames(temptotals)[3] <- "area_total"
temptotals
                 
# merge the temporary data frame holding your total areas with the original data_area data frame 
data_area <- merge(data_area,temptotals,by=c("id","day"))

# clean up 
data_area <- data_area[with(data_area, order(day,id)),] # re-order - by id and day
row.names(data_area) <- seq(nrow(data_area)) # Re-name the rows so they're not so ugly

# clean up your environment
rm(temptotals)

# total area of a well plate
diameter <- 35 
wellarea <- pi*(diameter/2)^2

# this standardized by the density of all species in the previous time step
# I'm not sure if this standardization makes any sense at all 
# currently, if you have high RGR and high density you get a smaller # than if you have the same RGR and low density -- this seems wrong!
rgr1_stand <- rgr1/(subset(data_area$area_total,data_area$day==0)/wellarea)
rgr3_stand <- rgr3/(subset(data_area$area_total,data_area$day==2)/wellarea)
rgr5_stand <- rgr5/(subset(data_area$area_total,data_area$day==4)/wellarea)
rgr7_stand <- rgr7/(subset(data_area$area_total,data_area$day==6)/wellarea)
rgr9_stand <- rgr9/(subset(data_area$area_total,data_area$day==8)/wellarea)

# combine each day's density-standarized rgr into a single vector
rgr_stand  <- c(rgr1_stand,rgr3_stand,rgr5_stand,rgr7_stand,rgr9_stand)

# set-up the dataframe to hold the rgrs
data_rgr <- subset(data_area, data_area$day != 10) # exclude day 10
data_rgr$day <- data_rgr$day+1 # add one to day so it corresponds to the midpoint for RGR calculations 
data_rgr <- data_rgr[-11] # get rid of columns you don't need 
data_rgr <- data_rgr[-10]
data_rgr <- data_rgr[-9]
data_rgr <- data_rgr[-8]

# add the rgr and rgr_stand vectors the the data frame 
data_rgr$rgr <- rgr
data_rgr$rgr_stand <- rgr_stand

# cleanup your environment
rm(list = c("rgr","rgr1","rgr3","rgr5","rgr7","rgr9","rgr_stand","rgr1_stand","rgr3_stand","rgr5_stand","rgr7_stand","rgr9_stand","diameter","wellarea"))

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
summary_data_rgr02 <- ddply(data_rgr, c("nutrients","treatment","species","id2"), 
                            summarise, 
                            N = length(rgr),
                            avgRGR = mean(rgr),
                            maxRGR = max(rgr),
                            sd = sd(rgr),
                            se = sd / sqrt(N) )
summary_data_rgr03 <- subset(summary_data_rgr02, summary_data_rgr02$species == "TOT")
summary_data_rgr02 <- subset(summary_data_rgr02, summary_data_rgr02$species != "TOT")


#######################
# Mean maxRGR         #
# Total               #
# by treatment combo  #
# Use for plotting    #
#######################
# rgr_max
summary_data_maxRGR <- ddply(summary_data_rgr03, 
                             c("treatment","nutrients"), 
                             summarise, 
                             N = length(maxRGR),
                             mean = mean(maxRGR),
                             sd = sd(maxRGR),
                             se = sd / sqrt(N) )
colnames(summary_data_maxRGR)[4] <- "maxRGR"
# re-order my treatments so they go from low to high
summary_data_maxRGR$nutrients <- factor(summary_data_maxRGR$nutrients , levels=c("low","high"))
summary_data_maxRGR$treatment <- factor(summary_data_maxRGR$treatment , levels=c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB"))
head(summary_data_maxRGR)

#######################
# Mean avgRGR         #
# Total               #
# by treatment combo  #
# Use for plotting    #
#######################
# rgr_max
summary_data_avgRGR <- ddply(summary_data_rgr03, 
                             c("treatment","nutrients"), 
                             summarise, 
                             N = length(avgRGR),
                             mean = mean(avgRGR),
                             sd = sd(avgRGR),
                             se = sd / sqrt(N) )
colnames(summary_data_avgRGR)[4] <- "avgRGR"
# re-order my treatments so they go from low to high
summary_data_avgRGR$nutrients <- factor(summary_data_avgRGR$nutrients , levels=c("low","high"))
summary_data_avgRGR$treatment <- factor(summary_data_avgRGR$treatment , levels=c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB"))
head(summary_data_avgRGR)


# Relative growth rate 
# Species-specifc 
# takes the mean of the mean growth rates by species in each well 
summary_data_meanrgr <- ddply(summary_data_rgr02, c("nutrients","treatment","species"), summarise,
                              N = length(mean),
                              mean2 = mean(mean),
                              sd = sd(mean),
                              se = sd / sqrt(N) )
colnames(summary_data_meanrgr)[5] <- "average_RGR"

# Relative growth rate 
# species-specific
# the mean of the maximum growth rates by species in each well 
summary_data_maxrgr <- ddply(summary_data_rgr02, c("nutrients","treatment","species"), summarise,
                             N = length(max),
                             max2 = mean(max),
                             sd = sd(max),
                             se = sd / sqrt(N) )
colnames(summary_data_maxrgr)[5] <- "maximum_RGR"

# Relative growth rate - standardized by density
summary_data_rgr_stand <- ddply(data_rgr, c("nutrients","treatment","species","day"), summarise, 
                          N = length(rgr_stand),
                          mean = mean(rgr_stand),
                          sd = sd(rgr_stand),
                          se = sd / sqrt(N) )
colnames(summary_data_rgr_stand)[6] <- "rgr_stand"

# average the Relative growth rate - standardized by density
summary_data_avg_rgr_stand <- ddply(summary_data_rgr_stand, c("nutrients","treatment","species"), summarise, 
                                N = length(rgr_stand),
                                mean = mean(rgr_stand),
                                sd = sd(rgr_stand),
                                se = sd / sqrt(N) )
colnames(summary_data_avg_rgr_stand)[5] <- "rgr_stand"


###########################
# Mean final-initial area #
# by treatment combo      #
# Use for plotting        #
###########################
# Area
summary_data_area_final_minus_initial <- ddply(subset(data_area_raw, data_area_raw$treatment != "Blank"), 
                                               c("nutrients","treatment"), 
                                               summarise, 
                                               N = sum(!is.na(final_minus_initial)),
                                               mean = mean(final_minus_initial,na.rm=T),
                                               sd = sd(final_minus_initial,na.rm=T),
                                               se = sd / sqrt(N) )
colnames(summary_data_area_final_minus_initial)[4] <- "final_minus_initial"
# re-order my treatments so they go from low to high
summary_data_area_final_minus_initial$nutrients <- factor(summary_data_area_final_minus_initial$nutrients , levels=c("low","high"))
summary_data_area_final_minus_initial$treatment <- factor(summary_data_area_final_minus_initial$treatment , levels=c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB"))
head(summary_data_area_final_minus_initial)


###########################
# Mean final/initial area #
# by treatment combo      #
# Use for plotting        #
###########################
# Area
summary_data_area_final_divide_initial <- ddply(subset(data_area_raw, data_area_raw$treatment != "Blank"), 
                                                c("nutrients","treatment"), 
                                                summarise, 
                                                N = sum(!is.na(final_divide_initial)),
                                                mean = mean(final_divide_initial,na.rm=T),
                                                sd = sd(final_divide_initial,na.rm=T),
                                                se = sd / sqrt(N) )
colnames(summary_data_area_final_divide_initial)[4] <- "final_divide_initial"
# re-order my treatments so they go from low to high
summary_data_area_final_divide_initial$nutrients <- factor(summary_data_area_final_divide_initial$nutrients , levels=c("low","high"))
summary_data_area_final_divide_initial$treatment <- factor(summary_data_area_final_divide_initial$treatment , levels=c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB"))
head(summary_data_area_final_divide_initial)



