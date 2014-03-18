################################################
# Analysis of multispecies duckweed experiment #
#                                              #
# Importing data                               #
# Attempint to do all calculations with R      #
################################################
data_area_raw <- read.csv("multispecies_area_raw.csv") # import area data 

# add a new variable that combines plate and well to use as an ID 
data_area_raw$id <- paste(data_area_raw$plate,data_area_raw$well,sep="")

# reshape so repeated measures (time) occur as separate rows 
data_area_new <- reshape(data_area_raw, 
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

# add relative % species composition here 
data_area_new$LM_comp <- data_area_new$LM / data_area_new$TOT
data_area_new$SP_comp <- data_area_new$SP / data_area_new$TOT
data_area_new$WB_comp <- data_area_new$WB / data_area_new$TOT
data_area_new$TOT_comp <- data_area_new$TOT / data_area_new$TOT

# re-shape again so each species within a well gets its own row
data_area_new <- reshape(data_area_new, 
                         idvar = c("id","day"),
                         varying = list(
                           c("LM","SP","WB","TOT"),
                           c("LM_comp","SP_comp","WB_comp","TOT_comp")),
                         timevar = "species",
                         v.names = c("area_mm2","comp_rel"),
                         times = c("LM","SP","WB","TOT"),
                         direction = "long")

# clean up 
data_area_new <- subset(data_area_new, data_area_new$area != 0) # remove rows where a species is not present in the treatment 
data_area_new <- data_area_new[with(data_area_new, order(day,id)),] # re-order - by id and day
row.names(data_area_new) <- seq(nrow(data_area_new)) # Re-name the rows so they're not so ugly

# add standardized area 
initial <- subset(data_area_new$area_mm2,data_area_new$day == 0) # creates a vector of initial areas 
data_area_new$area_stand <- data_area_new$area_mm2 / initial # divide area_mm2 by that initial area & create a new variable

# calculate rgr 
rgr1 <- (log(subset(data_area_new$area_mm2,data_area_new$day==2))-log(subset(data_area_new$area_mm2,data_area_new$day==0)))/2
rgr3 <- (log(subset(data_area_new$area_mm2,data_area_new$day==4))-log(subset(data_area_new$area_mm2,data_area_new$day==2)))/2
rgr5 <- (log(subset(data_area_new$area_mm2,data_area_new$day==6))-log(subset(data_area_new$area_mm2,data_area_new$day==4)))/2
rgr7 <- (log(subset(data_area_new$area_mm2,data_area_new$day==8))-log(subset(data_area_new$area_mm2,data_area_new$day==6)))/2
rgr9 <- (log(subset(data_area_new$area_mm2,data_area_new$day==10))-log(subset(data_area_new$area_mm2,data_area_new$day==8)))/2

# combine each day's rgr into a single vector
rgr <- c(rgr1,rgr3,rgr5,rgr7,rgr9)

# set-up the dataframe to hold the rgrs
data_rgr_new <- subset(data_area_new, data_area_new$day != 10) # exclude day 10
data_rgr_new$day <- data_rgr_new$day+1 # add one to day so it corresponds to the midpoint for RGR calculations 
data_rgr_new <- data_rgr_new[-10] # get rid of columns you don't need 
data_rgr_new <- data_rgr_new[-9]
data_rgr_new <- data_rgr_new[-8]

# add the rgr vector the the data frame 
data_rgr_new$rgr <- rgr

