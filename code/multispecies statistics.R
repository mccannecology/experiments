################################################
# Analysis of multispecies duckweed experiment #
#                                              #
# Statistical analyses                         #
################################################

###############################
# Test parametric assumptions #
###############################
nutrients<-c("low","high")
treatment<-c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB")

HIST<-list()
# plot data by treatment combinations 
for (i in 1:length(nutrients)){
  for (j in 1:length(treatment)){
    HIST[i*j]<-hist(subset(data_area$area_stand, data_area$day == 10 & data_area$nutrients == nutrients[i] & data_area$treatment ==  treatment[j] & data_area$species == "TOT"),main=paste(nutrients[i],treatment[j]))
  }
}


# QQ plots

# Shapiro-Wilk test - assess normality 
# null hypothesis = sample came from a normally distributed population 
# I need to do this test within each treatment combination (nutrients * treatment)
# an example with subsetting: shapiro.test(subset(data_area$area_stand, data_area$day == 10 & data_area$nutrients == "low"))
# an example with subsetting: shapiro.test(subset(data_area$area_stand, data_area$day == 10 & data_area$nutrients == "high"))

# Bartlett Test of Homogeneity of Variances - within each treatment group 
# null hypothesis = population variances are equal 

# Response: Area 
# Is final (day 10) standardized area different between species*nutrient treatments? (ANOVA)
# Is final (day 10) standardized area different between species richnesses? (harder to do this - how do I group? average?)

# Response: Relative growth rates 
# Within each species, is average average RGR different betweeen species*nutrient treatments? (ANOVA)
# Within each species, is average maximum RGR different betweeen species*nutrient treatments? (ANOVA)
# Is the average density-specific RGR different different between species?  



