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

##############################
# Two-way anova              #
# Y= total standardized area #
# Nutrients * sp. treatment  #
##############################

# subset your data first 
data_area_10 <- subset(data_area, data_area$day == 10)
data_area_10_TOT <- subset(data_area_10, data_area_10$species == "TOT")

anova_area_stand_10_TOT <- aov(area_stand ~ nutrients * treatment, data=data_area_10_TOT)
summary(anova_area_stand_10_TOT)
posthoc_anova_area_stand_10_TOT <- TukeyHSD(anova_area_stand_10_TOT)
posthoc_anova_area_stand_10_TOT <- posthoc_anova_area_stand_10_TOT$'nutrients:treatment'
significant <- subset(posthoc_anova_area_stand_10_TOT, posthoc_anova_area_stand_10_TOT[,4]<=0.050) # significant comparisons 
nonsignif <- subset(posthoc_anova_area_stand_10_TOT, posthoc_anova_area_stand_10_TOT[,4]>0.050) # non-significant comparisons 


# Response: Relative growth rates 
# Within each species, is average average RGR different betweeen species*nutrient treatments? (ANOVA)
# Within each species, is average maximum RGR different betweeen species*nutrient treatments? (ANOVA)
# Is the average density-specific RGR different different between species?  



