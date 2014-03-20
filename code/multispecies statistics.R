################################################
# Analysis of multispecies duckweed experiment #
#                                              #
# Statistical analyses                         #
################################################

###############################
# Test parametric assumptions # ################# This should go with the analysis that it accompanies 
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

##############################
# Two-way ancova             #
# Y = total area             #
# Covariate: initial area    #
# Nutrients * sp. treatment  #
##############################
# create a dataframe that has both area_0 and area_10 as a column (variable)
# I am goign to add this column to data_area_10
data_area_10$area_0 <- subset(data_area$area_mm2, data_area$day==0)
data_area_10_TOT <- subset(data_area_10, data_area_10$species == "TOT")
colnames(data_area_10_TOT)[8]<-"area_10"

# visualize the relationship between initial area (day 0) and final area (day 10) 
# low nutrients
plot(subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients=="low"), subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="low"))
# high nutrients
plot(subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients=="high"), subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="high"))

# do the ANCOVA 
ancova_area_10_TOT <- aov(area_10 ~ nutrients*treatment+area_0, data=data_area_10_TOT)
summary(ancova_area_10_TOT)
posthoc_ancova_area_10_TOT <- TukeyHSD(ancova_area_10_TOT)
posthoc_ancova_area_10_TOT <- posthoc_ancova_area_10_TOT$'nutrients:treatment'
significant <- subset(posthoc_ancova_area_10_TOT, posthoc_ancova_area_10_TOT[,4]<=0.050) # significant comparisons 
nonsignif <- subset(posthoc_ancova_area_10_TOT, posthoc_ancova_area_10_TOT[,4]>0.050) # non-significant comparisons 

##############################
# Two-way anova              #
# Y= max RGR                 #
# Lemna                      #
# Nutrients * sp. treatment  #
##############################
data_rgr_LM <- subset(summary_data_rgr02, summary_data_rgr02$species == "LM")
anova_rgr_max_LM <- aov(max ~ nutrients * treatment, data=data_rgr_LM)
summary(anova_rgr_max_LM)

##############################
# Two-way anova              #
# Y= avg RGR                 #
# Lemna                      #
# Nutrients * sp. treatment  #
##############################
anova_rgr_mean_LM <- aov(mean ~ nutrients * treatment, data=data_rgr_LM)
summary(anova_rgr_mean_LM)

##############################
# Two-way anova              #
# Y= max RGR                 #
# Wolffia                    #
# Nutrients * sp. treatment  #
##############################
data_rgr_WB <- subset(summary_data_rgr02, summary_data_rgr02$species == "WB")
anova_rgr_max_WB <- aov(max ~ nutrients * treatment, data=data_rgr_WB)
summary(anova_rgr_max_WB)

##############################
# Two-way anova              #
# Y= avg RGR                 #
# Wolffia                    #
# Nutrients * sp. treatment  #
##############################
anova_rgr_mean_WB <- aov(mean ~ nutrients * treatment, data=data_rgr_WB)
summary(anova_rgr_mean_WB)

##############################
# Two-way anova              #
# Y= max RGR                 #
# Spirodela                  #
# Nutrients * sp. treatment  #
##############################
data_rgr_SP <- subset(summary_data_rgr02, summary_data_rgr02$species == "SP")
anova_rgr_max_SP <- aov(max ~ nutrients * treatment, data=data_rgr_SP)
summary(anova_rgr_max_SP)

##############################
# Two-way anova              #
# Y= avg RGR                 #
# Spirodela                  #
# Nutrients * sp. treatment  #
##############################
anova_rgr_mean_SP <- aov(mean ~ nutrients * treatment, data=data_rgr_SP)
summary(anova_rgr_mean_SP)

##############################
# Permutation ANOVA          # 
# Y = area_stand_10          #
# Nutrients * sp. treatment  #
##############################
library(vegan)
# This works, but I'm not sure if it's right 
# I had to make the response variable Y multivariate, so I used area_10 and area_0
adonis(cbind(data_area_10_TOT$area_0,data_area_10_TOT$area_10) ~ nutrients + treatment, data = data_area_10_TOT, permutations =999)



# Response: Relative growth rates 
# Is the average density-specific RGR different different between species?  


