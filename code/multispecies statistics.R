################################################
# Analysis of multispecies duckweed experiment #
#                                              #
# Statistical analyses                         #
################################################

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


