###########################################
# Two-way anova                           #
# Y = total area day 10/ total area day 0 #
# Nutrients * sp. treatment               #
#                                         #
# Transformed Y variable below            #
###########################################

######################
# Modify data frames #
######################
head(data_area_10_TOT)

# this data frame has area_stand, the response variable of interest 

###############################
# Test parametric assumptions #
###############################
# Histograms to look at normality 

# Histogram - by nutrient treatment 
# things look almost normal 
hist(subset(data_area_10_TOT$area_stand, data_area_10_TOT$nutrients=="high"))
hist(subset(data_area_10_TOT$area_stand, data_area_10_TOT$nutrients=="low"))

# Histogram - by nutrient*species treatment combination 
# (this is going to be difficult to be normal with only n=6)
nutrients<-c("low","high")
treatment<-c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB")
HIST<-list()
for (i in 1:length(nutrients)){
  for (j in 1:length(treatment)){
    HIST[i*j]<-hist(subset(data_area_10_TOT$area_stand, data_area_10_TOT$nutrients == nutrients[i] & data_area_10_TOT$treatment ==  treatment[j]),main=paste(nutrients[i],treatment[j]))
  }
}

# QQ plot - across everything 
qqnorm(data_area_10_TOT$area_stand)
qqline(data_area_10_TOT$area_stand)

# QQ plot - by nutrient treatment 
qqnorm(subset(data_area_10_TOT$area_stand, data_area_10_TOT$nutrients=="low"))
qqline(subset(data_area_10_TOT$area_stand, data_area_10_TOT$nutrients=="low"))

qqnorm(subset(data_area_10_TOT$area_stand, data_area_10_TOT$nutrients=="high"))
qqline(subset(data_area_10_TOT$area_stand, data_area_10_TOT$nutrients=="high"))

# QQ plot - by nutrient*species treatment combination 
# (this is going to be difficult to be normal with only n=6)
nutrients<-c("low","high")
treatment<-c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB")
QQ<-list()
for (i in 1:length(nutrients)){
  for (j in 1:length(treatment)){
    qqnorm(subset(data_area_10_TOT$area_stand, data_area_10_TOT$nutrients == nutrients[i] & data_area_10_TOT$treatment ==  treatment[j]),main=paste(nutrients[i],treatment[j]))
    qqline(subset(data_area_10_TOT$area_stand, data_area_10_TOT$nutrients == nutrients[i] & data_area_10_TOT$treatment ==  treatment[j]))
  }
}

# Shapiro-Wilk test - assess normality - across everything
# null hypothesis = sample came from a normally distributed population 
shapiro.test(data_area_10_TOT$area_stand)

# Shapiro-Wilk test - assess normality - by nutrient treatment 
# null hypothesis = sample came from a normally distributed population 
shapiro.test(subset(data_area_10_TOT$area_stand, data_area_10_TOT$nutrients=="low"))
shapiro.test(subset(data_area_10_TOT$area_stand, data_area_10_TOT$nutrients=="high"))

# Shapiro-Wilk test - assess normality - by nutrient*species treatment combination 
# null hypothesis = sample came from a normally distributed population 
nutrients<-c("low","high")
treatment<-c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB")
shapiro<-list()
for (i in 1:length(nutrients)){
  for (j in 1:length(treatment)){
    temp <- shapiro.test(subset(data_area_10_TOT$area_stand, data_area_10_TOT$nutrients == nutrients[i] & data_area_10_TOT$treatment ==  treatment[j]))
    shapiro[i*j] <- temp[2]
  }
}

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal 
bartlett.test(data_area_10_TOT$area_stand~data_area_10_TOT$nutrients) # p-value = 2.2e-16

bartlett.test(data_area_10_TOT$area_stand ~ data_area_10_TOT$nutrients * data_area_10_TOT$treatment) # p-value = 2.2e-16

bartlett.test(subset(data_area_10_TOT$area_stand, data_area_10_TOT$nutrients=="low") ~ subset(data_area_10_TOT$treatment, data_area_10_TOT$nutrients=="low")) # p-value = 0.9308

bartlett.test(subset(data_area_10_TOT$area_stand, data_area_10_TOT$nutrients=="high") ~ subset(data_area_10_TOT$treatment, data_area_10_TOT$nutrients=="high")) # p-value = 0.5795


################
# Do the ANOVA #
################
anova_area_stand_TOT <- aov(area_stand ~ nutrients*treatment, data=data_area_10_TOT)
summary(anova_area_stand_TOT)
posthoc_anova_area_stand_TOT <- TukeyHSD(anova_area_stand_TOT)
posthoc_anova_area_stand_TOT <- posthoc_anova_area_stand_TOT$'nutrients:treatment'
significant <- subset(posthoc_anova_area_stand_TOT, posthoc_anova_area_stand_TOT[,4]<=0.050) # significant comparisons 
nonsignif <- subset(posthoc_anova_area_stand_TOT, posthoc_anova_area_stand_TOT[,4]>0.050) # non-significant comparisons 

#####################
# Examine residuals #
#####################
# plot a histogram 
# looks normal-ish
hist(resid(anova_area_stand_TOT))

# QQ plot 
# Does not look very normal 
qqnorm(resid(anova_area_stand_TOT)) 
qqline(resid(anova_area_stand_TOT)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(anova_area_stand_TOT)) # p-value = 4.105e-06


###########################
# Try sqrt transformation #
###########################
# square root transformation 
data_area_10_TOT["sqrt_area_stand"] <- sqrt(data_area_10_TOT$area_stand) # add the sqrt transformation of area_stand

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal 
bartlett.test(subset(data_area_10_TOT$sqrt_area_stand, data_area_10_TOT$nutrients=="low") ~ subset(data_area_10_TOT$treatment, data_area_10_TOT$nutrients=="low")) # p-value = 0.9952
bartlett.test(subset(data_area_10_TOT$sqrt_area_stand, data_area_10_TOT$nutrients=="high") ~ subset(data_area_10_TOT$treatment, data_area_10_TOT$nutrients=="high")) # p-value = 0.7889

# re-do-the anova 
anova_area_stand_TOT_sqrt <- aov(sqrt_area_stand ~ nutrients*treatment, data=data_area_10_TOT)
summary(anova_area_stand_TOT_sqrt)
posthoc_anova_area_stand_TOT_sqrt <- TukeyHSD(anova_area_stand_TOT_sqrt)
posthoc_anova_area_stand_TOT_sqrt <- posthoc_anova_area_stand_TOT_sqrt$'nutrients:treatment'
significant <- subset(posthoc_anova_area_stand_TOT_sqrt, posthoc_anova_area_stand_TOT_sqrt[,4]<=0.050) # significant comparisons 
nonsignif <- subset(posthoc_anova_area_stand_TOT_sqrt, posthoc_anova_area_stand_TOT_sqrt[,4]>0.050) # non-significant comparisons 

# Examine residuals #

# plot a histogram 
# looks normal-ish
hist(resid(anova_area_stand_TOT_sqrt))

# QQ plot 
# Does not look very normal 
qqnorm(resid(anova_area_stand_TOT_sqrt)) 
qqline(resid(anova_area_stand_TOT_sqrt)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(anova_area_stand_TOT_sqrt)) # p-value = 0.003875

# closer, but not quite there 

##############################
# Try a power transformation #
##############################
# figure out the best power transformation 
library(car)
powerTransform(area_stand ~ nutrients*treatment, data=data_area_10_TOT)
summary(powerTransform(area_stand ~ nutrients*treatment, data=data_area_10_TOT))
# Y1 -0.2150466 
power <- -0.2150466 

# add the power transformation of area_stand
data_area_10_TOT["power_area_stand"] <- ((data_area_10_TOT$area_stand)^power - 1) / power 

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal 
bartlett.test(subset(data_area_10_TOT$power_area_stand, data_area_10_TOT$nutrients=="low") ~ subset(data_area_10_TOT$treatment, data_area_10_TOT$nutrients=="low")) # p-value = 0.7878
bartlett.test(subset(data_area_10_TOT$power_area_stand, data_area_10_TOT$nutrients=="high") ~ subset(data_area_10_TOT$treatment, data_area_10_TOT$nutrients=="high")) # p-value = 0.6287

# re-do-the anova 
anova_area_stand_TOT_power <- aov(power_area_stand ~ nutrients*treatment, data=data_area_10_TOT)
summary(anova_area_stand_TOT_power)
posthoc_anova_area_stand_TOT_power <- TukeyHSD(anova_area_stand_TOT_power)
posthoc_anova_area_stand_TOT_power <- posthoc_anova_area_stand_TOT_power$'nutrients:treatment'
significant <- subset(posthoc_anova_area_stand_TOT_power, posthoc_anova_area_stand_TOT_power[,4]<=0.050) # significant comparisons 
nonsignif <- subset(posthoc_anova_area_stand_TOT_power, posthoc_anova_area_stand_TOT_power[,4]>0.050) # non-significant comparisons 

# Examine residuals #

# plot a histogram 
# looks normal-ish
hist(resid(anova_area_stand_TOT_power))

# QQ plot 
# Does not look very normal 
qqnorm(resid(anova_area_stand_TOT_power)) 
qqline(resid(anova_area_stand_TOT_power)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(anova_area_stand_TOT_power)) # p-value = 0.4362

# Residuals are actually normal!!!!!

