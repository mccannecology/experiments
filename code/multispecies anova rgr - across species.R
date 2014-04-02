###########################################
# Two-way anova                           #
# Y = rgr                                 #
# Nutrients * sp. treatment               #
# Comparing across species                #  
###########################################

# Check out the data that I will be working with 
head(summary_data_rgr02)

# add a column "species_richness"
summary_data_rgr02$species_richness <- NA

summary_data_rgr02$species_richness[summary_data_rgr02$treatment=="LM"]<-"one"
summary_data_rgr02$species_richness[summary_data_rgr02$treatment=="SP"]<-"one"
summary_data_rgr02$species_richness[summary_data_rgr02$treatment=="WB"]<-"one"
summary_data_rgr02$species_richness[summary_data_rgr02$treatment=="LMSP"]<-"two"
summary_data_rgr02$species_richness[summary_data_rgr02$treatment=="LMWB"]<-"two"
summary_data_rgr02$species_richness[summary_data_rgr02$treatment=="SPWB"]<-"two"
summary_data_rgr02$species_richness[summary_data_rgr02$treatment=="LMSPWB"]<-"three"

##################
# Y=rgr, average #
##################
# This is going to be a very unbalanced design
# There are a lot more replicates of species_richness=2 for each species 
# Due to the way SS are calculated when including an interaction (type III SS) 
# you must specify the contrasts option to obtain sensible results

# This does not account for an unbalanced design 
aov(mean ~ factor(nutrients)*factor(species)*factor(species_richness), data=summary_data_rgr02)

# Get Type III SS without package car 
# set the contrasts option in R
# choose a contrasts setting that sums to zero
options(contrasts = c("contr.sum","contr.poly"))

# run the anova
temp <- lm(mean ~ nutrients*species*species_richness, data=summary_data_rgr02)

# Call the drop1 function on each model component:
# The results give the type III SS, including the p-values from an F-test.
drop1(temp, mean ~ nutrients*species, test="F")
drop1(temp, mean ~ species*species_richness, test="F")
drop1(temp, mean ~ nutrients*species_richness, test="F")
drop1(temp, mean ~ nutrients, test="F")
drop1(temp, mean ~ species, test="F")
drop1(temp, mean ~ species_richness, test="F")

# This is an ANOVA with type III SS (for unbalanced design)
# It uses package car 
library(car)
library(multcomp)
anova_rgr_avg_ALL <- Anova(lm(mean ~ factor(nutrients)*factor(species)*factor(species_richness),
                              data=summary_data_rgr02, 
                              contrasts=list(nutrients=contr.sum, species=contr.sum, species_richness=contr.sum))
                           ,type=3)
anova_rgr_avg_ALL # gives you the summary 

glht(anova_rgr_avg_ALL, linfct = mcp(list(nutrients="Tukey",species="Tukey",species_richness="Tukey")))

glht(temp, linfct = mcp(nutrients="Tukey"))
glht(temp, linfct = mcp(species="Tukey"))
glht(temp, linfct = mcp(species_richness="Tukey"))

posthoc_anova_rgr_avg_ALL <- TukeyHSD(anova_rgr_avg_ALL)


posthoc_anova_rgr_avg_LM <- posthoc_anova_rgr_avg_LM$'nutrients:treatment'
significant <- subset(posthoc_anova_rgr_avg_LM, posthoc_anova_rgr_avg_LM[,4]<=0.050) # significant comparisons 
nonsignif <- subset(posthoc_anova_rgr_avg_LM, posthoc_anova_rgr_avg_LM[,4]>0.050) # non-significant comparisons 

# Examine residuals 
# plot a histogram 
# looks normal-ish
hist(resid(anova_rgr_avg_LM))

# QQ plot 
# Does not look very normal 
qqnorm(resid(anova_rgr_avg_LM)) 
qqline(resid(anova_rgr_avg_LM)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(anova_rgr_avg_LM)) # p-value = 0.002783

# Power Transformation 
# figure out the best power transformation 
library(car)
powerTransform(mean ~ nutrients*treatment, data=subset(summary_data_rgr02, summary_data_rgr02$species=="LM"))
summary(powerTransform(mean ~ nutrients*treatment, data=subset(summary_data_rgr02, summary_data_rgr02$species=="LM")))
# Y1 0.9500642
power <- 0.9500642    

# add the power transformation of mean rgr
data_rgr_avg_LM <- subset(summary_data_rgr02, summary_data_rgr02$species=="LM")
head(data_rgr_avg_LM)
data_rgr_avg_LM["power_mean"] <- ((data_rgr_avg_LM$mean)^power - 1) / power 

# take a quick look @ this data 
hist(subset(data_rgr_avg_LM$power_mean, data_rgr_avg_LM$nutrients=="low")) # looks normalish - except for some really low values
hist(subset(data_rgr_avg_LM$power_mean, data_rgr_avg_LM$nutrients=="high")) # looks normalish - except for some really low values

# re-do-the anova 
anova_rgr_avg_LM_power <- aov(power_mean ~ nutrients*treatment, data=data_rgr_avg_LM)
summary(anova_rgr_avg_LM_power)
posthoc_anova_rgr_avg_LM_power <- TukeyHSD(anova_rgr_avg_LM_power)
posthoc_anova_rgr_avg_LM_power <- posthoc_anova_rgr_avg_LM_power$'nutrients:treatment'
significant <- subset(posthoc_anova_rgr_avg_LM_power, posthoc_anova_rgr_avg_LM_power[,4]<=0.050) # significant comparisons 
nonsignif <- subset(posthoc_anova_rgr_avg_LM_power, posthoc_anova_rgr_avg_LM_power[,4]>0.050) # non-significant comparisons 

# Examine residuals #
# plot a histogram 
# looks normal-ish
hist(resid(anova_rgr_avg_LM_power),xlab="Residuals",main=NULL)

# QQ plot 
# Does not look very normal 
qqnorm(resid(anova_rgr_avg_LM_power),main=NULL) 
qqline(resid(anova_rgr_avg_LM_power)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(anova_rgr_avg_LM_power)) # p-value = 0.003201

# Waaayyy off

# LOG X + 1 TRANSFORMATION 
# add the log x + 1 transformation of mean rgr
head(data_rgr_avg_LM)
data_rgr_avg_LM["logx1_mean"] <- log(data_rgr_avg_LM$mean + 1)

# take a quick look @ this data 
hist(subset(data_rgr_avg_LM$logx1_mean, data_rgr_avg_LM$nutrients=="low")) # looks normalish - except for some really low values
hist(subset(data_rgr_avg_LM$logx1_mean, data_rgr_avg_LM$nutrients=="high")) # looks normalish - except for some really low values

# re-do-the anova 
anova_rgr_avg_LM_logx1 <- aov(logx1_mean ~ nutrients*treatment, data=data_rgr_avg_LM)
summary(anova_rgr_avg_LM_logx1)
posthoc_anova_rgr_avg_LM_logx1 <- TukeyHSD(anova_rgr_avg_LM_logx1)
posthoc_anova_rgr_avg_LM_logx1 <- posthoc_anova_rgr_avg_LM_logx1$'nutrients:treatment'
significant <- subset(posthoc_anova_rgr_avg_LM_logx1, posthoc_anova_rgr_avg_LM_logx1[,4]<=0.050) # significant comparisons 
nonsignif <- subset(posthoc_anova_rgr_avg_LM_logx1, posthoc_anova_rgr_avg_LM_logx1[,4]>0.050) # non-significant comparisons 

# Examine residuals #
# plot a histogram 
# looks normal-ish
hist(resid(anova_rgr_avg_LM_logx1),xlab="Residuals",main=NULL)

# QQ plot 
# Does not look very normal 
qqnorm(resid(anova_rgr_avg_LM_logx1),main=NULL) 
qqline(resid(anova_rgr_avg_LM_logx1)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(anova_rgr_avg_LM_logx1)) # p-value = 0.003482


# REMOVE OUTLIER
data_rgr_avg_LM_outlier <- subset(data_rgr_avg_LM, data_rgr_avg_LM$id2 != "J2LM")
head(data_rgr_avg_LM_outlier)

# take a quick look @ this data 
hist(subset(data_rgr_avg_LM_outlier$mean, data_rgr_avg_LM_outlier$nutrients=="low")) # looks normalish - except for some really low values
hist(subset(data_rgr_avg_LM_outlier$mean, data_rgr_avg_LM_outlier$nutrients=="high")) # looks normalish - except for some really low values

# re-do-the anova 
anova_rgr_avg_LM_outlier <- aov(mean ~ nutrients*treatment, data=data_rgr_avg_LM_outlier)
summary(anova_rgr_avg_LM_outlier)
posthoc_anova_rgr_avg_LM_outlier <- TukeyHSD(anova_rgr_avg_LM_outlier)
posthoc_anova_rgr_avg_LM_outlier <- posthoc_anova_rgr_avg_LM_outlier$'nutrients:treatment'
significant <- subset(posthoc_anova_rgr_avg_LM_outlier, posthoc_anova_rgr_avg_LM_outlier[,4]<=0.050) # significant comparisons 
nonsignif <- subset(posthoc_anova_rgr_avg_LM_outlier, posthoc_anova_rgr_avg_LM_outlier[,4]>0.050) # non-significant comparisons 

###### NOTE ###### 
# Now, I have an unbalanced design 
# Changing the order of the factors SLIGHTLY changes the results 
summary(aov(mean ~ treatment*nutrients, data=data_rgr_avg_LM_outlier)) # this gives you the SS result for nutrients
summary(aov(mean ~ nutrients*treatment, data=data_rgr_avg_LM_outlier)) # this gives you the SS result for treatment
###### NOTE ###### 

# Examine residuals #
# plot a histogram 
# looks normal-ish
hist(resid(anova_rgr_avg_LM_outlier),xlab="Residuals",main=NULL)

# QQ plot 
# Does not look very normal 
qqnorm(resid(anova_rgr_avg_LM_outlier),main=NULL) 
qqline(resid(anova_rgr_avg_LM_outlier)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(anova_rgr_avg_LM_outlier)) # p-value = 0.4799 - ok now residuals are normally distributed 
