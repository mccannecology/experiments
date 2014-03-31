###########################################
# Two-way anova                           #
# Y = rgr                                 #
# Nutrients * sp. treatment               #
# One ANOVA for each species              #  
###########################################

# Check out the data that I will be working with 
head(summary_data_rgr02)

##################
# Y=rgr, average #
# Lemna          #
##################
# take a quick look @ this data 
hist(subset(summary_data_rgr02$mean, summary_data_rgr02$species=="LM")) # looks bimodal
hist(subset(summary_data_rgr02$mean, summary_data_rgr02$species=="LM" & summary_data_rgr02$nutrients=="low")) # looks normalish 
hist(subset(summary_data_rgr02$mean, summary_data_rgr02$species=="LM" & summary_data_rgr02$nutrients=="high")) # not so normal looking 

anova_rgr_avg_LM <- aov(mean ~ nutrients*treatment, data=subset(summary_data_rgr02, summary_data_rgr02$species=="LM"))
summary(anova_rgr_avg_LM)
posthoc_anova_rgr_avg_LM <- TukeyHSD(anova_rgr_avg_LM)
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

##################
# Y=rgr, average #
# Spirodela      #
##################
anova_rgr_avg_SP <- aov(mean ~ nutrients*treatment, data=subset(summary_data_rgr02, summary_data_rgr02$species=="SP"))
summary(anova_rgr_avg_SP)
posthoc_anova_rgr_avg_SP <- TukeyHSD(anova_rgr_avg_SP)
posthoc_anova_rgr_avg_SP <- posthoc_anova_rgr_avg_SP$'nutrients:treatment'
significant <- subset(posthoc_anova_rgr_avg_SP, posthoc_anova_rgr_avg_SP[,4]<=0.050) # significant comparisons 
nonsignif <- subset(posthoc_anova_rgr_avg_SP, posthoc_anova_rgr_avg_SP[,4]>0.050) # non-significant comparisons 

# Examine residuals 
# plot a histogram 
# looks normal-ish
hist(resid(anova_rgr_avg_SP))

# QQ plot 
# Does not look very normal 
qqnorm(resid(anova_rgr_avg_SP)) 
qqline(resid(anova_rgr_avg_SP)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(anova_rgr_avg_SP)) # p-value = 0.2272

##################
# Y=rgr, average #
# Wolffia        #
##################
anova_rgr_avg_WB <- aov(mean ~ nutrients*treatment, data=subset(summary_data_rgr02, summary_data_rgr02$species=="WB"))
summary(anova_rgr_avg_WB)
posthoc_anova_rgr_avg_WB <- TukeyHSD(anova_rgr_avg_WB)
posthoc_anova_rgr_avg_WB <- posthoc_anova_rgr_avg_WB$'nutrients:treatment'
significant <- subset(posthoc_anova_rgr_avg_WB, posthoc_anova_rgr_avg_WB[,4]<=0.050) # significant comparisons 
nonsignif <- subset(posthoc_anova_rgr_avg_WB, posthoc_anova_rgr_avg_WB[,4]>0.050) # non-significant comparisons 

# Examine residuals 
# plot a histogram 
# looks normal-ish
hist(resid(anova_rgr_avg_WB))

# QQ plot 
# Does not look very normal 
qqnorm(resid(anova_rgr_avg_WB)) 
qqline(resid(anova_rgr_avg_WB)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(anova_rgr_avg_WB)) # p-value = 0.2179

##################
# Y=rgr, maximum #
# Lemna          #
##################
anova_rgr_max_LM <- aov(max ~ nutrients*treatment, data=subset(summary_data_rgr02, summary_data_rgr02$species=="LM"))
summary(anova_rgr_max_LM)
posthoc_anova_rgr_max_LM <- TukeyHSD(anova_rgr_max_LM)
posthoc_anova_rgr_max_LM <- posthoc_anova_rgr_max_LM$'nutrients:treatment'
significant <- subset(posthoc_anova_rgr_max_LM, posthoc_anova_rgr_max_LM[,4]<=0.050) # significant comparisons 
nonsignif <- subset(posthoc_anova_rgr_max_LM, posthoc_anova_rgr_max_LM[,4]>0.050) # non-significant comparisons 

# Examine residuals 
# plot a histogram 
# looks normal-ish
hist(resid(anova_rgr_max_LM))

# QQ plot 
# Does not look very normal 
qqnorm(resid(anova_rgr_max_LM)) 
qqline(resid(anova_rgr_max_LM)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(anova_rgr_max_LM)) # p-value = 0.09322

##################
# Y=rgr, maximum #
# Spirodela      #
##################
anova_rgr_max_SP <- aov(max ~ nutrients*treatment, data=subset(summary_data_rgr02, summary_data_rgr02$species=="SP"))
summary(anova_rgr_max_SP)
posthoc_anova_rgr_max_SP <- TukeyHSD(anova_rgr_max_SP)
posthoc_anova_rgr_max_SP <- posthoc_anova_rgr_max_SP$'nutrients:treatment'
significant <- subset(posthoc_anova_rgr_max_SP, posthoc_anova_rgr_max_SP[,4]<=0.050) # significant comparisons 
nonsignif <- subset(posthoc_anova_rgr_max_SP, posthoc_anova_rgr_max_SP[,4]>0.050) # non-significant comparisons 

# Examine residuals 
# plot a histogram 
# looks normal-ish
hist(resid(anova_rgr_max_SP))

# QQ plot 
# Does not look very normal 
qqnorm(resid(anova_rgr_max_SP)) 
qqline(resid(anova_rgr_max_SP)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(anova_rgr_max_SP)) # p-value = 0.9459

##################
# Y=rgr, maximum #
# Wolffia        #
##################
anova_rgr_max_WB <- aov(max ~ nutrients*treatment, data=subset(summary_data_rgr02, summary_data_rgr02$species=="WB"))
summary(anova_rgr_max_WB)
posthoc_anova_rgr_max_WB <- TukeyHSD(anova_rgr_max_WB)
posthoc_anova_rgr_max_WB <- posthoc_anova_rgr_max_WB$'nutrients:treatment'
significant <- subset(posthoc_anova_rgr_max_WB, posthoc_anova_rgr_max_WB[,4]<=0.050) # significant comparisons 
nonsignif <- subset(posthoc_anova_rgr_max_WB, posthoc_anova_rgr_max_WB[,4]>0.050) # non-significant comparisons 

# Examine residuals 
# plot a histogram 
# looks normal-ish
hist(resid(anova_rgr_max_WB))

# QQ plot 
# Does not look very normal 
qqnorm(resid(anova_rgr_max_WB)) 
qqline(resid(anova_rgr_max_WB)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(anova_rgr_max_WB)) # p-value = 0.7799
