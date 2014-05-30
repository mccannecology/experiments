#################################################
# Analysis of multispecies duckweed experiment  #
# Summer 2013                                   #
#                                               #
# Final Area / Initial Area                     #
# Total within a well - not species-specific    # 
#################################################

library(ggplot2)

# check out the data that you will use 
head(summary_data_area_final_divide_initial)
head(data_area_raw)

#############################
# Mean final / initial area #
# By each treatment combo   #
#############################
summary_data_finaldivideinitial <- read.csv("area_stand_posthoc.csv")
summary_data_finaldivideinitial$nutrients <- factor(summary_data_finaldivideinitial$nutrients , levels=c("low","high"))
summary_data_finaldivideinitial$treatment <- factor(summary_data_finaldivideinitial$treatment , levels=c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB"))

mean_area_final_divide_initial_plot <- ggplot(summary_data_finaldivideinitial, aes(x=treatment, y=area_stand,shape=nutrients)) 
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + geom_errorbar(aes(ymin=area_stand-se, ymax=area_stand+se), width=0.1)
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + geom_point(size=3)
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + ylab("final area / initial area")
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + xlab("species treatment")
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + theme_classic(base_size=18)
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + geom_text(data=summary_data_finaldivideinitial,aes(x=treatment, y=area_stand+se+0.5,label=label))
mean_area_final_divide_initial_plot

ggsave(filename="mean_area_final_divide_initial_plot.jpg",mean_area_final_divide_initial_plot,height=8,width=11)

#####################
# Preliminary anova #
# Three-way         #
#####################
area_final_divide_initial_anova <- aov(final_divide_initial ~ treatment*nutrients, data=data_area_raw)
summary(area_final_divide_initial_anova)
TukeyHSD(area_final_divide_initial_anova)

#####################
# Examine residuals #
#####################
hist(resid(area_final_divide_initial_anova)) # plot a histogram 

qqnorm(resid(area_final_divide_initial_anova)) # QQ plot 
qqline(resid(area_final_divide_initial_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_final_divide_initial_anova)) # p-value = 1.908e-06 

# Bartlett test - homogeneity of variances 
# Null hypothesis: equal variances
bartlett.test(final_divide_initial ~ treatment*nutrients, data=data_area_raw) # p-value = 0.1065


# Power transformation 
library(car)
powerTransform(final_divide_initial ~ nutrients*treatment, data=data_area_raw)
power <- -0.2009824 

# add the power transformation of area_stand
data_area_raw$power_final_divide_initial <- ((data_area_raw$final_divide_initial)^power - 1) / power 

# re-do-the anova 
anova_final_divide_initial_power <- aov(power_final_divide_initial ~ nutrients*treatment, data=data_area_raw)
summary(anova_final_divide_initial_power)
posthoc_anova_final_divide_initial_power <- TukeyHSD(anova_final_divide_initial_power)
posthoc_anova_final_divide_initial_power <- posthoc_anova_final_divide_initial_power$'nutrients:treatment'
significant <- subset(posthoc_anova_final_divide_initial_power, posthoc_anova_final_divide_initial_power[,4]<=0.050) # significant comparisons 
nonsignif <- subset(posthoc_anova_final_divide_initial_power, posthoc_anova_final_divide_initial_power[,4]>0.050) # non-significant comparisons 

# Examine residuals #
# plot a histogram 
# looks normal-ish
hist(resid(anova_final_divide_initial_power),xlab="Residuals",main=NULL)

# QQ plot 
# Does not look very normal 
qqnorm(resid(anova_final_divide_initial_power),main=NULL) 
qqline(resid(anova_final_divide_initial_power)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(anova_final_divide_initial_power)) # p-value = 0.4453

bartlett.test(power_final_divide_initial ~ nutrients*treatment, data=data_area_raw)





############################ 
#                          #  
# This old code below uses #
# diff. data frames        #
#                          #  
############################

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
# across all treatment combinations 
bartlett.test(power_area_stand ~ treatment*nutrients, data = data_area_10_TOT)
# within low nutrients
bartlett.test(subset(data_area_10_TOT$power_area_stand, data_area_10_TOT$nutrients=="low") ~ subset(data_area_10_TOT$treatment, data_area_10_TOT$nutrients=="low")) # p-value = 0.7878
# within high nutrients
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
hist(resid(anova_area_stand_TOT_power),xlab="Residuals",main=NULL)

# QQ plot 
# Does not look very normal 
qqnorm(resid(anova_area_stand_TOT_power),main=NULL) 
qqline(resid(anova_area_stand_TOT_power)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(anova_area_stand_TOT_power)) # p-value = 0.4362

# Residuals are actually normal!!!!!
