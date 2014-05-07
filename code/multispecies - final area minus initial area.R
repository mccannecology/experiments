#################################################
# Analysis of multispecies duckweed experiment  #
# Summer 2013                                   #
#                                               #
# Final Area - Initial Area                     #
# Total within a well - not species-specific    #
#################################################

library(ggplot2)

# check out the data that you will use 
head(data_area_raw)
head(summary_data_area_final_minus_initial)

###########################
# Mean area at day 0      #
# By each treatment combo #
###########################
mean_area_final_minus_initial_plot <- ggplot(summary_data_area_final_minus_initial, aes(x=treatment, y=final_minus_initial)) 
mean_area_final_minus_initial_plot <- mean_area_final_minus_initial_plot + geom_errorbar(aes(ymin=final_minus_initial-se, ymax=final_minus_initial+se), width=0.1)
mean_area_final_minus_initial_plot <- mean_area_final_minus_initial_plot + geom_point(size=3)
mean_area_final_minus_initial_plot <- mean_area_final_minus_initial_plot + facet_grid(nutrients ~ .)
mean_area_final_minus_initial_plot <- mean_area_final_minus_initial_plot + ylab("final area - initial area")
mean_area_final_minus_initial_plot <- mean_area_final_minus_initial_plot + xlab("species treatment")
mean_area_final_minus_initial_plot <- mean_area_final_minus_initial_plot + theme_gray(base_size=18)
mean_area_final_minus_initial_plot

#####################
# Preliminary anova #
# Three-way         #
#####################
area_final_minus_initial_anova <- aov(final_minus_initial ~ treatment*nutrients, data=data_area_raw)
summary(area_final_minus_initial_anova)
TukeyHSD(area_final_minus_initial_anova)

#####################
# Examine residuals #
#####################
hist(resid(area_final_minus_initial_anova)) # plot a histogram 

qqnorm(resid(area_final_minus_initial_anova)) # QQ plot 
qqline(resid(area_final_minus_initial_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_final_minus_initial_anova)) # p-value = 2.349e-10 




#################################
# transform and re-do the anova #
#################################



#############################
# try a sqrt transformation #
#############################
sqrt_area_final_minus_initial_anova <- aov(sqrt(final_minus_initial) ~ treatment*nutrients, data=data_area_raw)
summary(sqrt_area_final_minus_initial_anova)
posthoc_sqrt_area_final_minus_initial_anova<- TukeyHSD(sqrt_area_final_minus_initial_anova)
posthoc_sqrt_area_final_minus_initial_anova

#####################
# Examine residuals #
#####################
hist(resid(sqrt_area_final_minus_initial_anova)) # plot a histogram 

qqnorm(resid(sqrt_area_final_minus_initial_anova)) # QQ plot 
qqline(resid(sqrt_area_final_minus_initial_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(sqrt_area_final_minus_initial_anova)) # p-value =  1.79e-05




#################################
# transform and re-do the anova #
#################################



###############################
# try a logx+1 transformation #
###############################
logx1_area_final_minus_initial_anova <- aov(log(final_minus_initial+1) ~ treatment*nutrients, data=data_area_raw)
summary(logx1_area_final_minus_initial_anova)
posthoc_logx1_area_final_minus_initial_anova <- TukeyHSD(logx1_area_final_minus_initial_anova)
posthoc_logx1_area_final_minus_initial_anova[7]

#####################
# Examine residuals #
#####################
hist(resid(logx1_area_final_minus_initial_anova)) # plot a histogram 

qqnorm(resid(logx1_area_final_minus_initial_anova)) # QQ plot 
qqline(resid(logx1_area_final_minus_initial_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(logx1_area_final_minus_initial_anova)) # p-value = 3.828e-10



#########################################
# convert any (-) values or 0s to 0.001 #
#########################################
data_area_raw$final_minus_initial[data_area_raw$final_minus_initial < 0] <- 0 
data_area_raw$final_minus_initial[data_area_raw$final_minus_initial == 0] <- 0.001 


##############################
# Try a power transformation #
##############################
# figure out the best power transformation 
library(car)
powerTransform(final_minus_initial ~ nutrients*treatment, data=data_area_raw)
summary(powerTransform(area_stand ~ nutrients*treatment, data=data_area_raw))
power <- 0.2924576 

# add the power transformation of area_stand
data_area_raw["power_final_minus_initial"] <- ((data_area_raw$final_minus_initial)^power - 1) / power 

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal 
# across all treatment combinations 
bartlett.test(power_final_minus_initial ~ treatment*nutrients, data = data_area_raw)
# within low nutrients
bartlett.test(subset(data_area_raw$power_final_minus_initial, data_area_raw$nutrients=="low") ~ subset(data_area_raw$treatment, data_area_raw$nutrients=="low")) # p-value < 2.2e-16
# within high nutrients
bartlett.test(subset(data_area_raw$power_final_minus_initial, data_area_raw$nutrients=="high") ~ subset(data_area_raw$treatment, data_area_raw$nutrients=="high")) # p-value < 2.2e-16

# re-do-the anova 
anova_area_stand_TOT_power <- aov(power_final_minus_initial ~ nutrients*treatment, data=data_area_raw)
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
shapiro.test(resid(anova_area_stand_TOT_power)) # p-value = 0.001137


