#################################################
# Analysis of multispecies duckweed experiment  #
# Summer 2013                                   #
#                                               #
# Day 0 area                                    # 
# Total within a well - not species-specific    #
#################################################
library(ggplot2)
library(gridExtra)

# check out the data that you will use 
head(data_area)
head(summary_data_area)

###########################
# Mean area at day 0      #
# By each treatment combo #
###########################
mean_area_day0_plot <- ggplot(subset(summary_data_area, summary_data_area$day == 0 & species == "TOT"), aes(x=treatment, y=area,shape=nutrients)) 
mean_area_day0_plot <- mean_area_day0_plot + geom_errorbar(aes(ymin=area-se, ymax=area+se), width=0.1)
mean_area_day0_plot <- mean_area_day0_plot + geom_point(size=3)
mean_area_day0_plot <- mean_area_day0_plot + ylab("initial area (sq. mm)")
mean_area_day0_plot <- mean_area_day0_plot + xlab("species")
mean_area_day0_plot <- mean_area_day0_plot + theme_classic(base_size=18)
mean_area_day0_plot

#####################
# Preliminary anova #
# TWo-way           #
#####################
# Y = Area day 0
# Treatments: species, nitrogen, phosphorus       
area_day0_anova <- aov(area_total ~ treatment*nutrients, data=subset(data_area, data_area$day == 0 & species == "TOT"))
summary(area_day0_anova)
# pair-wise interactions 
TukeyHSD(area_day0_anova)[3]

#####################
# Examine residuals #
#####################
hist(resid(area_day0_anova)) # plot a histogram 

qqnorm(resid(area_day0_anova)) # QQ plot 
qqline(resid(area_day0_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_day0_anova)) # p-value = 0.755 


