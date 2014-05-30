#################################################
# Analysis of multispecies duckweed experiment  #
# Summer 2013                                   #
#                                               #
# maxRGR                                        #
# max within a replicate                        #
# All spp. within a well - not species-specific #
#################################################
library(ggplot2)

# check out the data you will use
head(data_rgr)
head(summary_data_maxRGR)

############
# maxRGR   #
# Average  #
############
summary_data_maxRGR <- read.csv("maxRGR_posthoc.csv")
summary_data_maxRGR$nutrients <- factor(summary_data_maxRGR$nutrients , levels=c("low","high"))
summary_data_maxRGR$treatment <- factor(summary_data_maxRGR$treatment , levels=c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB"))

mean_maxRGR_plot <- ggplot(summary_data_maxRGR, aes(x=treatment,y=maxRGR,shape=nutrients)) + geom_point() 
mean_maxRGR_plot <- mean_maxRGR_plot + geom_errorbar(aes(ymin=maxRGR-se, ymax=maxRGR+se), width=0.1)
mean_maxRGR_plot <- mean_maxRGR_plot + ylab("Maximum RGR (all species combined)")
mean_maxRGR_plot <- mean_maxRGR_plot + theme_classic(base_size=18)
mean_maxRGR_plot <- mean_maxRGR_plot + geom_text(data=summary_data_maxRGR,aes(x=treatment, y=maxRGR+se+0.01,label=label))
mean_maxRGR_plot 

ggsave(filename="mean_maxRGR_plot.jpg",mean_maxRGR_plot,height=8,width=11)

#####################
# Preliminary anova #
# Y = maxRGR        #
# total             #
# all spp. combined # 
# Treatments        #
# nutrients         #
#####################
maxRGR_anova <- aov(maxRGR ~ treatment*nutrients, data=data)
summary(maxRGR_anova)
posthoc_maxRGR_anova <- TukeyHSD(maxRGR_anova)
posthoc_maxRGR_anova <- posthoc_maxRGR_anova$'treatment:nutrients'
significant <- subset(posthoc_maxRGR_anova, posthoc_maxRGR_anova[,4]<=0.050) # significant comparisons 
nonsignif <- subset(posthoc_maxRGR_anova, posthoc_maxRGR_anova[,4]>0.050) # non-significant comparisons 

#####################
# Examine residuals #
#####################
# plot a histogram 
# looks normal-ish
hist(resid(maxRGR_anova))

# QQ plot 
# Does not look very normal 
qqnorm(resid(maxRGR_anova)) 
qqline(resid(maxRGR_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_anova)) # p-value = 0.2728 

# Bartlett test - homogeneity of variances 
# Null hypothesis: equal variances
bartlett.test(maxRGR ~ treatment*nutrients, data=data) # p-value = 0.9864




