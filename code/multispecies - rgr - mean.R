#################################################
# Analysis of multispecies duckweed experiment  #
# Summer 2013                                   #
#                                               #
# avgRGR                                        #
# average within a replicate                    #
# All spp. within a well - not species-specific #
#################################################
library(ggplot2)

# check out the data you will use
head(data_rgr)
head(summary_data_rgr_by_day)
head(summary_data_avgRGR)

############
# avgRGR   #
# Average  #
############
summary_data_avgRGR <- read.csv("avgRGR_posthoc.csv")
summary_data_avgRGR$nutrients <- factor(summary_data_avgRGR$nutrients , levels=c("low","high"))
summary_data_avgRGR$treatment <- factor(summary_data_avgRGR$treatment , levels=c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB"))

mean_avgRGR_plot <- ggplot(summary_data_avgRGR, aes(x=treatment,y=avgRGR,shape=nutrients)) + geom_point(size=3) 
mean_avgRGR_plot <- mean_avgRGR_plot + geom_errorbar(aes(ymin=avgRGR-se, ymax=avgRGR+se), width=0.1)
mean_avgRGR_plot <- mean_avgRGR_plot + ylab("Average RGR (all species combined)")
mean_avgRGR_plot <- mean_avgRGR_plot + xlab("Species combination")
mean_avgRGR_plot <- mean_avgRGR_plot + theme_classic(base_size=18)
mean_avgRGR_plot <- mean_avgRGR_plot + geom_text(data=summary_data_avgRGR,aes(x=treatment, y=avgRGR+se+0.01,label=label))
mean_avgRGR_plot 

ggsave(filename="mean_avgRGR_plot.jpg",mean_avgRGR_plot,height=8,width=11)


#####################
# Preliminary anova #
# Y = avgRGR        #
# Total             #
# Treatment         #
# Nutrients         #
#####################
avgRGR_anova <- aov(avgRGR ~ treatment*nutrients, data=data)
summary(avgRGR_anova)
posthoc_avgRGR_anova <- TukeyHSD(avgRGR_anova)
posthoc_avgRGR_anova <- posthoc_avgRGR_anova$'treatment:nutrients'
significant <- subset(posthoc_avgRGR_anova, posthoc_avgRGR_anova[,4]<=0.050) # significant comparisons 
nonsignif <- subset(posthoc_avgRGR_anova, posthoc_avgRGR_anova[,4]>0.050) # non-significant comparisons 

#####################
# Examine residuals #
#####################
# plot a histogram 
# looks normal-ish
hist(resid(avgRGR_anova))

# QQ plot 
# Does not look very normal 
qqnorm(resid(avgRGR_anova)) 
qqline(resid(avgRGR_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_anova)) # p-value = 0.3773 

# Bartlett test - homogeneity of variances 
# Null hypothesis: equal variances
bartlett.test(avgRGR ~ treatment*nutrients, data=data) # p-value = 0.9149
