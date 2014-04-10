###########################################
# Two-way anova                           #
# Y = total areaday 0                     #
# Nutrients * sp. treatment               #
#                                         #
# Transformed Y variable below            #
###########################################

######################
# Modify data frames #
######################
head(data_area_TOT)
data_area_TOT_day0 <- subset(data_area_TOT, data_area_TOT$day == 0)
head(data_area_TOT_day0)

###############################
# Test parametric assumptions #
###############################
# Histograms to look at normality 

# Histogram - by nutrient treatment 
# things look almost normal 
hist(subset(data_area_TOT_day0$area_total, data_area_TOT_day0$nutrients=="high"))
hist(subset(data_area_TOT_day0$area_total, data_area_TOT_day0$nutrients=="low"))

# Histogram - by nutrient*species treatment combination 
# (this is going to be difficult to be normal with only n=6)
nutrients<-c("low","high")
treatment<-c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB")
HIST<-list()
for (i in 1:length(nutrients)){
  for (j in 1:length(treatment)){
    HIST[i*j]<-hist(subset(data_area_TOT_day0$area_total, data_area_TOT_day0$nutrients == nutrients[i] & data_area_TOT_day0$treatment ==  treatment[j]),main=paste(nutrients[i],treatment[j]))
  }
}

# QQ plot - across everything 
qqnorm(data_area_TOT_day0$area_total)
qqline(data_area_TOT_day0$area_total)

# QQ plot - by nutrient treatment 
qqnorm(subset(data_area_TOT_day0$area_total, data_area_TOT_day0$nutrients=="low"))
qqline(subset(data_area_TOT_day0$area_total, data_area_TOT_day0$nutrients=="low"))

qqnorm(subset(data_area_TOT_day0$area_total, data_area_TOT_day0$nutrients=="high"))
qqline(subset(data_area_TOT_day0$area_total, data_area_TOT_day0$nutrients=="high"))

# QQ plot - by nutrient*species treatment combination 
# (this is going to be difficult to be normal with only n=6)
nutrients<-c("low","high")
treatment<-c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB")
QQ<-list()
for (i in 1:length(nutrients)){
  for (j in 1:length(treatment)){
    qqnorm(subset(data_area_TOT_day0$area_total, data_area_TOT_day0$nutrients == nutrients[i] & data_area_TOT_day0$treatment ==  treatment[j]),main=paste(nutrients[i],treatment[j]))
    qqline(subset(data_area_TOT_day0$area_total, data_area_TOT_day0$nutrients == nutrients[i] & data_area_TOT_day0$treatment ==  treatment[j]))
  }
}

# Shapiro-Wilk test - assess normality - across everything
# null hypothesis = sample came from a normally distributed population 
shapiro.test(data_area_TOT_day0$area_total)

# Shapiro-Wilk test - assess normality - by nutrient treatment 
# null hypothesis = sample came from a normally distributed population 
shapiro.test(subset(data_area_TOT_day0$area_total, data_area_TOT_day0$nutrients=="low"))
shapiro.test(subset(data_area_TOT_day0$area_total, data_area_TOT_day0$nutrients=="high"))

# Shapiro-Wilk test - assess normality - by nutrient*species treatment combination 
# null hypothesis = sample came from a normally distributed population 
nutrients<-c("low","high")
treatment<-c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB")
shapiro<-list()
for (i in 1:length(nutrients)){
  for (j in 1:length(treatment)){
    temp <- shapiro.test(subset(data_area_TOT_day0$area_total, data_area_TOT_day0$nutrients == nutrients[i] & data_area_TOT_day0$treatment ==  treatment[j]))
    shapiro[i*j] <- temp[2]
  }
}

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal 
bartlett.test(data_area_TOT_day0$area_total~data_area_TOT_day0$nutrients) # p-value = 2.2e-16

bartlett.test(data_area_TOT_day0$area_total ~ data_area_TOT_day0$nutrients * data_area_TOT_day0$treatment) # p-value = 2.2e-16

bartlett.test(subset(data_area_TOT_day0$area_total, data_area_TOT_day0$nutrients=="low") ~ subset(data_area_TOT_day0$treatment, data_area_TOT_day0$nutrients=="low")) # p-value = 0.9308

bartlett.test(subset(data_area_TOT_day0$area_total, data_area_TOT_day0$nutrients=="high") ~ subset(data_area_TOT_day0$treatment, data_area_TOT_day0$nutrients=="high")) # p-value = 0.5795


################
# Do the ANOVA #
################
anova_area_total_day0 <- aov(area_total ~ nutrients*treatment, data=data_area_TOT_day0)
summary(anova_area_total_day0)
posthoc_anova_area_total_day0 <- TukeyHSD(anova_area_total_day0)
posthoc_anova_area_total_day0 <- posthoc_anova_area_total_day0$'nutrients:treatment'
significant <- subset(posthoc_anova_area_total_day0, posthoc_anova_area_total_day0[,4]<=0.050) # significant comparisons 
nonsignif <- subset(posthoc_anova_area_total_day0, posthoc_anova_area_total_day0[,4]>0.050) # non-significant comparisons 

#####################
# Examine residuals #
#####################
# plot a histogram 
# looks normal-ish
hist(resid(anova_area_total_day0))

# QQ plot 
# Does not look very normal 
qqnorm(resid(anova_area_total_day0)) 
qqline(resid(anova_area_total_day0)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(anova_area_total_day0)) # p-value = 0.755
# Residuals are noramlly distributed 


