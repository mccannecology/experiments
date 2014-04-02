###########################################
# Repeated measures, two-way anova        #
# Y = total area day 10/ total area day 0 #
# Nutrients * sp. treatment               #
#                                         #
# Transformed Y variable below            #
###########################################

######################
# Modify data frames #
######################
head(data_area)
# this data frame has area_stand, the response variable of interest 
# but I will only species=="TOT"

data_area_TOT <- subset(data_area, data_area$species=="TOT")

###############################
# Test parametric assumptions #
###############################
# Histograms to look at normality 

# Histogram - by nutrient treatment 
# Things don't look too normal because there are a lot of 1s (the first day)
hist(subset(data_area_TOT$area_stand, data_area_TOT$nutrients=="high"))
hist(subset(data_area_TOT$area_stand, data_area_TOT$nutrients=="low"))

# Histogram - by nutrient*species treatment combination 
# (this is going to be difficult to be normal with only n=6)
# Again, lots of 1s (first day)
nutrients<-c("low","high")
treatment<-c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB")
HIST<-list()
for (i in 1:length(nutrients)){
  for (j in 1:length(treatment)){
    HIST[i*j]<-hist(subset(data_area_TOT$area_stand, data_area_TOT$nutrients == nutrients[i] & data_area_TOT$treatment ==  treatment[j]),main=paste(nutrients[i],treatment[j]))
  }
}

# QQ plot - across everything 
qqnorm(data_area_TOT$area_stand)
qqline(data_area_TOT$area_stand)

# QQ plot - by nutrient treatment 
qqnorm(subset(data_area_TOT$area_stand, data_area_TOT$nutrients=="low"))
qqline(subset(data_area_TOT$area_stand, data_area_TOT$nutrients=="low"))

qqnorm(subset(data_area_TOT$area_stand, data_area_TOT$nutrients=="high"))
qqline(subset(data_area_TOT$area_stand, data_area_TOT$nutrients=="high"))

# QQ plot - by nutrient*species treatment combination 
# (this is going to be difficult to be normal with only n=6)
nutrients<-c("low","high")
treatment<-c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB")
QQ<-list()
for (i in 1:length(nutrients)){
  for (j in 1:length(treatment)){
    qqnorm(subset(data_area_TOT$area_stand, data_area_TOT$nutrients == nutrients[i] & data_area_TOT$treatment ==  treatment[j]),main=paste(nutrients[i],treatment[j]))
    qqline(subset(data_area_TOT$area_stand, data_area_TOT$nutrients == nutrients[i] & data_area_TOT$treatment ==  treatment[j]))
  }
}

# Shapiro-Wilk test - assess normality - across everything
# null hypothesis = sample came from a normally distributed population 
shapiro.test(data_area_TOT$area_stand)

# Shapiro-Wilk test - assess normality - by nutrient treatment 
# null hypothesis = sample came from a normally distributed population 
shapiro.test(subset(data_area_TOT$area_stand, data_area_TOT$nutrients=="low"))
shapiro.test(subset(data_area_TOT$area_stand, data_area_TOT$nutrients=="high"))

# Shapiro-Wilk test - assess normality - by nutrient*species treatment combination 
# null hypothesis = sample came from a normally distributed population 
nutrients<-c("low","high")
treatment<-c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB")
shapiro<-list()
for (i in 1:length(nutrients)){
  for (j in 1:length(treatment)){
    temp <- shapiro.test(subset(data_area_TOT$area_stand, data_area_TOT$nutrients == nutrients[i] & data_area_TOT$treatment ==  treatment[j]))
    shapiro[i*j] <- temp[2]
  }
}
shapiro

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal 
bartlett.test(data_area_TOT$area_stand~data_area_TOT$nutrients) # p-value = 2.2e-16

bartlett.test(data_area_TOT$area_stand ~ data_area_TOT$nutrients * data_area_TOT$treatment) # p-value = 2.2e-16

bartlett.test(subset(data_area_TOT$area_stand, data_area_TOT$nutrients=="low") ~ subset(data_area_TOT$treatment, data_area_TOT$nutrients=="low")) # p-value = 5.904e-05

bartlett.test(subset(data_area_TOT$area_stand, data_area_TOT$nutrients=="high") ~ subset(data_area_TOT$treatment, data_area_TOT$nutrients=="high")) # p-value = 4.643e-06


################
# Do the ANOVA #
################
rm_anova_area_stand_TOT <- aov(area_stand ~ nutrients*treatment*day, data=data_area_TOT)
summary(rm_anova_area_stand_TOT)
posthoc_rm_anova_area_stand_TOT <- TukeyHSD(rm_anova_area_stand_TOT)
posthoc_rm_anova_area_stand_TOT <- posthoc_rm_anova_area_stand_TOT$'nutrients:treatment'
significant <- subset(posthoc_rm_anova_area_stand_TOT, posthoc_rm_anova_area_stand_TOT[,4]<=0.050) # significant comparisons 
nonsignif <- subset(posthoc_rm_anova_area_stand_TOT, posthoc_rm_anova_area_stand_TOT[,4]>0.050) # non-significant comparisons 

#####################
# Examine residuals #
#####################
# plot a histogram 
# looks normal-ish
hist(resid(rm_anova_area_stand_TOT))

# QQ plot 
# Does not look very normal 
qqnorm(resid(rm_anova_area_stand_TOT)) 
qqline(resid(rm_anova_area_stand_TOT)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(rm_anova_area_stand_TOT)) # p-value = 4.105e-06




