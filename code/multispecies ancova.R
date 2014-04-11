###############################
# Two-way ancova              #
# Y = total area day 10       #
# Covariate: total area day 0 #
# Nutrients * sp. treatment   #
###############################

######################
# Modify data frames #
######################
# create a dataframe that has both area_0 and area_10 as a column (variable)
# I am going to add this column to data_area_10
data_area_10$area_0 <- subset(data_area$area_mm2, data_area$day==0)
data_area_10_TOT <- subset(data_area_10, data_area_10$species == "TOT")
colnames(data_area_10_TOT)[8]<-"area_10"
head(data_area_10_TOT)

############################
# Should I use an ANCOVA?  #
# (as opposed to an ANOVA) #
############################
# visualize the relationship between initial area (day 0) and final area (day 10) 
# There appears to  be a relationship between initial and final area 

# low nutrients
plot(subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients=="low"), subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="low"))
# high nutrients
plot(subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients=="high"), subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="high"))

# BUT this relationship does not appear to be similar in each treatment combination!


library(ggplot2)
area0_area10_regression <- ggplot(data=data_area_10_TOT,aes(x=area_0,y=area_10)) + geom_point(size=3)
area0_area10_regression <- area0_area10_regression + ylab("area day 10 (sq. mm)") + xlab("area day 0 (sq.mm)")
area0_area10_regression <- area0_area10_regression + facet_grid(nutrients ~ treatment)                   
area0_area10_regression <- area0_area10_regression + stat_smooth(method=lm)
area0_area10_regression

# regress each treatment combination 
nutrients<-c("low","high")
treatment<-c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB")
for (i in 1:length(nutrients)){
  for (j in 1:length(treatment)){       
    
    jpeg(paste(i,j,".jpg",sep=""), width = 250, height = 250, units = "px", quality = 100)
    
    temp <- plot(subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients==nutrients[i]& data_area_10_TOT$treatment ==  treatment[j]), 
         subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients==nutrients[i]& data_area_10_TOT$treatment ==  treatment[j]),
         main=paste(nutrients[i],treatment[j]),
         ylab = "area day 10 (sq. mm)",
         xlab = "area day 0 (sq. mm)")
    
    dev.off()
  }
}


nutrients<-c("low","high")
treatment<-c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB")
# calculate the slope (and its SE) for each treatment combination (14 total)
temp<-matrix(data=rep(0,3*14),nrow=14,ncol=3)
temp<-as.data.frame(temp)
colnames(temp)<-c("treatment","slope","SE")
temp$treatment <- c("low LM","low SP","low WB","low LMSP","low LMWB","low SPWB","low LMSPWB",
                    "high LM","high SP","high WB","high LMSP","high LMWB","high SPWB","high LMSPWB")
temp

for (i in 1:length(nutrients)){
  for (j in 1:length(treatment)){       
    temp2 <- lm(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients==nutrients[i]& data_area_10_TOT$treatment ==  treatment[j]) ~ 
         subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients==nutrients[i]& data_area_10_TOT$treatment ==  treatment[j]), 
       data = data_area_10_TOT)    
    temp$slope[i*j] <- coef(summary(temp2))[2,1]
    temp$SE[i*j] <- coef(summary(temp2))[2,2]
  } 
}

# Still need to do the actual comparison between the two 
# The standard error of the difference is computed as the square root of the sum of squared standard errors 
# Dividing the obtained t stat (diff. btwn. slopes) by the standard error equals 0.0065/0.0049 = 1.3. 
# We therefore have no reason to suspect the slopes to be different, and can proceed with the ANCOVA.


###############################
# Test parametric assumptions #
###############################
# Histograms to look at normality 

# Histogram - by nutrient treatment - things look pretty normal 
hist(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="high"))
hist(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="low"))

# Histogram - by nutrient*species treatment combination 
# (this is going to be difficult to be normal with only n=6)
nutrients<-c("low","high")
treatment<-c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB")
HIST<-list()
for (i in 1:length(nutrients)){
  for (j in 1:length(treatment)){
    HIST[i*j]<-hist(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients == nutrients[i] & data_area_10_TOT$treatment ==  treatment[j]),main=paste(nutrients[i],treatment[j]))
  }
}

# QQ plot - across everything 
qqnorm(data_area_10_TOT$area_10)
qqline(data_area_10_TOT$area_10)

# QQ plot - by nutrient treatment 
qqnorm(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="low"))
qqline(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="low"))

qqnorm(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="high"))
qqline(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="high"))

# QQ plot - by nutrient*species treatment combination 
# (this is going to be difficult to be normal with only n=6)
nutrients<-c("low","high")
treatment<-c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB")
QQ<-list()
for (i in 1:length(nutrients)){
  for (j in 1:length(treatment)){
    qqnorm(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients == nutrients[i] & data_area_10_TOT$treatment ==  treatment[j]),main=paste(nutrients[i],treatment[j]))
    qqline(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients == nutrients[i] & data_area_10_TOT$treatment ==  treatment[j]))
  }
}

# Shapiro-Wilk test - assess normality - across everything
# null hypothesis = sample came from a normally distributed population 
shapiro.test(data_area_10_TOT$area_10)

# Shapiro-Wilk test - assess normality - by nutrient treatment 
# null hypothesis = sample came from a normally distributed population 
shapiro.test(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="low"))
shapiro.test(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="high"))

# Shapiro-Wilk test - assess normality - by nutrient*species treatment combination 
# null hypothesis = sample came from a normally distributed population 
nutrients<-c("low","high")
treatment<-c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB")
shapiro<-list()
for (i in 1:length(nutrients)){
  for (j in 1:length(treatment)){
    temp <- shapiro.test(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients == nutrients[i] & data_area_10_TOT$treatment ==  treatment[j]))
    shapiro[i*j] <- temp[2]
  }
}

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal 
bartlett.test(data_area_10_TOT$area_10~data_area_10_TOT$nutrients) # p-value = 2.982e-14

bartlett.test(data_area_10_TOT$area_10 ~ data_area_10_TOT$nutrients * data_area_10_TOT$treatment) # p-value = 2.982e-14

bartlett.test(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="low") ~ subset(data_area_10_TOT$treatment, data_area_10_TOT$nutrients=="low")) # p-value = 0.01032

bartlett.test(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="high") ~ subset(data_area_10_TOT$treatment, data_area_10_TOT$nutrients=="high")) # p-value = 0.001272


#################
# Do the ANCOVA #
#################
ancova_area_10_TOT <- aov(area_10 ~ nutrients*treatment+area_0, data=data_area_10_TOT)
summary(ancova_area_10_TOT)
posthoc_ancova_area_10_TOT <- TukeyHSD(ancova_area_10_TOT)
posthoc_ancova_area_10_TOT <- posthoc_ancova_area_10_TOT$'nutrients:treatment'
significant <- subset(posthoc_ancova_area_10_TOT, posthoc_ancova_area_10_TOT[,4]<=0.050) # significant comparisons 
nonsignif <- subset(posthoc_ancova_area_10_TOT, posthoc_ancova_area_10_TOT[,4]>0.050) # non-significant comparisons 
