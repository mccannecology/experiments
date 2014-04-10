#########################################################
# Alternatives to ANCOVA                                #
# Non-parametric analysis of covariance                 #  
# R package: fANCOVA                                    #
#                                                       #
# Test equality of curves based on ANOVA-type statistic #
#########################################################
# Need to figure out what is "non-parametric" about this 
# does it allow for heterogeneous regression slopes?
# or non-normality? 

library(fANCOVA)

###################################
# Real Data                       #
# Y = Area day 10                 #
# X = Area day 0                  #
# group = treatment combination   #
###################################
head(data_area_10_TOT)

# arrange group labels based on combination of species and treatment 
data_area_10_TOT$group <- paste(data_area_10_TOT$treatment, data_area_10_TOT$nutrients,sep=" ")
# make group labels that are numeric 
l=unique(as.character(data_area_10_TOT$group))
data_area_10_TOT$group01 <- as.numeric(factor(data_area_10_TOT$group, levels=l))
data_area_10_TOT$group
data_area_10_TOT$group01

# Group labels 
# 1 = "LM low"      
# 2 = "SP low"      
# 3 = "LM high"     
# 4 = "SP high"     
# 5 = "WB high"     
# 6 = "LMSP high"   
# 7 = "LMWB high"   
# 8 = "WB low"      
# 9 = "LMSP low"    
# 10 = "LMWB low"    
# 11 = "SPWB low"    
# 12 = "LMSPWB low" 
# 13 = "SPWB high"   
# 14 = "LMSPWB high"

# run the nonparametric ancova 
nonpar_ancova <- T.aov(data_area_10_TOT$area_0, data_area_10_TOT$area_10, data_area_10_TOT$group01)
# more than 50 warnings 
# fewer data values than degrees of freedom 

nonpar_ancova
# Test the equality of curves based on an ANOVA-type statistic
# Comparing 14 nonparametric regression curves 
# Local polynomial regression with automatic smoothing parameter selection via AICC is used for curve fitting. 
# Wide-bootstrap algorithm is applied to obtain the null distribution. 
# 
# Null hypothesis: there is no difference between the 14 curves.
# T =  4.787e+04     p-value =  0.1692

# I still want each pairwise compariso nf

##############
#  EXAMPLE   #
##############
## Nonparametric test the equality of multiple regression curves
## Simulate data sets
n1 <- 100
x1 <- runif(n1,min=0, max=3)
sd1 <- 0.2
e1 <- rnorm(n1,sd=sd1)
y1 <- sin(2*x1) + e1

n2 <- 100
x2 <- runif(n2, min=0, max=3)
sd2 <- 0.25
e2 <- rnorm(n2, sd=sd2)
y2 <- sin(2*x2) + 1 + e2

n3 <- 120
x3 <- runif(n3, min=0, max=3)
sd3 <- 0.25
e3 <- rnorm(n3, sd=sd3)
y3 <- sin(2*x3) + e3

data.bind <- rbind(cbind(x1,y1,1), cbind(x2,y2,2),cbind(x3,y3,3))
data.bind <- data.frame(data.bind)
colnames(data.bind)=c('x','y','group')

t1 <- T.aov(data.bind$x, data.bind$y, data.bind$group)
t1
plot(t1)
plot(t1, test.statistic=FALSE)

# cleanup your workspace
rm(n1,x1,sd1,e1,y1)
rm(n2,x2,sd2,e2,y2)
rm(n3,x3,sd3,e3,y3)
rm(t1)
