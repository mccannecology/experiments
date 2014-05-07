#################################################
# Alternatives to ANCOVA                        # 
# Wilcoxon modification for Johnson-Neyman      # 
# Quinn & Keough Box 12.4                       #
#################################################
head(data_area_10_TOT)

nutrients<-c("low","high")
treatment<-c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB")

#######################
# Group01: low LM     #
#######################
# mean of covariate (area 0)
mean01 <- mean(subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "low" & data_area_10_TOT$treatment == "LM"))


# SS of covariate (area 0)
SS01 <- sum((subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "low" & data_area_10_TOT$treatment == "LM") - mean01)^2)

# a linear model for this group 
temp <- lm(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="low" & data_area_10_TOT$treatment ==  "LM") ~ 
     subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients=="low" & data_area_10_TOT$treatment ==  "LM"), 
   data = data_area_10_TOT)    

# intercept 
intercept01 <- coef(summary(temp))[1,1]

# slope 
slope01 <- coef(summary(temp))[2,1]

# residual SS of the regression 
RSS01 <- sum(resid(temp)^2)
#######################
# Group02: low SP     #
#######################
# mean of covariate (area 0)
mean02 <- mean(subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "low" & data_area_10_TOT$treatment == "SP"))


# SS of covariate (area 0)
SS02 <- sum((subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "low" & data_area_10_TOT$treatment == "SP") - mean02)^2)

# a linear model for this group 
temp <- lm(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="low" & data_area_10_TOT$treatment ==  "SP") ~ 
             subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients=="low" & data_area_10_TOT$treatment ==  "SP"), 
           data = data_area_10_TOT)    

# intercept 
intercept02 <- coef(summary(temp))[1,1]

# slope 
slope02 <- coef(summary(temp))[2,1]

# residual SS of the regression 
RSS02 <- sum(resid(temp)^2)
#######################
# Group03: low WB     #
#######################
# mean of covariate (area 0)
mean03 <- mean(subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "low" & data_area_10_TOT$treatment == "WB"))


# SS of covariate (area 0)
SS03 <- sum((subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "low" & data_area_10_TOT$treatment == "WB") - mean03)^2)

# a linear model for this group 
temp <- lm(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="low" & data_area_10_TOT$treatment ==  "WB") ~ 
             subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients=="low" & data_area_10_TOT$treatment ==  "WB"), 
           data = data_area_10_TOT)    

# intercept 
intercept03 <- coef(summary(temp))[1,1]

# slope 
slope03 <- coef(summary(temp))[2,1]

# residual SS of the regression 
RSS03 <- sum(resid(temp)^2)
#######################
# Group04: low LMSP   #
#######################
# mean of covariate (area 0)
mean04 <- mean(subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "low" & data_area_10_TOT$treatment == "LMSP"))


# SS of covariate (area 0)
SS04 <- sum((subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "low" & data_area_10_TOT$treatment == "LMSP") - mean04)^2)

# a linear model for this group 
temp <- lm(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="low" & data_area_10_TOT$treatment ==  "LMSP") ~ 
             subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients=="low" & data_area_10_TOT$treatment ==  "LMSP"), 
           data = data_area_10_TOT)    

# intercept 
intercept04 <- coef(summary(temp))[1,1]

# slope 
slope04 <- coef(summary(temp))[2,1]

# residual SS of the regression 
RSS04 <- sum(resid(temp)^2)
#######################
# Group05: low LMWB   #
#######################
# mean of covariate (area 0)
mean05 <- mean(subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "low" & data_area_10_TOT$treatment == "LMWB"))


# SS of covariate (area 0)
SS05 <- sum((subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "low" & data_area_10_TOT$treatment == "LMWB") - mean05)^2)

# a linear model for this group 
temp <- lm(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="low" & data_area_10_TOT$treatment ==  "LMWB") ~ 
             subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients=="low" & data_area_10_TOT$treatment ==  "LMWB"), 
           data = data_area_10_TOT)    

# intercept 
intercept05 <- coef(summary(temp))[1,1]

# slope 
slope05 <- coef(summary(temp))[2,1]

# residual SS of the regression 
RSS05 <- sum(resid(temp)^2)
#######################
# Group06: low SPWB   #
#######################
# mean of covariate (area 0)
mean06 <- mean(subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "low" & data_area_10_TOT$treatment == "SPWB"))


# SS of covariate (area 0)
SS06 <- sum((subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "low" & data_area_10_TOT$treatment == "SPWB") - mean06)^2)

# a linear model for this group 
temp <- lm(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="low" & data_area_10_TOT$treatment ==  "SPWB") ~ 
             subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients=="low" & data_area_10_TOT$treatment ==  "SPWB"), 
           data = data_area_10_TOT)    

# intercept 
intercept06 <- coef(summary(temp))[1,1]

# slope 
slope06 <- coef(summary(temp))[2,1]

# residual SS of the regression 
RSS06 <- sum(resid(temp)^2)
#######################
# Group07: low LMSPWB #
#######################
# mean of covariate (area 0)
mean07 <- mean(subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "low" & data_area_10_TOT$treatment == "LMSPWB"))


# SS of covariate (area 0)
SS07 <- sum((subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "low" & data_area_10_TOT$treatment == "LMSPWB") - mean07)^2)

# a linear model for this group 
temp <- lm(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="low" & data_area_10_TOT$treatment ==  "LMSPWB") ~ 
             subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients=="low" & data_area_10_TOT$treatment ==  "LMSPWB"), 
           data = data_area_10_TOT)    

# intercept 
intercept07 <- coef(summary(temp))[1,1]

# slope 
slope07 <- coef(summary(temp))[2,1]

# residual SS of the regression 
RSS07 <- sum(resid(temp)^2)
#######################
# Group08: high LM     #
#######################
# mean of covariate (area 0)
mean08 <- mean(subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "high" & data_area_10_TOT$treatment == "LM"))


# SS of covariate (area 0)
SS08 <- sum((subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "high" & data_area_10_TOT$treatment == "LM") - mean08)^2)

# a linear model for this group 
temp <- lm(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="high" & data_area_10_TOT$treatment ==  "LM") ~ 
             subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients=="high" & data_area_10_TOT$treatment ==  "LM"), 
           data = data_area_10_TOT)    

# intercept 
intercept08 <- coef(summary(temp))[1,1]

# slope 
slope08 <- coef(summary(temp))[2,1]

# residual SS of the regression 
RSS08 <- sum(resid(temp)^2)
#######################
# Group09: high SP     #
#######################
# mean of covariate (area 0)
mean09 <- mean(subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "high" & data_area_10_TOT$treatment == "SP"))


# SS of covariate (area 0)
SS09 <- sum((subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "high" & data_area_10_TOT$treatment == "SP") - mean09)^2)

# a linear model for this group 
temp <- lm(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="high" & data_area_10_TOT$treatment ==  "SP") ~ 
             subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients=="high" & data_area_10_TOT$treatment ==  "SP"), 
           data = data_area_10_TOT)    

# intercept 
intercept09 <- coef(summary(temp))[1,1]

# slope 
slope09 <- coef(summary(temp))[2,1]

# residual SS of the regression 
RSS09 <- sum(resid(temp)^2)
#######################
# Group10: high WB     #
#######################
# mean of covariate (area 0)
mean10 <- mean(subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "high" & data_area_10_TOT$treatment == "WB"))


# SS of covariate (area 0)
SS10 <- sum((subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "high" & data_area_10_TOT$treatment == "WB") - mean10)^2)

# a linear model for this group 
temp <- lm(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="high" & data_area_10_TOT$treatment ==  "WB") ~ 
             subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients=="high" & data_area_10_TOT$treatment ==  "WB"), 
           data = data_area_10_TOT)    

# intercept 
intercept10 <- coef(summary(temp))[1,1]

# slope 
slope10 <- coef(summary(temp))[2,1]

# residual SS of the regression 
RSS10 <- sum(resid(temp)^2)
#######################
# Group11: high LMSP   #
#######################
# mean of covariate (area 0)
mean11 <- mean(subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "high" & data_area_10_TOT$treatment == "LMSP"))


# SS of covariate (area 0)
SS11 <- sum((subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "high" & data_area_10_TOT$treatment == "LMSP") - mean11)^2)

# a linear model for this group 
temp <- lm(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="high" & data_area_10_TOT$treatment ==  "LMSP") ~ 
             subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients=="high" & data_area_10_TOT$treatment ==  "LMSP"), 
           data = data_area_10_TOT)    

# intercept 
intercept11 <- coef(summary(temp))[1,1]

# slope 
slope11 <- coef(summary(temp))[2,1]

# residual SS of the regression 
RSS11 <- sum(resid(temp)^2)
#######################
# Group12: high LMWB   #
#######################
# mean of covariate (area 0)
mean12 <- mean(subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "high" & data_area_10_TOT$treatment == "LMWB"))


# SS of covariate (area 0)
SS12 <- sum((subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "high" & data_area_10_TOT$treatment == "LMWB") - mean12)^2)

# a linear model for this group 
temp <- lm(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="high" & data_area_10_TOT$treatment ==  "LMWB") ~ 
             subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients=="high" & data_area_10_TOT$treatment ==  "LMWB"), 
           data = data_area_10_TOT)    

# intercept 
intercept12 <- coef(summary(temp))[1,1]

# slope 
slope12 <- coef(summary(temp))[2,1]

# residual SS of the regression 
RSS12 <- sum(resid(temp)^2)
#######################
# Group13: high SPWB   #
#######################
# mean of covariate (area 0)
mean13 <- mean(subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "high" & data_area_10_TOT$treatment == "SPWB"))


# SS of covariate (area 0)
SS13 <- sum((subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "high" & data_area_10_TOT$treatment == "SPWB") - mean13)^2)

# a linear model for this group 
temp <- lm(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="high" & data_area_10_TOT$treatment ==  "SPWB") ~ 
             subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients=="high" & data_area_10_TOT$treatment ==  "SPWB"), 
           data = data_area_10_TOT)    

# intercept 
intercept13 <- coef(summary(temp))[1,1]

# slope 
slope13 <- coef(summary(temp))[2,1]

# residual SS of the regression 
RSS13 <- sum(resid(temp)^2)
#######################
# Group14: high LMSPWB #
#######################
# mean of covariate (area 0)
mean14 <- mean(subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "high" & data_area_10_TOT$treatment == "LMSPWB"))


# SS of covariate (area 0)
SS14 <- sum((subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients == "high" & data_area_10_TOT$treatment == "LMSPWB") - mean14)^2)

# a linear model for this group 
temp <- lm(subset(data_area_10_TOT$area_10, data_area_10_TOT$nutrients=="high" & data_area_10_TOT$treatment ==  "LMSPWB") ~ 
             subset(data_area_10_TOT$area_0, data_area_10_TOT$nutrients=="high" & data_area_10_TOT$treatment ==  "LMSPWB"), 
           data = data_area_10_TOT)    

# intercept 
intercept14 <- coef(summary(temp))[1,1]

# slope 
slope14 <- coef(summary(temp))[2,1]

# residual SS of the regression 
RSS14 <- sum(resid(temp)^2)





print(ls.str(envir = .GlobalEnv),max.level=0)
