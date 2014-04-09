###############################
# generalized linear model    #
# Y = total area day 10       #
# Covariate: total area day 0 #
# Nutrients * sp. treatment   #
###############################
library(multcomp)

# I'm not sure how to interpret these results 
# Instead of an effect of nutrients, species treatment, and area_0
# I get an effect of each level within nutrients and species treatments and their interactions 
# Also, it leaves out one level of each factor - i.e., no treatmentLM and nutrientshigh

# the significant coefficients:
# Intercept, nutrientslow, treatmentWB, area_0

# check out the data 
head(data_area_10_TOT)

# glm, Y=area_10 
glm_area_10 <- glm(area_10 ~ nutrients + treatment + area_0 + nutrients:treatment, data = data_area_10_TOT)
summary(glm_area_10)

# multiple comparisons 
# this does not do it for interactions 
glht(glm_area_10, mcp(treatment="Tukey"))
summary(glht(glm_area_10, mcp(treatment="Tukey")))

# trying to do it for interactions 
# it doesn't look like I have an significant interaction term, so this might not be necessary
# following: http://cran.r-project.org/web/packages/multcomp/vignettes/multcomp-examples.pdf
tmp <- expand.grid(nutrients = unique(data_area_10_TOT$nutrients), treatment = unique(data_area_10_TOT$treatment))
X <- model.matrix(~ nutrients * treatment, data = tmp)
glht(glm_area_10, linfct = X)
# Error in glht.matrix(glm_area_10, linfct = X) : ‘ncol(linfct)’ is not equal to ‘length(coef(model))’

# Examine residuals #
# plot a histogram 
# looks normal-ish
hist(resid(glm_area_10),xlab="Residuals",main=NULL)

# QQ plot 
# Does not look very normal 
qqnorm(resid(glm_area_10),main=NULL) 
qqline(resid(glm_area_10)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(glm_area_10)) # p-value =1.861e-08 --- definitely not normally distributed residuals 

# get the residuals
glm_area_10_resids <- resid(glm_area_10)

# plot the residulas against observed values 
plot(data_area_10_TOT$area_10, glm_area_10_resids, xlab="Area day 10", ylab="Residuals")

