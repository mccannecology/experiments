###############################
# generalized linear model    #
# Y = total area day 10       #
# Covariate: total area day 0 #
# Nutrients * sp. treatment   #
###############################

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

# get the residuals
glm_area_10_resids <- resid(glm_area_10)

# plot the residulas against observed values 
plot(data_area_10_TOT$area_10, glm_area_10_resids, xlab="Area day 10", ylab="Residuals")

