#################################################
# Alternatives to ANCOVA                        #
# Non-parametric analysis of covaraiance        #
# R package: sm                                 #
# sm.ancova                                     #
#                                               #
#################################################
# Need to figure out what is "non-parametric" about this 
# does it allow for heterogeneous regression slopes?
# or non-normality? 

# Also, I think this only compares two groups 

library(sm)

# Example 
x <- runif(50, 0, 1)
y <- 4*sin(6*x) + rnorm(50)
g <- rbinom(50, 1, 0.5)
sm.ancova(x, y, g, h = 0.15, model = "equal")
# Test of equality :  h =  0.15    p-value =  0.3449 
# And returns a plot 

# what if there were more than 2 groups? 
x <- runif(50, 0, 1)
y <- 4*sin(6*x) + rnorm(50)
g <- sample(c(0,1,2,3,4),size=50,replace=TRUE)
sm.ancova(x, y, g, h = 0.15, model = "equal")
# Test of equality :  h =  0.15    p-value =  0.3979 
# Returns 
# Band available only to compare two groups.


# cleanup workspace 
rm(x,y,g)
