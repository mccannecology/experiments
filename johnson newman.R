#################################################
# Alternatives to ANCOVA                        # 
# Johnson-Neyman Technique for ANCOVA           #
# http://r-forge.r-project.org/projects/jnt/    #
# https://r-forge.r-project.org/R/?group_id=646 # 
#                                               #
# R package: jnt                                #
# Status: Failed to Build                       #  
#################################################
# This method only works to compare two groups (I think) 

# load packages 
require(devtools)
install_github("jnt", "kmiddleton")
require(jnt)

# an example
example(White.1)

# another example 
example(Lavin.1)
# Lavin.1 - 64 observations of v1 and v2
# Lavin.2 - 220 observations of v1 and v2 
jnt(Lavin.1,Lavin.2)
# Alpha =  0.05 
# Set 1:
# Slope		 0.39 
# Intercept	 2.047 
# Set 2:
# Slope		 0.5205 
# Intercept	 1.169 
# Region of non-significant slope difference
# Lower		Upper
# 5.9 	 7.873 


jnt(Lavin.1,Lavin.1)
# says slopes are not significantly different (duh)

plot(jnt(Lavin.1,Lavin.2))
# x,y plot 
# with region of non-significant slope difference indicated 

# clean-up workspace
rm(Lavin.1,Lavin.2,Lavin,White,White.1,White.2)

