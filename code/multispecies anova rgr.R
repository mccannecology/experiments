###########################################
# Two-way anova                           #
# Y = rgr                                 #
# Nutrients * sp. treatment               #
# One ANOVA for each species              #  
###########################################

head(summary_data_rgr02)

# Y=rgr, average
# Lemna
subset(summary_data_rgr02, summary_data_rgr02$species=="LM")
# Spirodela
subset(summary_data_rgr02, summary_data_rgr02$species=="SP")
# Wolffia
subset(summary_data_rgr02, summary_data_rgr02$species=="WB")

# Y=rgr, maximum 
# Lemna
# Spirodela
# Wolffia