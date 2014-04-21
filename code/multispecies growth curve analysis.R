################################################
# Analysis of multispecies duckweed experiment #
#                                              #
# Growth curve analysis                        #
################################################

###################################
# Plot raw area data through time #
###################################
# re-order my treatments so they go from monocultures to polycultures in alphabetical order 
data_area$treatment <- factor(data_area$treatment, levels=c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB"))

# colour w/ unqie y-axis scales for each facet 
area_plot_raw <- ggplot(data_area, aes(x=day,y=area_mm2,group=id2,colour=species)) + geom_line() + geom_point() 
area_plot_raw <- area_plot_raw + facet_grid(nutrients ~ treatment, scales="free_y")
area_plot_raw <- area_plot_raw + scale_x_discrete(breaks=c(0,2,4,6,8,10),labels=c(0,2,4,6,8,10))
area_plot_raw <- area_plot_raw + ylab("area (sq. mm)")
area_plot_raw 

###############################
# Plot mean area through time #
###############################
# re-order my treatments so they go from monocultures to polycultures in alphabetical order 
summary_data_area$treatment <- factor(summary_data_area$treatment, levels=c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB"))

# colour w/ unqie y-axis scales for each facet 
area_plot_avg <- ggplot(summary_data_area, aes(x=day, y=area,colour=species)) + geom_errorbar(aes(ymin=area-se, ymax=area+se), width=0.1)
area_plot_avg <- area_plot_avg + geom_line() + geom_point()
area_plot_avg <- area_plot_avg + facet_grid(nutrients ~ treatment, scales="free_y")
area_plot_avg <- area_plot_avg + scale_x_discrete(breaks=c(0,2,4,6,8,10),labels=c(0,2,4,6,8,10))
area_plot_avg <- area_plot_avg + ylab("area (sq. mm)")
area_plot_avg

###############
# A test case #
###############
# LM, high nutrients 
# compare the fit of three different curves: exponential, logistic, quadratic 

temp <- subset(data_area,data_area$treatment == "LM" & data_area$nutrients == "high")
nrow(temp) # 72
head(temp)


