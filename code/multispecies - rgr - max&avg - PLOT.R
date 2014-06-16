#################################################
# Analysis of multispecies duckweed experiment  #
# Summer 2013                                   #
#                                               #
# Plotting data                                 #
# avgRGR and maxRGR                             #
# average within a replicate                    #
#################################################
library(ggplot2)
library(gridExtra)

# check out the data you will use
head(data_rgr)

# re-format the data frames for plotting both on the same plot 
summary_data_maxRGR$RGR <- "maximum"
colnames(summary_data_maxRGR)[4] <- "RGRvalue"
summary_data_avgRGR$RGR <- "average"
colnames(summary_data_avgRGR)[4] <- "RGRvalue"

# combine into a single data frame 
summary_data_RGR <- rbind(summary_data_maxRGR,summary_data_avgRGR)
summary_data_RGR

# Alternatively, import as a .csv
summary_data_RGR <- read.csv("rgr_combo_posthoc.csv")

summary_data_RGR$treatment <- factor(summary_data_RGR$treatment , levels=c("LM","SP","WB","LMSP","LMWB","SPWB","LMSPWB"))

############
# RGR      #
# both max #
# and avg  #
############
# High nutrients 
summary_data_avgRGR_high <- ggplot(subset(summary_data_RGR, summary_data_RGR$nutrients=="high"), aes(x=treatment,y=RGRvalue,group=RGR)) 
summary_data_avgRGR_high <- summary_data_avgRGR_high + geom_point(aes(shape=RGR),size=3) 
summary_data_avgRGR_high <- summary_data_avgRGR_high + scale_shape_manual(values=c(1,16))
summary_data_avgRGR_high <- summary_data_avgRGR_high + geom_errorbar(aes(ymin=RGRvalue-se, ymax=RGRvalue+se), width=0.1)
summary_data_avgRGR_high <- summary_data_avgRGR_high + xlab("Species composition")
summary_data_avgRGR_high <- summary_data_avgRGR_high + ylab(expression(paste("Relative Growth Rate (", mm^2,"/",mm^-2,"/",day^-1,")",sep="")))
summary_data_avgRGR_high <- summary_data_avgRGR_high + ylim(0,0.42)
summary_data_avgRGR_high <- summary_data_avgRGR_high + geom_text(data=subset(summary_data_RGR, summary_data_RGR$nutrients=="high"),aes(x=treatment, y=RGRvalue+se+0.01,label=label1))
summary_data_avgRGR_high <- summary_data_avgRGR_high + geom_text(data=subset(summary_data_RGR, summary_data_RGR$nutrients=="high"),aes(x=treatment, y=RGRvalue-se-0.01,label=label2))
summary_data_avgRGR_high <- summary_data_avgRGR_high + geom_text(aes(x=1,y=0.42,label="a)"),size=7) # add pane label
summary_data_avgRGR_high <- summary_data_avgRGR_high + theme_classic(base_size=18)
summary_data_avgRGR_high 

# save it 
ggsave(filename = "summary_data_avgRGR_high.jpg", summary_data_avgRGR_high, height=11, width=11)

# Low nutrients 
summary_data_avgRGR_low <- ggplot(subset(summary_data_RGR, summary_data_RGR$nutrients=="low"), aes(x=treatment,y=RGRvalue,group=RGR)) 
summary_data_avgRGR_low <- summary_data_avgRGR_low + geom_point(aes(shape=RGR),size=3) 
summary_data_avgRGR_low <- summary_data_avgRGR_low + scale_shape_manual(values=c(1,16))
summary_data_avgRGR_low <- summary_data_avgRGR_low + geom_errorbar(aes(ymin=RGRvalue-se, ymax=RGRvalue+se), width=0.1)
summary_data_avgRGR_low <- summary_data_avgRGR_low + xlab("Species composition")
summary_data_avgRGR_low <- summary_data_avgRGR_low + ylab(expression(paste("Relative Growth Rate (", mm^2,"/",mm^-2,"/",day^-1,")",sep="")))
summary_data_avgRGR_low <- summary_data_avgRGR_low + geom_text(data=subset(summary_data_RGR, summary_data_RGR$nutrients=="low"),aes(x=treatment, y=RGRvalue+se+0.0075,label=label1))
summary_data_avgRGR_low <- summary_data_avgRGR_low + geom_text(data=subset(summary_data_RGR, summary_data_RGR$nutrients=="low"),aes(x=treatment, y=RGRvalue-se-0.0075,label=label2))
summary_data_avgRGR_low <- summary_data_avgRGR_low + geom_text(aes(x=1,y=0.2,label="b)"),size=7) # add pane label
summary_data_avgRGR_low <- summary_data_avgRGR_low + theme_classic(base_size=18)
summary_data_avgRGR_low 

# save it 
ggsave(filename = "summary_data_avgRGR_low.jpg", summary_data_avgRGR_low, height=11, width=11)

# arrnage the plots 
combo_RGR_plot <- arrangeGrob(summary_data_avgRGR_high,summary_data_avgRGR_low,ncol=1,nrow=2) #grid.arrange does not work with ggsave()
combo_RGR_plot

ggsave(filename = "combo_RGR_plot.jpg", combo_RGR_plot, height=12, width=9)

