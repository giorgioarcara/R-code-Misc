library(ggplot2)
library(MASS)
# load function to plot flat violin
source("geom_flat_violin.R")


# Compute confidence interval around mean using normal approximation
mean_ci <- function(x, conf = 0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  lims = mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
  names(lims)=c("ymin", "ymax")
  return(lims)
}


############
# CREATE THEME (not mandatory, but lead to fancier plot)
##############
my_theme = theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(),  # remove backround
                 axis.title.x=element_blank(),  # get rid of of x axis title
                 axis.title.y=element_blank(), # get rid of y axis title
                 axis.text.x = element_text(colour="black", size=12, margin=margin(t=20)), # x labels ticks text
                 axis.text.y = element_text(colour="black", size=12), # set y labels ticks text
                 axis.line.x = element_line(color="black", size = 0.5), # set x axis line
                 axis.line.y = element_line(color="black", size = 0.5), # set y asxis line
                 plot.margin = unit(c(1,1,1,1), "cm"),
                 plot.title = element_text(hjust = 0.5, size=20) # 0.5 to adjust in center. 0 is left aligned
)


####################
# GENERATE FAKE DATA  ###
####################
# substitute with real data
Group = factor(c(rep("Patients", 40), rep("Controls", 40)))
Dep = c(rnorm(40, 30, 10), rnorm(40, 60, 10))

mydat = data.frame(Group=Group, Dep=Dep)

#######################
####  CREATE GGPLOT OBJECTS
#########################
my_flat_violinplot =
  ggplot(mydat, aes(x = Group, y = Dep, label=Group), stat="identity")+
  geom_flat_violin(aes(colour=Group, fill=Group, alpha = 0.5), trim = FALSE, position = position_nudge(x=0.13))+
  scale_alpha(guide=FALSE)+
  #guides(fill = guide_legend(override.aes= list(alpha = 0.5)))+
  #geom_boxplot(width=0.2, aes(fill=Group, col=Group), alpha=0.5)+
  geom_jitter(aes(col=Group), width=0.1, alpha=0.6, size=1) +
  stat_summary(fun.data = mean_ci, geom = "errorbar", width=0.05, size=1) +
  stat_summary(fun.y = mean, geom = "point", fill = "black", colour = "black", size=2) +
  ggtitle("My fabolous plot")+
  my_theme+
  NULL


###
## PRINT PLOT
my_flat_violinplot + my_theme + theme(legend.position="none")

# SAVE PLOT
ggsave("raincloud_plot.png",  scale = 1, width=20, height=15, units="cm")

