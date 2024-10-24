library(ggplot2)
library(dplyr)

data(iris)

# create initial barplot
myplot = ggplot(iris, aes(x = Species, y = Sepal.Length))

# create barplot
my_barplot = myplot + stat_summary(fun.y = mean, geom = "col", fill = "White", colour = "Black", width=0.5) + 
  geom_jitter(width=0.05, col="gray", alpha=0.8) +
stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.2, size=1)
##########
# THEME ##
##########

### create a theme (i.e., change a series of parameters of the plot)
my_theme = theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(),  # remove backround
                 axis.title.x=element_blank(),  # get rid of of x axis title
                 axis.title.y=element_blank(), # get rid of y axis title
                 axis.text.x = element_text(colour="black", size=12, vjust=0), # x labels ticks text
                 axis.text.y = element_text(colour="black", size=12), # set y labels ticks text
                 axis.line.x = element_line(color="black", size = 0.5), # set x axis line
                 axis.line.y = element_line(color="black", size = 0.5), # set y asxis line
                 plot.margin = unit(c(1,1,1,1), "cm")

)

my_barplot_t = my_barplot + my_theme # + coord_cartesian(ylim=c(-6,6))

print(my_barplot_t)

## function to create standard error similar to mean_cl_normal
# in misc package. Contain y, ymin, ymax.


mean_se = function(x){
  se = function(x){sd(x)/sqrt(length(x))}
  data.frame(y=mean(x), ymin=mean(x)+se(x), ymax=mean(x)-se(x))
}


### VIOLIN
my_violin_plot = myplot +
  geom_violin(col="black", fill="gray", alpha=0.2, trim = FALSE) +
   geom_jitter(width=0.05, col="darkgray", alpha=0.9) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.2, size=1) +
stat_summary(fun.y = mean, geom = "point", fill = "black", colour = "black", size=4) +
stat_summary(aes(group=1), fun.y=mean, geom="line", size=1.1) 
  

my_violin_plot_t = my_violin_plot + my_theme

my_violin_plot_t


### BAR
my_bar_plot = myplot +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.2, size=1, position=position_dodge(width=50)) +
  stat_summary(fun.y = mean, geom = "bar", fill = c("red", "blue", "black"), 
               colour = "black", size=1, position=position_nudge(c(0.1, 0.1, 0.8)))


my_bar_plot_t = my_bar_plot + my_theme

my_bar_plot_t


ggplot(iris, aes(x = Species, y = Sepal.Length)) + 
  stat_summary()
  stat_summary(fun.data = mean_se, geom = "errorbar", position=position_dodge(width=10)) 
  
  
### LINE PLOT
my_line_plot = myplot +
    stat_summary(aes(group = 1), fun.data = mean_se, geom = "line", size = 1, col = "black") +
    stat_summary(fun = mean, geom = "point", color = "black", size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar", color = "black", width = 0.2, size = 0.5)
  
  
  my_line_plot_t = my_line_plot + my_theme
  
  my_line_plot_t
  
  
  
  

ggsave("My_plot.png",  scale = 1, width=20, height=15, units="cm")
  
