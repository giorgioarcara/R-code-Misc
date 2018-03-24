###### lOAD PACKAGES #######
library(grid)
library(ggplot2)
library(png)

plot1 = ggplot(data = iris, aes(x=Sepal.Length, y=Sepal.Width))+
  geom_point()

plot2 = ggplot(data = iris, aes(x=Species, y=Petal.Length, group=Species))+
  stat_summary(fun.data = mean_se, geom = "bar", width=0.2, size=1)
  


## I got the following function from internet 
# https://stackoverflow.com/questions/18427455/multiple-ggplots-of-different-sizes
lay_out = function(...) {    
  x <- list(...)
  n <- max(sapply(x, function(x) max(x[[2]])))
  p <- max(sapply(x, function(x) max(x[[3]])))
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(n, p)))    
  
  for (i in seq_len(length(x))) {
    print(x[[i]][[1]], vp = grid::viewport(layout.pos.row = x[[i]][[2]], 
                                           layout.pos.col = x[[i]][[3]]))
  }
} 


# I modified the preceding function (that arrange on the basis of relative proportion)
# to arrange plots on the basis of "ABSOLUTE" position, that are specified as percentage
# (actually you can increase precision, if you use 1000 in place of 100 for r and p)

my_lay_out = function(...) {    
  x <- list(...)
  n = 100
  p = 100
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(n, p)))    
  
  for (i in seq_len(length(x))) {
    print(x[[i]][[1]], vp = grid::viewport(layout.pos.row = x[[i]][[2]], 
                                           layout.pos.col = x[[i]][[3]]))
  }
} 

#### NOTE: It is importand to do grid.newpage() before
grid.newpage()
my_lay_out(list(plot1, 1:100, 1:50),
         list(plot2+theme(plot.background = element_blank(), 
                             panel.background= element_blank(),
                             panel.grid.minor = element_blank(), 
                             panel.grid.major = element_blank()), 30:70, 30:80))


#### NOTE: It is importand to do grid.newpage() before
grid.newpage()
my_lay_out(list(plot1, 1:100, 1:50),
           list(plot2, 30:70, 30:80),
           list(plot(rnorm(10)), 1:50, 51:100))



#########
# CARICO IMMAGINE 
##############


img = readPNG("TOPO.png")

##### EMPTY PLOT (maybe there is a better way)
df <- data.frame()
p0=ggplot(df)  +  xlim(-1, 1) + ylim(-1, 1)+
  theme(axis.text=element_blank())+ # get rid of text ticks
  theme(axis.ticks=element_blank())+ # remove x and y ticks
  theme(axis.title=element_blank())+ # get rid of both axis titles
  theme(axis.line = element_blank(), # line colour of axis
        panel.grid.major = element_blank(), # major lines of grid
        panel.grid.minor = element_blank(), # minor lines of grid
        panel.border = element_blank(), # border (dont't know what it is)
        panel.background = element_blank())+  # background style
  theme(plot.margin = unit(c(0,0,0,0), "cm")) # ch

# empty plot plus topoplot
plot3 = p0 + annotation_raster(img, xmin=-1, xmax=1, ymin=-1, ymax=1) # note min and max are the same
plot3

### IMPORTANT
# with grid.newpage ggsave does not work
tiff("prova1.tiff", height=800, width=1000, res=100)
grid.newpage()
lay_out(list(plot1, 1:100/8, 1:50/10),
           list(plot2, 1:50/8, 51:100/10),
           list(plot3, 51:100/8, 51:100/10))
dev.off()





# DOES NOT WORK
#ggsave("prova.tiff", scale=1, width=16, height=8, units="cm")

