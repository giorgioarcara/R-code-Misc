###### lOAD PACKAGES #######
library(grid)
library(ggplot2)

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


grid.newpage()
my_lay_out(list(plot1, 1:100, 1:50),
         list(plot2+theme(plot.background = element_blank(), 
                             panel.background= element_blank(),
                             panel.grid.minor = element_blank(), 
                             panel.grid.major = element_blank()), 30:70, 30:80))


#ggsave("prova.tiff", scale=1, width=16, height=8, units="cm")

