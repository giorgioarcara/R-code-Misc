# TO BLUR IMAGES, not used
#install.packages("spatstat")
#library(spatstat)
#prova = blur(as.im(volcano), sigma=1)

# foreign::read.octave #
# to load data from mat file

prova = volcano

#library(foreign)
#prova = read.octave("/Users/giorgioarcara/Documents/MATLAB/provaTF.mat")


library(akima)

## SET PARAMETERS

xvalues = rep(1:dim(prova)[1], dim(prova)[2])
yvalues = rep(1:dim(prova)[2], each=dim(prova)[1])

xlim=range(xvalues)
ylim=range(yvalues)

interp.points=200

palette.steps= 100

mypalette <- colorRampPalette(heat.colors(palette.steps))


## INTERPOLATE DATA

interpdata = interp(x = xvalues, y = yvalues,  
                    z = unlist(prova),
                    xo =seq(xlim[1], xlim[2], length = interp.points), 
                    yo=seq(ylim[1], ylim[2], length = interp.points) 
                    )

# CREATE PALETTE
mypalette <- colorRampPalette(heat.colors(palette.steps))


image(interpdata, col=mypalette(palette.steps))


