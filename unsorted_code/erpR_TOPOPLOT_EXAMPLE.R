# load package
library(erpR)

# load sample data
data(erplistExample)

# create grandaverage
word=grandaverage("Exp1_word_subj", 1:20, erplist=erplistExample)



# uncomment this line and the "dev.off()" line below to generate an external jpeg


#jpeg("my_topoplot_image.jpeg", height = 800, width = 800, res = 150)

mat=matrix(c(1,2), 1, 2, byrow=TRUE)
layout(mat, widths=c(0.8, 0.2))

par(pty="s")
# to identify not found electrode in the list (typically VEOG, or external electrode)
notfound=topoplot(word, return.notfound=TRUE)
topo.data=topoplot(word, startmsec=-200, endmsec=1500, win.ini=100, contour = F, interp.points = 200,
                   win.end=300, exclude=notfound, projection = "equalarea", zlim=c(-7, 7), draw.elec.lab = F, draw.nose = T, draw.ears = T, head.lwd = 4, nose.lwd=2, ears.lwd = 2 )



#draw the palette on a new empty plot.

par(pty="m", mar=c(0,0,0,0))
plot.new()
topoplot.palette(cols=topo.data$palette, 
                 
                 palette.lim=topo.data$zlim, p.height=0.6 )

#dev.off()

