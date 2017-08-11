data(ERPsets)

word=grandaverage("Exp1_word_subj", 1:20, erplist=ERPsets)

# check if some electrodes are not present in the list
# and create an object with these electrode names.
notfound=topoplot(word, return.notfound=TRUE)



#make a topoplot excluding not found electrode
prova=function(x){
  #define a layout for
  mat=matrix(c(1,2), 1, 2, byrow=TRUE)
  
  layout(mat, widths=c(0.8, 0.2))
  par(pty="s")
  topo.data=topoplot(word, startmsec=-200, endmsec=1500, win.ini=x, 
                   win.end=x+20, exclude=notfound, interp.points=100)

  #draw the palette on a new empty plot.
  par(pty="m", mar=c(0,0,0,0))
  plot.new()
  topoplot.palette(cols=topo.data$palette, 
                 palette.lim=topo.data$zlim, p.height=0.6) 
}

manipulate(
  prova(x),
  x=slider(0, 600, step=10)
  
)
