topoplot.explore <- function(erpobj, myvar=600, startmsec=-200, endmsec=1200, win.ini, win.end, exclude = NULL,
  elec.coord=NULL, projection="orthographic", palette.col="jet", palette.steps=10, return.coord = FALSE,
  zlim=NULL, interpolation = "cubicspline", extrap = TRUE, interp.points = 500, return.notfound=FALSE, mask = TRUE,  contour=TRUE, x.rev=FALSE,
  draw.elec.pos=TRUE,  elec.pos.toplot="all", elec.pos.pch=19, elec.pos.cex=1, draw.nose=FALSE, draw.ears=FALSE, draw.elec.lab=TRUE, elec.lab.adj=c(0.5, NA), elec.lab.cex=1, elec.lab.toplot=elec.pos.toplot, head.col="black", head.lwd=1, ...)

{


# preliminary checks
	if (is.null(erpobj)){
	stop("an erpobj name must be specified!", call.=F)
	}

# la funzione contiene all'interno una funzione che crea il panel. Questa funzione a sua volta contiene la funzione scalp.infl.endo, che è quella che effettivamente fa il grafico appoggiandosi alla funzione scalp.endo. 

topoplot.explore.panel=function(panel, ...)
{
  topoplot(erpobj=erpobj, win.ini = panel$myvar, win.end=800, interp.points=50)
}
		panel <- rp.control() #se volessi creare più pannelli allora dovrei aggiungere un'altro panel.
       rp.slider(panel, myvar, startmsec, endmsec, action = topoplot.explore.panel, showvalue=T, title="Time", labels="ciao")
	   rp.do(panel, topoplot.explore.panel)
	   
	   
	   		
   }
