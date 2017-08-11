##################
### funzione per matrice rettangolare di scatterplot. ancora in bozza.
#####################


rect.pairs=function(row.vars, col.vars, dat, smo=0.6, cor.type="pearson", row.lab=NULL, col.lab=NULL, lab.cex=1){
	
	# compute correlations
require(Hmisc)	
	
d=dat[, c(row.vars, col.vars)]

### recupero nomi di colonna.
if (is.null(col.lab)){
	col.lab=col.vars
}

if (is.null(row.lab)){
	row.lab=row.vars
}


cormatrix = rcorr(as.matrix(d), type=cor.type) # utilissima funzione da Hmisc


# CON QUESTA TRASFORMAZIONE CHE SEGUE faccio in modo che la matrice sia rettangolare
# praticamente faccio in modo che righe e colonne siano mutualmente esclusive (e APACS sia solo in righe)
cormatrix$r=cormatrix$r[rownames(cormatrix$r)%in%row.vars, !colnames(cormatrix$r)%in%row.vars]
cormatrix$P=cormatrix$P[rownames(cormatrix$P)%in%row.vars, !colnames(cormatrix$P)%in%row.vars]

	
	par(mfrow=c(length(row.vars)+1, length(col.vars)+1), mar=c(0.5,0.5,0.5,0.5))
	for (plot.row in 1:(length(row.vars)+1)){
		for (plot.col in 1:(length(col.vars)+1)){ # vado per colonne

			# plot first empty square
			if (plot.row==1&plot.col==1){
				plot(0,0, type="n", frame.plot=F, axes=F, xlab="", ylab="", ylim=c(-1,1), xlim=c(-1,1)) # 
				}

			# plot col labels
			if (plot.row==1&plot.col>1){
				plot(0,0, type="n", frame.plot=F, axes=F, xlab="", ylab="", ylim=c(-1,1), xlim=c(-1,1))
				text(0,0, labels=col.lab[plot.col-1], cex=lab.cex)
				}

			# plot row labels
			if (plot.col==1&plot.row>1){
				plot(0,0, type="n", frame.plot=F, axes=F, xlab="", ylab="", ylim=c(-1,1), xlim=c(-1,1))
				text(0,0, labels=row.lab[plot.row-1], cex=lab.cex)
				}

			# plot scatterplots
			if (plot.col>1&plot.row>1){

			x=dat[, row.vars[plot.row-1]]
			y=dat[, col.vars[plot.col-1]]
			
			ok <- is.finite(x) & is.finite(y)
				
	
			plot(x[ok], y[ok], xlab="", ylab="", axes=F, frame.plot=T, col="darkgray")
			lines(lowess(x=(x[ok]), y=y[ok]))
			
			if (cor.type=="pearson"){
				cor.col="#0000ff99"
			}
			if (cor.type=="spearman"){
				cor.col="#E51B1B"
			}
			
			
				if (cormatrix$P[plot.row-1,plot.col-1]<0.05){
					legend("center", legend=round(cormatrix$r[plot.row-1,plot.col-1],2), bty="n", text.col=cor.col, cex=2.5, text.font=2, xjust=0.8)
				}
			}
		}
	}
}

