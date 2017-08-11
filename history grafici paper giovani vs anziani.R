setwd("/Volumes/GIORGIOHD/ANZIANI_vs_giovani_PM_TIME/ANALISI_ANZIANI_vs_GIOVANI_TIME")
stop.point=grep("stop.point", parse("history confronti grafici ANZIANI e giovani .R", n=100))
eval(parse("History confronti grafici ANZIANI e giovani .R", n=stop.point-1))


source("/Users/giorgioarcara/Desktop/R files/eeg.axis.R")
oldpar<-par(no.readonly=TRUE)

# par(plt=c(0.6,0.1, 0.02857143, 0.5)) #plt dovrebbe settare le coordinate del grafico, ma non capisco rispetto a cosa



#########################
# FIGURE ELETTRODI GIOVANI
#########################
setwd("/Volumes/GIORGIOHD/ANZIANI_vs_giovani_PM_TIME/ANALISI_ANZIANI_vs_GIOVANI_TIME/figure paper ANZIANI vs giovani")
pdf(file="giovani_elettrodi.pdf")

par(mfrow=c(2,2), mai=c(0.1,0.1,0.4,0.1), pty="s")


eeg(baseline_giovani$Fpz, startmsec=-200, endmsec=1200, xaxis0=TRUE, yaxis0=TRUE, main="Fpz", lty=1, frame.plot=FALSE, yticks=c(-10,-5,5,10), xticks=seq(100,1200,100), cex.main=2, smo=0, lwd=3, col="darkgray",ylim=c(-10,10))
 comp.add(PMong_giovani$Fpz, smo=0, lwd=3)
 
eeg(baseline_giovani$FZ, startmsec=-200, endmsec=1200, xaxis0=TRUE, yaxis0=TRUE, main="Fz", lty=1, frame.plot=FALSE, yticks=c(-10,-5,5,10), xticks=seq(100,1200,100), cex.main=2, , smo=0, lwd=3, col="darkgray",ylim=c(-10,10))
comp.add(PMong_giovani$FZ, smo=0, lwd=3)
 
eeg(baseline_giovani$CPz, startmsec=-200, endmsec=1200, xaxis0=TRUE, yaxis0=TRUE, main="CPz", lty=1, frame.plot=FALSE, yticks=c(-10,-5,5,10), xticks=seq(100,1200,100), cex.main=2, smo=0, lwd=3, col="darkgray",ylim=c(-10,10))
comp.add(PMong_giovani$CPz, smo=0, lwd=3)

eeg(baseline_giovani$PZ, startmsec=-200, endmsec=1200, xaxis0=TRUE, yaxis0=TRUE, main="Pz", lty=1, frame.plot=FALSE, yticks=c(-10,-5,5,10), xticks=seq(100,1200,100), cex.main=2, , smo=0, lwd=3, col="darkgray",ylim=c(-10,10))
comp.add(PMong_giovani$PZ, smo=0, lwd=3)


dev.off()
#########################
# FIGURE ELETTRODI ANZIANI
######################### 
pdf("ANZIANI_elettrodi.pdf")

par(mfrow=c(2,2), mai=c(0.1,0.1,0.4,0.1), pty="s")


eeg(baseline_anziani$Fpz, startmsec=-200, endmsec=1200, xaxis0=TRUE, yaxis0=TRUE, main="Fpz", lty=1, frame.plot=FALSE, yticks=c(-10,-5,5,10), xticks=seq(100,1200,100), cex.main=2, smo=0, lwd=3, col="darkgray",ylim=c(-10,10))
 comp.add(PMong_anziani$Fpz, smo=0, lwd=3)
 
eeg(baseline_anziani$FZ, startmsec=-200, endmsec=1200, xaxis0=TRUE, yaxis0=TRUE, main="Fz", lty=1, frame.plot=FALSE, yticks=c(-10,-5,5,10), xticks=seq(100,1200,100), cex.main=2, , smo=0, lwd=3, col="darkgray",ylim=c(-10,10))
comp.add(PMong_anziani$FZ, smo=0, lwd=3)
 
eeg(baseline_anziani$CPz, startmsec=-200, endmsec=1200, xaxis0=TRUE, yaxis0=TRUE, main="CPz", lty=1, frame.plot=FALSE, yticks=c(-10,-5,5,10), xticks=seq(100,1200,100), cex.main=2, smo=0, lwd=3, col="darkgray",ylim=c(-10,10))
comp.add(PMong_anziani$CPz, smo=0, lwd=3)

eeg(baseline_anziani$PZ, startmsec=-200, endmsec=1200, xaxis0=TRUE, yaxis0=TRUE, main="Pz", lty=1, frame.plot=FALSE, yticks=c(-10,-5,5,10), xticks=seq(100,1200,100), cex.main=2, , smo=0, lwd=3, col="darkgray",ylim=c(-10,10))
comp.add(PMong_anziani$PZ, smo=0, lwd=3)


dev.off()


par(oldpar)

####################
## FIGURE DIFFERENzIALI
###################
pdf("differenziali.pdf")

par(mfrow=c(2,2), mai=c(0.1,0.1,0.4,0.1), pty="s")


eeg(diff_giovani$Fpz, startmsec=-200, endmsec=1200, xaxis0=TRUE, yaxis0=TRUE, main="Fpz",  frame.plot=FALSE, yticks=c(-4,4), xticks=seq(100,1200,100), cex.main=2, smo=0, lwd=3, col="darkgray", lty=1)
 comp.add(diff_anziani$Fpz, smo=0, lwd=3, lty=1)
 
eeg(diff_giovani$FZ, startmsec=-200, endmsec=1200, xaxis0=TRUE, yaxis0=TRUE, main="Fz",  frame.plot=FALSE, yticks=c(-4,4), xticks=seq(100,1200,100), cex.main=2, smo=0, lwd=3, col="darkgray", lty=1)
 comp.add(diff_anziani$FZ, smo=0, lwd=3)
 
eeg(diff_giovani$CPz, startmsec=-200, endmsec=1200, xaxis0=TRUE, yaxis0=TRUE, main="CPz",  frame.plot=FALSE, yticks=c(-4,4), xticks=seq(100,1200,100), cex.main=2, smo=0, lwd=3, col="darkgray", lty=1)
 comp.add(diff_anziani$CPz, smo=0, lwd=3)
 
eeg(diff_giovani$PZ, startmsec=-200, endmsec=1200, xaxis0=TRUE, yaxis0=TRUE, main="Pz",  frame.plot=FALSE, yticks=c(-4,4), xticks=seq(100,1200,100), cex.main=2, smo=0, lwd=3, col="darkgray", lty=1)
 comp.add(diff_anziani$PZ, smo=0, lwd=3)

dev.off()


par(oldpar)
####################
## FIGURE DIFFERENZIALI COLORATI
###################
pdf("differenziali_col.pdf")

par(mfrow=c(2,2), mai=c(0.1,0.1,0.4,0.1), pty="s")


eeg(diff_giovani$Fpz, startmsec=-200, endmsec=1200, xaxis0=TRUE, yaxis0=TRUE, main="Fpz",  frame.plot=FALSE, yticks=c(-4,4), xticks=seq(100,1200,100), cex.main=2, smo=0, lwd=3, col="green", lty=1)
 comp.add(diff_anziani$Fpz, smo=0, lwd=3, lty=1, col="red")
 
eeg(diff_giovani$FZ, startmsec=-200, endmsec=1200, xaxis0=TRUE, yaxis0=TRUE, main="Fz",  frame.plot=FALSE, yticks=c(-4,4), xticks=seq(100,1200,100), cex.main=2, smo=0, lwd=3, col="green", lty=1)
 comp.add(diff_anziani$FZ, smo=0, lwd=3,col="red")
 
eeg(diff_giovani$CPz, startmsec=-200, endmsec=1200, xaxis0=TRUE, yaxis0=TRUE, main="CPz",  frame.plot=FALSE, yticks=c(-4,4), xticks=seq(100,1200,100), cex.main=2, smo=0, lwd=3, col="green", lty=1)
 comp.add(diff_anziani$CPz, smo=0, lwd=3,col="red")
 
eeg(diff_giovani$PZ, startmsec=-200, endmsec=1200, xaxis0=TRUE, yaxis0=TRUE, main="Pz",  frame.plot=FALSE, yticks=c(-4,4), xticks=seq(100,1200,100), cex.main=2, smo=0, lwd=3, col="green", lty=1)
 comp.add(diff_anziani$PZ, smo=0, lwd=3,col="red")

dev.off()


####################
## FIGURE ASSI
###################

pdf(file="assi.pdf")
par(mfrow=c(2,2), mai=c(0.1,0.1,0.4,0.1), pty="s")
eeg.axis(baseline_giovani$Fpz, startmsec=-200, endmsec=1200, xaxis0=TRUE, yaxis0=TRUE, lty=1, frame.plot=FALSE, x.axis=seq(100,1200,100), y.axis.step=2 , smo=0, cex.xaxis=1, cex.yaxis=1.2, x.dist=3, y.dist=6, yticks=c(-10,-5,5,10), xlim=c(-300, 1200), ylim=c(-10,10), ynumbers=c(""), xnumbers=c(200,400,600,800,1000))
plot.new()
plot.new()
plot.new()
dev.off()

pdf(file="assi2.pdf", height=3.5, width=3.5)#questo è l'asse per i differenziali
eeg.axis(baseline_giovani$Fpz, startmsec=-200, endmsec=1200, xaxis0=TRUE, yaxis0=TRUE, lty=1, frame.plot=FALSE, x.axis=seq(100,1200,100), y.axis.step=2 , smo=0, cex.xaxis=0.6, cex.yaxis=1.2, x.dist=3, y.dist=6, yticks=c(-4,4), xlim=c(-300, 1200), ynumbers=c(""), xnumbers=c(200,400,600,800,1000), ylim=c(-6,6))
dev.off()
#########################
# FIGURA LEGENDA
#########################

pdf("legend.diff.pdf", height=3, width=4)
plot.new()
legend("center", legend=c("Younger adults", "Older adults"), lty=c(1,1), lwd=3, bty="n", cex=1.5, col=c("darkgray", "black"))
dev.off()


par(oldpar)
#########################
# FIGURA LEGENDA COLL
#########################

pdf("legend.diff_col.pdf", height=3, width=4)
plot.new()
legend("center", legend=c("Younger adults", "Older adults"), lty=c(1,1), lwd=3, bty="n", cex=1.5, col=c("green", "red"))
dev.off()


par(oldpar)

