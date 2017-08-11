### BOZZA PER FUNZIONE ISPEZIONE VARIABILITA' DIVERSE VARS

source("source('~/Desktop/Lavori unipd/MOCA italian version/history analisi MOCA.R', chdir = TRUE)")

par(pty="s")
image( 1:(nrow(a)), 1:ncol(a), scale(a), axes=F, xlab="", ylab="")
axis(2, at=1:ncol(a), labels=MOCA.subtests, las=2)