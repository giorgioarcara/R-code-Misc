#copia il codice sotto e incollalo per fare partire la funzione
####################################
## FUNZIONE 1 (asse y mobile)#######
####################################
#nella funzione che segue l'asse y viene riscalato dai coefficienti che inserisci
library(rpanel)
if (interactive()) {
   plotf <- function(panel) {
      with(panel, {
		 pars   <- as.numeric(pars)
         x=1:100 # puoi modificare questa che è l'asse delle x mettendo primo e ultimo valore.
         plot(pars[1]*(pars[2])^x, type = "l", col = "black", lwd = 2, xlim=c(0,100))#qui ho inserito la tua formula
         })
      panel
      }
   panel <- rp.control(pars = c(5, 10, NA))
   rp.textentry(panel, pars, plotf, labels = c("coef1", "coef2"), #qui tra parentesi puoi cambiare i nomi dei coef
          initval = c(1000, 0.5)) #cambia i valori di default di coef1 e coef2 dentro questa parentesi
   rp.do(panel, plotf)
   }
   
#nella riga dove c'è plot(pars[1]*(pars[2])^x..... puoi modificare la funzione attualmente la funzione messa
# è coef1*coef2^x. Nota che coef1 è espresso come pars[1] e coef2 come pars[2]. Se adesempio la funzione che vuoi mettere diventa coef1 + (coef2^x), la sintassi è pars[1]+(pars[2]^x)


#se vuoi mettere un limite fisso all'asse delle y devi utilizzare il codice sotto
####################################
## FUNZIONE 2 (asse y fisso)#######
####################################
#cambia i valori sottolineati in xlim sono quelli che indicano gli estremi dell'asse y

if (interactive()) {
   plotf <- function(panel) {
      with(panel, {
		 pars   <- as.numeric(pars)
         x=1:100 # puoi modificare questa che è l'asse delle x mettendo primo e ultimo valore.
         plot(pars[1]*(pars[2])^x, type = "l", col = "black", lwd = 2, xlim=c(0,100), ylim=c(0,500))#qui ho inserito la tua formula
         })
      panel
      }
   panel <- rp.control(pars = c(5, 10, NA))
   rp.textentry(panel, pars, plotf, labels = c("coef1", "coef2"), #qui tra parentesi puoi cambiare i nomi dei coef
          initval = c(1000, 0.5)) #cambia i valori di default dentro questa parentesi
   rp.do(panel, plotf)
   }