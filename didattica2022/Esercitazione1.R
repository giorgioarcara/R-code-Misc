## ESERCIZIO 1
# è nota la varianza della popolazione da cui è estratto il campione.
x= scan(file.choose())

mu0=150
sigma = sqrt(5)
m = mean(x)
n = length(x)

## calcolo statistica test (z)
zobs = (m-mu0)/(sigma/sqrt(n))

alpha = 0.05
zcrit_low = qnorm(alpha/2)
zcrit_high = qnorm(alpha/2, lower.tail = F)

# controllo se lo z osservato è inferiore a z critico a sinistra e destra.
zobs < zcrit_low
zobs > zcrit_high

# trovare p-value
# siccome è positivo faccio 1-p
1-pnorm(zobs)

# intervalli di confidenza.
m -1.96*(1/sqrt(n))
m + 1.96*(1/sqrt(n))


## ESERCIZIO 1
# non è nota la varianza, si stimano i parametri dal campione stesso
mu0 = 150
m = mean(x)
s = sd(x)

# calcolo t-test con funzione R
t.test(x, mu=150)
# calcolo statistica t a mano
t = (m-mu0)/(s/sqrt(n))

## intervallo di confidenza nel caso di t- (nota che uso 0.005 che è 0.01/2)
m+qt(0.005, df = n-1, lower.tail = T)*(s/sqrt(n))
m+qt(1-0.005, df = n-1, lower.tail = T)*(s/sqrt(n))



# ESERCIZIO 5 
n=100
m  = 115
s = 24
mu0 = 120


tobs = (m-mu0)/(s/sqrt(n))

pt(tobs, df = n - 1, lower.tail = T) # trovo il pvalue associato al mio t osservato (coda a sinistra)
qt(0.05, df = n-1, lower.tail=T) # trovo il t-critico associato a p = 0.05 (unidirezionale inferiore)
qt(0.01, df = n-1, lower.tail=T)  # trovo il t-critico associato a p = 0.01 (unidirezionale inferiore)




### ESERCIZIO 6 p.112
n=10
m  = 13
s = 4
mu0 = 15


tobs = (m-mu0)/(s/sqrt(n)) # calcolo t osservato
pt(tobs, df = n - 1, lower.tail = T) # trovo il p-value associato al mio t
qt(0.05, df = n-1, lower.tail=T) # trovo il t-critico associato a p = 0.05



# funzioni per distribuzioni statistiche
# distribuzione t
?pt
?qt
?dt

#distribuzione normale
?pnorm
?dnorm
?qnorm






