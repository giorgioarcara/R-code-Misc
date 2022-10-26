# ES.6  (la formula usata è alla slide 112 del pdf Lezione_1_2022_Richiami)
rm(list=ls())

m=11 # media campione
s=11 # deviazione standard campione
mu0=24 # media popolazione 
n = 8 # numerosità camponaria
alpha = 0.01 # alpha critico

# calcolo t osservato
tobs = (m-mu0)/(s/sqrt(n))
qt(0.01, df = n-1)
pt(tobs, df = n-1)

### ESERCIZIO P. 
rm(list=ls())
# - test z per proporzioni
n = 36150
n2 = 1940
p = n2 / n
p0 = 0.04

zobs = (p-p0) / (sqrt(p0 * (1-p0)/n ) )

# tre modi equivalenti di calcolare p-value
pnorm(zobs, lower.tail = F)
pnorm(-zobs)
1-pnorm(zobs)

# siccome è a due code, moltiplico il risultato di p-norm (messo correttamente come sopra) * 2
pobs = (1-pnorm(zobs))*2
# oppure
pnorm(zobs, lower.tail = F)*2

alpha = 0.05
z_crit = abs(qnorm(alpha/2))

CI_upper = p + z_crit * sqrt(p*(1-p)/n)
CI_lower = p - z_crit * sqrt(p*(1-p)/n)


### Esercizio 2
m1 = 132.86
m2 = 127.44
s1 = 15.34
s2 = 18.23
n1 = 8
n2 = 21

sp_2 = ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2)
sp = sqrt(sp_2)

tobs = (m1-m2)/(sp*(sqrt( (1/n1) +(1/n2) )) )
(1-pt(tobs, df = n1+n2-2))*2
# oppure un modo equivalente 
pt(tobs, df = n1+n2-2, lower.tail = F)*2
