# Euler method (dy/dx è uguale a f(x))
# point-slope formula of a line
# y - y0 = m*(x - x0)

# per poter funzionare il metodo di eulero devo avere un modo per ricavare f'(x) in termini di f(x) e x.
# questo può essere perché f'(x)=f(x) es. se f(x)=e^x. oppure posso avere una formula che mi dice quale è dydx, cioè la derivata. 
# Questo secondo esempio è quello che capitava negli esercizi di mooculus in cui veniva fornita una formula che legava dydx in termini di y (cioè f(x)) e x. e veniva chiesto di approssimare il valore ad una certa y, conoscendo il punto in passava la funzione. Un esempio di funzione che era data è
dydx= -x+2y, passante per il punto (2,3). In altre parole funzione è espressa sulla base della sua derivata.

x0=0
y0=1

h=0.5

# si vuole valutare il valore della funzione a xn
xn=1.5



#setto il primo momento di y e dydx
yi=y0
xi=x0

## creo equazione differenziale
a=-1 # il coefficiente di x
b=-1 # il coefficience di y
c=0  # il termine noto

expx=1
expy=1


#calcolo la slope al punto x0,y0
dydx0=a*(x0^expx)+b*(y0^expy)+c #nota che dydx0 non è la derivata, ma un approssimazione della derivata. Un'approssimazione di quanto varia y al variare di x

# per il ciclo, chiamo dydxi il primo momento
dydxi=dydx0


## calcolo fx per approssimazioni di eulero

for (i in seq(x0+h,xn,h)){
 
 yn=yi+h*dydxi
 xi=i
 yi=yn
 dydxi=a*(xi^expx)+b*(yi^expy)+c

 cat("x=", xi, "y=", yi, "dydx=", dydxi, "\n")
}

## NOTA! il tuo risultato è la y, non dydx.