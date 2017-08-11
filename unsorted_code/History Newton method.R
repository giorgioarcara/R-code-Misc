## history newton method

#setto il valore di partenza di x

x=6

#calcola la derivata della funzione f(x)
fx=((exp(1)^(x^2)))-(2.156*10^15)
f1x=2*x*((exp(1)^(x^2)))



#trova xn1 iterativamente a partire da xn1 trovando la secante a (xn, yn)



for (i in 0:20) {
	
	x=x-(fx/f1x)
fx=((exp(1)^(x^2)))-(2.156*10^15)
f1x=2*x*((exp(1)^(x^2)))
	
	}

print(x)
