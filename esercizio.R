linf <- 0
lsup <- .99
passo <- (lsup-linf)/10000
x<- seq (linf,lsup,passo)
y <- x/(1-x)
plot(x,y,type="l",lwd=2)