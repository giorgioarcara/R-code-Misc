x1 = rnorm(100, )
x2 = x1 + rnorm(100, sd = 0.5)
x3 = x1 + rnorm(100, sd = 0.5)

dat1 = data.frame(x1=x1, x2=x2, x3=x3)


y1 = rnorm(100)
y2 = y1 + rnorm(100, sd = 0.5)
y3 = y1 + rnorm(100, sd = 0.5)

dat2 = data.frame(y1=y1, y2=y2, y3=y3)


dat = cbind(dat1, dat2)

round(cor(dat), 2)

dat.pca = prcomp(dat)
