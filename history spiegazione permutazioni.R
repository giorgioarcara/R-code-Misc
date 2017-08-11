t.values=seq(-10,10,0.01)


plot(t.values, dt(x=t.values, df=10),  type="l")


abline(v=qt(p=0.025, df=10))
abline(v=qt(p=0.975, df=10))



hist(rt(n=100, df=10), breaks=100)



hist(rt(n=10000, df=10), breaks=100)
