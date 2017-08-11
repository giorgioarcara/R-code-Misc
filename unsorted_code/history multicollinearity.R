set.seed(42)
age=rnorm(100, mean=50, sd=10)
set.seed(666)
education=rnorm(100, mean=15, sd=5)

set.seed(147)
noise=rnorm(100, mean=0, sd=50)

RT=500+(age*10)+education*(4)+noise


summary(lm(RT~age+education))

set.seed(42)
age=rnorm(100, mean=50, sd=10)

set.seed(666)
education=(0.95*age)+rnorm(100,mean=0, sd=1)

set.seed(147)
noise=rnorm(100, mean=0, sd=50)


RT=500+(age*10)+education*(4)+noise

summary(lm(RT~age+education))
summary(lm(RT~age))
