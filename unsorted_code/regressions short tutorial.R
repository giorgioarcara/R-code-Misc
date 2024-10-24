x = rnorm(100, mean=80, sd=10)
b = -2
e = rnorm(100, mean=0, sd = 6)
q = 0
d = 2

# regressione
y = b*x + q + e

cor(x, y)
plot(x, y)

# create data.frame (to be more similar to real data)
dat = data.frame(x=x, y=y)

mod = lm(y~x, data = dat)

print(mod)
summary(mod)

plot(dat$x, dat$y)
abline(mod)

# Assumptions check
hist(residuals(mod))
min(residuals(mod))
quantile(residuals(mod), probs = 0.25)
quantile(residuals(mod), probs = 0.5)
quantile(residuals(mod), probs = 0.75)
max(residuals(mod))
# normality of residuals
# - quantiles check (symmetrical, centered to 0)
# - qqnorm residuals

qqnorm(residuals(mod))
qqline(residuals(mod))

library(effects)
plot(allEffects(mod, partial.residuals=T))

## factors
# group
# example 1 factor with 2 levels
g = factor( c(rep("A", 50), rep("B", 50)))


# y = bx + q + e
# x = (0,1)
dat2 = dat

dat2$g = g
g1_effA = 5
dat2[dat2$g=="A", "y"] = dat2[dat2$g=="A", "y"] + g1_effA # subject of group A have + gv
boxplot(y~g, dat2)


mod2 = lm(y~g, data=dat2)
summary(mod2)
model.matrix(mod2)


predict(mod2, newdata=data.frame(g="A"))
predict(mod2, newdata=data.frame(g="B"))

plot(allEffects(mod2))

# example with 3 levels
dat3 = dat
# y = bx1 + cx2 + q + e
# x = (0,1), x2 = (0,1)
g2 = factor( c(rep("A", 33), rep("B", 33), rep("C", 34)) )
dat3$g2=g2

g2_effA = 0
dat3[dat3$g=="A", "y"] = dat3[dat3$g=="A", "y"] + g2_effA # subject of group A have + gv


mod2.2 = lm(y~g2, data=dat3)
model.matrix(mod2.2)
summary(mod2.2)


#at3$g2 = relevel(dat3$g2, ref=c("C"))

mod2.2 = lm(y ~ g2 , data=dat3)
summary(mod2.2)

plot(allEffects(mod2.2))


#### multiple regression
# y = gx1 + gx2 + bx + q + e
mod3 = lm(y ~ g2 + x, data=dat3)
summary(mod3)
hist(residuals(mod3))

plot(allEffects((mod3)))


