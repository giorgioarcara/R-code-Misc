rm(list=ls())

load("example_data.RData")

library(ggplot)
library(rms)
library(effects)

# first change the datadist witht he data.frame to be used (it is mandatory)
d=datadist(dat.con)
options(datadist="d")


Percentuali.lm.co_rcs=ols(Percentuali~rcs(scolarita,3)+sesso, dat.con)


mymodel=Percentuali.lm.co_rcs

#### education effects
part_eff=Predict(mymodel, scolarita=seq( min(dat.con$scolarita), max(dat.con$scolarita), ( max(dat.con$scolarita) - min(dat.con$scolarita) ) / 50 ) )

part_eff$xobs=part_eff$scolarita # aggiungo questa riga per riciclare codice. Replico la colonna con la variabile fattore di interesse (sesso).

## IMPORTANTE!
# devo cambiare l'oggetto in data.frame

part_eff=as.data.frame(part_eff)


#######
### PLOT WITH MEANS AND CONFIDENCE BARS

mydat=part_eff

Percentuali.edu=ggplot(mydat, aes(xobs, yhat)) + # where to take data (x and y)
  geom_line(data=mydat)+ # plot line.
  geom_ribbon(data=mydat, mapping=aes(ymin=lower, ymax=upper), alpha=0.3, colour="black")+ # confidence bands
  labs(x="education", y="score") 



## example graph for covariate 
print(Percentuali.edu)



### gender effect
part_eff=Predict(mymodel,  sesso=c("F", "M") )

part_eff$xobs=part_eff$sesso # aggiungo questa riga per riciclare codice. Replico la colonna con la variabile fattore di interesse (sesso).

part_eff=as.data.frame(part_eff)


mydat=part_eff

Percentuali.gender=ggplot(mydat, aes(xobs, yhat))+ # where to take data (x and y)
  geom_point(size=2)+
  geom_line(aes(x=as.numeric(xobs)))+
  geom_errorbar(aes(ymin=lower, ymax=upper), colour="black", width=0.1)+
  scale_y_continuous(breaks=c(6,7,8,9,10), limits=c(6, 9))

####

## example graph for factor
print(Percentuali.gender)

