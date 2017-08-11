#########################
# PROVA SIMULAZIONE LOGIT
##########################

# scirpt che mostra come il fatto di usare proporzioni può creare un bias nell astima degli effetti per basse proporzioni rispetto all'utilizzo di Logit.
# nasce da una discussione della presentazione di Myrna Schwartz, prima spearker di Academy of Aphasia 2015


# number of observations
n=1000

# creo logit
logits=rnorm(n, mean=0, sd=2)

# creo funzione per inverso di logit
inv_logit=function(x){exp(x)/(1+exp(x))}


# calcolo proporzione dai logit simulati
props=inv_logit(logits)

# simulo un effetto lineare SUI LOGITS
logits_eff=logits+rnorm(n, mean=0, sd=1)

# creo un datagrame

dat=data.frame(logits=logits, logits_eff=logits_eff, props=props)

# creo un gruppo high vs low a partire dalle proporzioni
dat$group="low"
dat$group[dat$prop>0.05]="high"
dat$group=factor(dat$group)

table(dat$group)

# i low sono meno degli high (vedi risultato di table(dat$group)), quindi uso una selezione
# in modo che le numerosità di high e low siano uguali

# ordino per gruppo (prima high poi low)
dat=dat[order(dat$group),]

n.low=table(dat$group)[2]
n.high=table(dat$group)[1]

n.toremove=n.high-n.low

# seleziono solo lo righe (di high) in modo che siano appaiate per numero con low.
dat=dat[(n.toremove+1):dim(dat)[1], ]




mod.logit=lm(logits~logits_eff, dat)

mod.props=lm(props~logits_eff, dat)


mod.logit.group=lm(logits~logits_eff*group, dat)
mod.props.group=lm(props~logits_eff*group, dat)



library(effects)


plot(allEffects(mod.logit.group))

plot(allEffects(mod.props.group))

# NOTA che l'interazione è più forte nel caso di props che in logit (per alcune proporzioni è significativa vs non significativa.)
# ho fatto varie prove, cambiando vari parametri. (come il numero di osservazioni e come splitto tra low e high). Pare che gli effetti
# più drammatici si trovano proprio quando vai a vedere le differenze prossime a zero (quindi consideri i low fino a prop = 0.05)