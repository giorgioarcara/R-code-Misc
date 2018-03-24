# the aim of this code is to simulate a data.frame to check for the degrees of freedom in the following mixde model.

library(lme4)
library(lmerTest)

subj=20
electrode=12
Condition=2

RT = rnorm(subj*electrode*Condition*trials)
electrode_c=rep(as.factor(1:electrode), subj*Condition*trials)
subj_c=rep(as.factor(1:subj), electrode*Condition*trials)
Condition_c=rep(as.factor(1:Condition), subj*electrode*trials)
RT[Condition_c=="1"]=RT[Condition_c=="1"]+100

a = lmer(RT~Condition_c +  (1|subj_c))
