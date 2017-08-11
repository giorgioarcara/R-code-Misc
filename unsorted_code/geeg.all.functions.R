setwd("~/Desktop/R files/funzioni erpR")
options(encoding="macroman")
for (i in 1:length(dir())){
	source(dir()[i], , chdir=T)
}

source("/Users/giorgioarcara/Desktop/R files/char2fac.R")
source("/Users/giorgioarcara/Desktop/R files/rearrange 0.2.R")
source("/Users/giorgioarcara/Desktop/R files/cn.R")
source("/Users/giorgioarcara/Desktop/R files/factorall.R")
source("/Users/giorgioarcara/Desktop/R files/ezresults 3.0.R")
source("/Users/giorgioarcara/Desktop/R files/contr.within 0.5.R")
source("/Users/giorgioarcara/Desktop/R files/contr.mixed.matrix.R")
source("/Users/giorgioarcara/Desktop/R files/contr.mixed 0.2.R")
source("/Users/giorgioarcara/Desktop/R files/contr.bet.matrix.R")
source("/Users/giorgioarcara/Desktop/R files/contr.within.matrix 0.5.R")
source("/Users/giorgioarcara/Desktop/R files/ezANOVA.nout.R")
source("/Users/giorgioarcara/Desktop/R files/ezANOVA.nout.R")
source("/Users/giorgioarcara/Desktop/R files/plot.nout.R")
source("/Users/giorgioarcara/Desktop/R files/summarytab.R")

source("/Users/giorgioarcara/Desktop/R files/t.test.pairs.R")
source("/Users/giorgioarcara/Desktop/R files/pairwise.t.test.g.R")