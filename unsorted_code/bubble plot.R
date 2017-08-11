{\rtf1\ansi\ansicpg1252\cocoartf949\cocoasubrtf350
{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
\paperw11900\paperh16840\margl1418\margr1134\vieww9000\viewh8400\viewkind0
\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\ql\qnatural\pardirnatural

\f0\fs24 \cf0 model=RM(raschdat1)\
parameters=person.parameter(model)\
item=itemfit(parameters)\
person=personfit(parameters)\
\
bubble=function(x) \{ \
temp=itemfit(person.parameter(x))$i.outfitMSQ 
\b ### devi sostituire questa parte, dall oggetto ottenuto dall'itemfit vanno presi Chisq e gradi di libert\'e0 per ottenere il p\
poi plotterai solo il p mettendo sullo 0.5 una soglia\

\b0 \
\
temp2=NULL\
\pard\pardeftab1134\ql\qnatural
\cf0 for (i in 1:length(x[[11]])) \{ temp2[i]=(x[[11]])[[i]]\} ###recupera la stima del parametro\
plot(temp,temp2, xlim=range(0,1), cex=1.5)\
abline(v=c(0.5), col="red")\}\
\
\
### x[11] prende la lista betapar dove ci  sono le stime dei parameteri}