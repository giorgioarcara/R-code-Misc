# ## PROVE Topographic scalp
# topo.scalp

library(erpR)


# recupero le coordinate in generale degli elettrodi.
coord=topoplot(return.coord=TRUE)

coord$el.name=toupper(coord$el.name)

data(ERPsets)
# compute the average of subjects 1 to 20 for the condition 
# specified by the string "Exp1_word_subj".
	
word=grandaverage("Exp1_word_subj", 1:20, erplist=ERPsets)

# faccio toupper altrimenti non posso selezionare elettrodi centrali,
# che hanno delle minuscole (rispetto) all'elenco degli elettrodi
names(word)=toupper(names(word))


# seleziono gli elettrodi che voglio effettivamente plottare
el.toplot=names(word)[1:31]

# recupero coordinate degli elettrodi che voglio plottare
curr.elec.coord=coord[toupper(coord$el.name)%in%toupper(el.toplot),]


# creo le x e y con le coordinate.
#if (projection=="equalarea"){
	x=curr.elec.coord$x *sqrt(1/(1 + curr.elec.coord$z))
	y=curr.elec.coord$y *sqrt(1/(1 + curr.elec.coord$z))

#}


## prova

## convert original x from - 1 to 1, to 0 to 1
# to uniform with fig=c(0,1,0,1)
	
# NOTA: x.marin e per dare un po' di margine al plot (altrimenti 
# gli elettrodi più esterni sarebbero troppo vicini al bordo
	
# calcolo questi x2 che sono le coordinate degli elettrodi dopo 
# 1) aver aggiunto dei margini
#2) avere convertito le x e le y da -1, 1 a 0,1

###
# VARIANDO X.MARGIN E Y.MARGIN, VARI LA GRANDEZZA DEL CERCHIO 
# (QUINDI ELETTRODI PIU' O MENO VICINI)
	
x.margin=1
y.margin=1

x1=(x-min(x-x.margin))
x2=x1/max(x1+x.margin)

y1=(y-min(y-y.margin))
y2=y1/max(y1+y.margin)


par(mar=c(0,0,0,0), fig=c(0,1,0,1))
plot(x2, y2, xlim=c(0-x.margin,1+x.margin), ylim=c(0-y.margin,1+y.margin))


# x.size e y.size determinano la grandezza di ciascun pannello degli erp
# determine size of each erp plot.
x.size=0.1
y.size=0.1


##########################
# PROVE PLOTTING

i= 1
# questo polygon è solo per fare un check
polygon(x=c(x2[i]-(x.size/2), x2[i]-(x.size/2), x2[i]+(x.size/2), x2[i]+(x.size/2)), y=c(y2[i]-(y.size/2),y2[i]+(y.size/2), y2[i]+(y.size/2), y2[i]-(y.size/2)))

# check di un elettrodo
points(x2[1], y2[1], cex=2)

# ho provato a fare il più possibile delle corrispondenze tra la topografia che gli do io
# e i plot che faccio di ciascun elettrodo.

# la corrispondenza (in proporzione) è perfetta, ma non sono riuscito a plottare esattamente
# nelle posizioni desiderate

# questo non pare possibile facilmente: in R è difficile fare un plot e poi fare un altro plot in posizione, a meno che non si usi
# una funzione come subplot(), che si trova in Hmisc.

# voglio evitare di usarla percHè comporterebbe aggiungere la dependency al pacchetto.
# in linea generale il grafico sembra perfettamente rispettare le proporzioni, solo
# leggermente cambia la dimensione






# uso x2 come ciclo possibile
for (i in 1:dim(curr.elec.coord)[1]){
  
  par(mar=c(0,0,0,0), new=TRUE, fig=c(x2[i]-(x.size/2), x2[i]+(x.size/2), y2[i]-(y.size/2), y2[i]+(y.size/2)))
  
  
  #plot(0.5, 0.5, frame.plot=F, xlim=c(0,1), ylim=c(0,1))
  #plot(word[, as.character(curr.elec.coord$el.name[i]) ], type="l", frame.plot=F, axes=F)
  erp(word[, as.character(curr.elec.coord$el.name[i]) ], frame.plot=F, draw.xaxis=F, draw.yaxis=F )
  
}

