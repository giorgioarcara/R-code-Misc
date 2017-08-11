##################################
####### NEIGHBORS COUNTER ######
##################################

## "dat" è un vettore di parole delle quali vuoi calcolare i vicini ortografici
## "corpus" è un vettore di caratteri di cui vuoi calcolare i vicini ortografici
### "details" se "TRUE" allora restituisce anche quali sono i vicini ortografici
###"ign.case", serve se si vuole ignorare il fatto che ci siano lettere maiuscole o minuscole nel confronto

neigh=function(dat, corpus, details=FALSE, ign.case=TRUE) {
if (length(dat)>10&length(corpus)>100){
	ask=function(msg) { 
   	cat(msg)
   	readLines(con = stdin(), n = 1)
		}
med=function() {
  risp <- ask("L'operazione potrebbe richiedere molto tempo. Vuoi continuare? (y/n)")
  if (risp=="n"){stop("operazione interrotta!", call.=FALSE)}
  if (risp=="y"){}
	}
		med()
	}
	cat(length(dat)," words to be checked")
	dat=as.character(dat)
	dat.risultati=NULL
	neighbors=as.list(NULL)
	cat("\n")
	for (a in 1:length(dat)) {
		x=dat[a]
		database=corpus[nchar(corpus)==nchar(x)]
		database=unique(corpus[-grep(x, corpus,ignore.case=ign.case)])
		temp1=(strsplit(x,""))
		temp2=rep(temp1, nchar(x))
		for (i in 1:length(temp2)) {
			temp2[[i]][i]="."
		}
		#il ciclo che segue serve per ottenere questo passaggio "." "i" "a" "o" -> ".iao". Non si può evitare?
		temp3=NULL
		for (i in 1:length(temp2)){
		temp3[i]=temp2[[i]][1]
			for (k in 2:length(temp2[[1]])) {        ### in realtà questa lunghezza è uguale per tutti i temp2
			temp3[i]=paste(temp3[i], temp2[[i]][k], sep="")
			}
		}
		temp4=paste("^", temp3, "$", sep="")
		temp5=NULL
		for (i in 1:length(temp4)){
		temp5[i]=length(grep(temp4[i], database, ignore.case=ign.case))
		}
		temp.risultato=sum(temp5)
		dat.risultati[a]=temp.risultato
		if (details=="TRUE") {
			temp.neighbors=NULL
			for (i in 1:length(temp4)){
				if (length(grep(temp4[i], database, value=TRUE, ignore.case=ign.case))!=0){
					temp.neighbors=c(temp.neighbors, (grep(temp4[i], database, value=TRUE, ignore.case=ign.case)))
				}
			}
			if (length(temp.neighbors)!=0) {
			#neighbors[[a]]=list(temp.neighbors) ####questa linea andrà tolta.è per cercare di mettere i nomi alla dat successiva (quella di output)
			neighbors[[a]]=as.vector(as.matrix(temp.neighbors)) ###questa linea è per creare un vettore non riesco direttamente da list a vector.
			}
		}
	cat("\n")
	cat(length(dat)-a, "words left")}
data.risultati=data.frame(dat, as.vector(dat.risultati))
data.risultati[,2]=as.numeric(data.risultati[,2])
names(data.risultati)=c("words", "n.neighbors")
	if (details=="TRUE") {
		output=list(data.risultati, neighbors)
		names(output)[[1]]="neighbors"
		names(output)[2]="details"
			for (i in 1:length(neighbors)) {
				names(output$details)[i]=paste(data.risultati[i, "words"])
			}
		return(output)
	}	
	if (details=="FALSE") {
		output=list(data.risultati)
		names(output)[[1]]="neighbors"
		cat("\n")
		cat("\n")
		return(output)
	}
}

