#####TEST U DI MANN-WHITNEY##### questa funzione calcola la statistica U di Mann-Whitney. La fonte delle formule è "Statistica", Murray R.Spiegel, Mc Graw Hill. Funzione scritta da Giorgio Arcara

###ho verificato l'effettiva correttezza della funzione con statistica: l'unica differenza è che non effetta il calcolo di uno z aggiustato, cosa che in statistica avviene.

u.test<-function(dip, group, details="F")#chiede in input la variabile dipendente e la variabile di gruppo
 {livelli<-sort(levels(factor(group)));#non va bene, perché non puoi sapere in che ordine li mette!!
 N1<-length(na.exclude(dip[group==livelli[1]])); #calcolo la numerosità di N1
 N2<-length(na.exclude(dip[group==livelli[2]]));#calcolo la numerosità di N2
 Rango<-rank(dip); 
 tabella<-cbind(Rango, group); 
 R1<-sum(na.exclude(Rango[group==livelli[1]])); #calcola la somma dei ranghi del primo gruppo
 R2<-sum(na.exclude(Rango[group==livelli[2]])); #calcola la somma dei ranghi del secondo gruppo
 U<- (N1*N2)+((N1*(N1+1))/2)-R1;  #calcola la statistica U
 muU<-(N1*N2)/2; #stima della media mu della popolazione
 varU<-(N1*N2*(N1+N2+1))/12; #stima della varianza della popolazione
 zU<- (U-muU)/(sqrt(varU)); #calcolo del punto z
 pvalue<-2*(1-pnorm(abs(zU))); #calcolo del p, in base alla percentuale di area delimitata dal punto z
 if(details=="F")
 {results<-cbind(N1,N2,U,round(zU,4), round(pvalue,4))
 	results<-as.data.frame(results); 
 	colnames(results)<-c("N1", "N2", "U", "z", "p");rownames(results[1])<-("results")
 	return(list("Mann-Whitney U  MAIN RESULTS"=results))}
 if(details=="T")
 	{results<-cbind(livelli[1],livelli[2],N1, N2, R1, R2, U, muU, varU, round(zU,4), round(pvalue,4)); 
 results<-as.data.frame(results); colnames(results)<-c("gruppo1", "gruppo2","N1", "N2", "R1", "R2", "U", "muU", "varU", "z", "p"); 
return(list("Mann-Whitney U  DETAILED RESULTS"=results))}
}