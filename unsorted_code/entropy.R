# l'entropia è in genere calcolata sulle probabilità.
# Negli studi di linguaggio l'entropia viene spesso modificata in termini di frequenza relativa.
# (la frequenza relativa non è altro che una misura della probabilità di una forma rispetto ad una serie di altre forme. Ad esempio la probabilità di avere capobanda, all'interno di tutti i composti che hanno capo come primo costituente)

entropy=function(x){
	x=x/sum(x)
	entropy.value=-sum(x*(log2(x)))
	return(entropy.value)
}