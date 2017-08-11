prova=function(type="l"){
	plot(rnorm(10), type= if (type=="l") 
	"l"
	else if (type=="p")
	"p"
	)
}

