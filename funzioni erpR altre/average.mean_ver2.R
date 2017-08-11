### INCOMPLETA!!!

average.mean <-

# in questa funzione non uso più base e numbers, ma obj. Non l'ho più sviluppata perché creava casino di consistenza tra soggetti.

function(obj, win.ini, win.end, Subject.n=F, env=.GlobalEnv, startmsec=-200, endmsec=1200)
	{	
		datall=NULL		
		for (i in 1:length(obj))
		{
		average.temp=eval(parse(file="", text=obj[i]), env=env)
		Subject_name=comment(eval(parse(file="", text=obj[i]), env=env))
		average.temp=colMeans(average.temp[round(msectopoints(win.ini,dim(average.temp)[1],startmsec, endmsec)):round(msectopoints(win.end,dim(average.temp)[1],startmsec, endmsec)),])
		average.temp=data.frame(t(average.temp))
		if (Subject.n==TRUE)
		{
			average.temp$Subject.n=i
		}
		average.temp$Subject_name=Subject_name
		average.temp$Subject=obj[i]
		datall=rbind(datall, average.temp)
		}
		rownames(datall)=1:dim(datall)[1]
		return(datall)
		}
