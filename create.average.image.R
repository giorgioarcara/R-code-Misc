create.average.image=function(base, numbers, Electrode, env=.GlobalEnv, step=3, col.skip=4){
		{
	
	comment_text=paste("Subjects averaged: ", paste(base,numbers[1], sep=""))
	average.temp=eval(parse(file="", text=paste(base,numbers[1], sep="")),env=env)
	t.ini=col.skip+1 ## setto da che colonna prendere
	t.end=length(average.temp) #setto colonna finale
	t.step=step
	average.temp=average.temp[average.temp$Electrode==Electrode,seq(t.ini,t.end,t.step)]
	cat(".")
		for (i in 2:length(numbers))
		{
			average.temp2=eval(parse(file="", text=paste(base,numbers[i], sep="")),env=env)
			average.temp2=average.temp2[average.temp2$Electrode==Electrode,seq(t.ini,t.end,t.step)]
			average.temp=rbind(average.temp, average.temp2)
			comment_text=paste(comment_text,paste(base,numbers[i], sep=""),"\n")
			cat(".")
		}
		average=average.temp
		comment(average)=comment_text
		return(average)
		}
	
	
	
}