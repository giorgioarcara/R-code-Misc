## example 



crawford.t <- function(pat.score, control.scores, tails=c("lower", "upper", "two")){
  n = length(control.scores)
  craw.t = (pat.score - mean(control.scores))/( sd(control.scores)*sqrt((n+1)/n))
  df = n-1
  
  if (tails[1]=="lower"){
  p.val = pt(craw.t, df=df, lower=T)
  }
  if (tails[1]=="upper"){
  p.val = pt(craw.t, df=df, lower=T)
  }
  if (tails[1]=="two"){
    p.val = pt(abs(craw.t), df=df, lower=F) * 2
  }
  
  cat("t = ", craw.t, "\n")
  cat("p value = ", p.val, "\n")
  cat("tails = ", tails[1], "\n")
  
  
  
}