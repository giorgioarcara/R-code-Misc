tostpairs = function (dat, vars, contr, dep, wid, p.adjust.methods = "none", 
          paired = "CHECK", ...) 
{
  dat$newfactor = apply(data.frame(dat[, vars]), 1, function(x) {
    paste(x, collapse = "_")
  })
  dat$newfactor = factor(dat$newfactor)
  dat = aggregate(dat[, dep], list(dat$newfactor, dat[, wid]), 
                  mean)
  names(dat) = c("newfactor", "wid", dep)
  contrast.names = NULL
  p.values = NULL
  #t.values = NULL
  df = NULL
  mean.1 = NULL
  mean.2 = NULL
  pair=NULL
  if (contr[[1]][[1]] == "all") {
    combs = combn(levels(dat$newfactor), 2)
    contr = list(NULL)
    length(contr) = dim(combs)[2]
    for (k in 1:dim(combs)[2]) {
      contr[[k]] = combs[, k]
    }
  }
  for (i in 1:length(contr)) {
    
    # check within/between
    
    if (paired=="CHECK"){
      # check se within o between (if wid are the same)
      wid.diff=setdiff(dat[dat$newfactor %in% contr[[i]][[1]], "wid"],dat[dat$newfactor %in% contr[[i]][[2]], "wid"])
      wid.check=!length(wid.diff)>0
      
      if (wid.check==TRUE){
        res = tost(dat[dat$newfactor %in% contr[[i]][[1]], 
                         dep], dat[dat$newfactor %in% contr[[i]][[2]], dep], 
                     paired = TRUE, ...)
        # aggiorno campo pair
        pair=c(pair, TRUE)
      }
      if (wid.check==FALSE){
        res = tost(dat[dat$newfactor %in% contr[[i]][[1]], 
                         dep], dat[dat$newfactor %in% contr[[i]][[2]], dep], 
                     paired = FALSE, ...)
        pair=c(pair, FALSE)
      }
    }
    
    if (paired==TRUE|paired==FALSE){
    res = equivalence::tostdat[dat$newfactor %in% contr[[i]][[1]], 
                     dep], dat[dat$newfactor %in% contr[[i]][[2]], dep], 
                 paired = paired, ...)
    pair=c(pair, paired)
    }
    contrast.names = c(contrast.names, paste(contr[[i]][[1]], 
                                             "vs", contr[[i]][[2]], sep = " "))
    p.values = c(p.values, res$tost.p.value)
    #t.values = c(t.values, res$statistic)
    df = c(df, res$parameter)
    mean.1 = c(mean.1, mean(dat[dat$newfactor %in% contr[[i]][[1]], 
                                dep]))
    mean.2 = c(mean.2, mean(dat[dat$newfactor %in% contr[[i]][[2]], 
                                dep]))
    
  }
  p.values.corr = signif(p.adjust(p.values, p.adjust.methods), 
                         2)

  results = data.frame(contr = contrast.names, tost.p.value = p.values.corr, 
                        df = df, mean.1 = mean.1, mean.2, pair)
  attr(results, "p.corr") = p.adjust.methods
  cat("p values adjustment = ", p.adjust.methods, "\n")
  return(results)
}
