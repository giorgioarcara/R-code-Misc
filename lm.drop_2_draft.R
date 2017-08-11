### function to drop non significant terms

lm.drop <- function(mod) {
  mod.coef = data.frame(summary(mod)$coefficients) #recupero i coefficienti del modello
  
  if (dim(mod.coef)[1] > 1) {
    updated.mod = mod
    
    mod.coef = data.frame(summary(updated.mod)$coefficients)
    mod.coef = mod.coef[(2:dim(mod.coef)[1]), ]
    
    while (any(mod.coef[, 4] > 0.05)) {
      #salto l'intercetta e check se c'è almeno un coeff > 0.05
      
      to.drop = rownames(mod.coef[mod.coef[, 4] == max(mod.coef[, 4]), ]) # trovo il valore con p-value più alto.
      
      preds = attr(mod$terms, "dataClasses")
      numerics = preds[preds == "numeric"]
      factors = preds[preds == "factor"]
      
      ### case numeric
      if (to.drop %in% names(numerics)) {
        updated.mod = eval(parse(
          file = "",
          text = paste("update(updated.mod, .~.-", factor_name, ")", sep = "")
        )
        
      } else {
        ## case factor
        my_match = rep(NA, length(factors))
        for (k in 1:length(factors)){
          x = grep(names(factors[k]), to.drop)
          if (length(x)>0)
            my_match[k] = x
        }
        n_factor=which(!is.na(my_match))
        
        factor_name = names(factors[n_factor])
        ps = mod.coef[grep(paste("^", factor_name, sep = ""), rownames(mod.coef)), "Pr...t.."]
        
        if (all(ps > 0.05)) {
          # drop only if all levels are > 0.05
          
          updated.mod = eval(parse(
            file = "",
            text = paste("update(updated.mod, .~.-", factor_name, ")", sep = "")
          )
        }
        
        # check se è un fattore. Se lo è, todrop col fattore.
        #
        
        # factor (candidate to drop)
          my_match = NA
         for (k in 1:length(factors)){
         my_match[k] = grep(names(factors[k]), to.drop)
         }
         n_factor=which(!is.na(my_match))
        
         if(n_factor>0){ #case factor
        
        # find all terms with that factor
        factor_name = names(factors[n_factor])
        ps = mod.coef[grep(paste("^", factor_name, sep = ""), rownames(mod.coef)), "Pr...t.."]
        
        
        if (all(ps > 0.05)) {
          # drop only if all levels are > 0.05
          
          updated.mod = eval(parse(
            file = "",
            text = paste("update(updated.mod, .~.-", factor_name, ")", sep = "")
          )
        }
        
        
      } else {
        #case covariate
        
        
        cat("VARIABLE: ", to.drop, "dropped.\n")
        mod.coef = data.frame(summary(updated.mod)$coefficients)
        mod.coef = mod.coef[(2:dim(mod.coef)[1]), ]
      }
      invisible(updated.mod)
      
    } else {
      cat("only intercept in this model")
      invisible(mod)
    }
  }
  