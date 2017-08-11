plot.mer=function(model)
{
Temp=VarCorr(model)
Residerror=as.numeric(attr(Temp, "sc"))

## nelle y plotto gli standardized residuals. Formula per calcolarli presa da Pinheiro e Bates pag.11

plot(fitted(model), residuals(model)/Residerror, ylim=c(-max(abs(residuals(model)/Residerror)), max(abs(residuals(model)/Residerror))))

abline(h=0)


}