gg_checkna=function(data, rel_col, ID_col){
  # data: data.frame
  # rel_col, all names of columns to be used
  # ID_col column with ID
  require(reshape2)
  require(plyr)
  
  data_na=data[, c(ID_col, rel_col)]
  data_na[, ID_col]=factor(data_na[, ID_col])
  data_na[, rel_col]=as.numeric(is.na(data_na[, rel_col]))
  data_na.m=melt(data_na)
  
  (p <- ggplot(data_na.m, aes(variable, ID)) + geom_tile(aes(fill = value), colour = "white") 
    + scale_fill_gradient(low = "white",  high = "steelblue", guide = FALSE) # guide = FALSE, for the colormap
    + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10))
    + theme(axis.text.y = element_text(size = 8))
)
  
  
}
  