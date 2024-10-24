rm(list=ls())
library(erpR)
data(ERPsets)

source("/Users/giorgioarcara/Documents/R_code_Giorgio/debug_clust_based_erpR/find.clusters.R")
source("/Users/giorgioarcara/Documents/R_code_Giorgio/debug_clust_based_erpR/follow_clust.R")
source("/Users/giorgioarcara/Documents/R_code_Giorgio/debug_clust_based_erpR/spatial_neighbours.R")
source("/Users/giorgioarcara/Documents/R_code_Giorgio/debug_clust_based_erpR/cluster_mass.R")
source("/Users/giorgioarcara/Documents/R_code_Giorgio/debug_clust_based_erpR/cluster.sel.R")


setwd("/Users/giorgioarcara/Documents/R_code_Giorgio/debug_clust_based_erpR")


load("/Users/giorgioarcara/Documents/Lavori Unipd/ERP agreement/Exp numerosity 2 ERP analysis/ExpNumerosity R analysis/R Data/Exp2Second_bc.RData")

Subject.numbers = c(1:4, 6:27)

my_startmsec = -500
my_endmsec = 1500


