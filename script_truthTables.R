setwd(my_dir)
source(file = "TruthTables.R")
source(file = "writeTruth.R")

#read graph
g = igraph::read.graph(file = path, format = "graphml")
#adapt to needed format
#E(g)$"Relationship Type" = sample(x = c("activation", "inhibition"), size = length(E(g)), replace = TRUE)
E(g)$tipo = E(g)$"Relationship Type"
E(g)$peso = ifelse(E(g)$tipo == "activation", 1, -3)
#make truth tables 
tablas = truth_tables(g)
#write out
setwd(dir = outback)
writeTruth(tablas = tablas)
