library(igraph)

#load graph
g = read.graph(file = "ESR_duplex/RED_5.graphml", format = "graphml")
#make a new "relationship type" attribute named "tipo" for comfort
E(g)$tipo = E(g)$"Relationship Type"
#make a new attribute "peso" such that activation is +1, inhibition -3
E(g)$peso = ifelse(E(g)$tipo == "activation", 1, -3)
E(g)$peso
E(g)$tipo

### make contribution table 

#get indegree neighbors
neighbors(graph = g, v = "AR", mode = "in")
#get values of input edges
x = E(g)[to("AR")]
tail_of(graph = g, es = x)$name
df = data.frame(input = tail_of(graph = g, es = x)$name, 
                value = x$peso
                )

#make a data frame with all possible states
tail_of(graph = g, es = x)$name
#get length
ene = length(tail_of(graph = g, es = x)$name)
#make 0,1 combinatorial 
matriz = expand.grid(lapply(numeric(ene), function(x) 0:1))
matriz
colnames(matriz) = df$input
#make truth table

matriz2 = as.data.frame(t(apply(X = matriz, 
                            MARGIN = 1, 
                            FUN = function(r){r*df$value})
                      )
                    )

#add truth table result for node of interest
matriz2$x = rowSums(matriz2)
matriz2$x = ifelse(matriz2$x > 0, 1, 0) #substitute positives for 1, else 0
matriz2   = data.table::setnames(matriz2, 
                                 old = "x", 
                                 new = "AR"
                                 )



truthTableNode = function(g, node){
  #takes directed graph, and a node
  #with edge argument "peso" for plus or minus contribution
  #returns a list 
  #with data frame of contribution of inputs for node  
  #and  truth table data frame
  resultList = list()
  #get values of input edges
  x = E(g)[to(node)]
  tail_of(graph = g, es = x)$name
  df = data.frame(input = tail_of(graph = g, es = x)$name, 
                  value = x$peso
  )
  
  resultList$contribution = df
  
  #make 0,1 combinatorial 
  matriz = expand.grid(lapply(numeric(length(tail_of(graph = g, es = x)$name)), 
                              function(x) 0:1)
                       )
  colnames(matriz) = df$input
  #make truth table
  
  matriz2 = as.data.frame(t(apply(X = matriz, 
                                  MARGIN = 1, 
                                  FUN = function(r){r*df$value})
  )
  )
  
  #add truth table result for node of interest
  matriz2$x = rowSums(matriz2)
  matriz2$x = ifelse(matriz2$x > 0, 1, 0) #substitute positives for 1, else 0
  matriz2   = data.table::setnames(matriz2, 
                                   old = "x", 
                                   new = node
  )
  resultList$truthTable = matriz2
  return(resultList)
  }


truth_tables = function(g){
  nodos = V(g)$name
  x =lapply(X = nodos, FUN = truthTableNode, g = g)
  names(x) = nodos
  return(x)
}

prueba = truth_tables(g)
str(prueba)
prueba$AR$truthTable
prueba$CR$truthTable
