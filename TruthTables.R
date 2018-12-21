#make truth tables for directed networks
library(igraph)

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
  matriz = as.data.frame(matriz)%>%arrange_all()
  matriz = as.matrix(matriz)
  colnames(matriz) = df$input
  
  #make truth table
  
  ##for nodes without regulator 
  ###add an autoregulator
  if(nrow(df)==0){
    matriz2 = data.frame(V1 = 0:1)
    colnames(matriz2) = node
  }else
  
  ##for nodes with only one regulator 
  if(nrow(df)==1){
  matriz2 = as.data.frame(apply(X = matriz, 
                                  MARGIN = 1, 
                                  FUN = function(r){r*df$value}
                                )
                          )
  colnames(matriz2) = colnames(matriz)
  
  }else{
    matriz2 = as.data.frame(t(apply(X = matriz, 
                                    MARGIN = 1, 
                                    FUN = function(r){r*df$value})
    )
    )  
  }
  
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