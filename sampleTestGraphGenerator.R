library(igraph)
g = random.graph.game(n = 10, p.or.m = 0.3, type = "gnp", directed = TRUE, loops = TRUE)
E(g)$"Relationship Type" = sample(x = c("activation", "inhibition"), size = length(E(g)), replace = TRUE)
V(g)$name = paste0(LETTERS[1:10], "R")
E(g)$tipo = E(g)$"Relationship Type"
E(g)$peso = ifelse(E(g)$tipo == "activation", 1, -3)
plot(g)
