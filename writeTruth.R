writeTruth <- function(tablas){
for(i in seq_along(tablas)){
  nomen = names(tablas)[i]
  x = tablas[[i]][[2]]
  out_file = paste0(nomen, ".txt")
  cat(nomen, ncol(x) -1, sep = "\n",file = out_file)
  #y = ifelse(x<1, 0, 1)
  y = ifelse(test = x==-3, 
             yes = 1, no = 
               ifelse(test = x==1, 
                      yes = 1, 
                      no = 0)
             )
  #print(y)
  write.table(x = y, 
              file = out_file, 
              append = TRUE, 
              quote = FALSE, 
              sep = "\t", 
              col.names = TRUE, 
              row.names = FALSE
  )
}
}