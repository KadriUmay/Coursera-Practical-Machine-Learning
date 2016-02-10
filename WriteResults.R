setwd("c://users//kadriu//documents//GitHub//Coursera-Practical-Machine-Learning")
writefiles <- function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_results/problem_id_",i,".txt")
    write.table(x[i], file=filename, quote=FALSE,
                row.names=FALSE, col.names=FALSE)
  }
}
writefiles(predRf)