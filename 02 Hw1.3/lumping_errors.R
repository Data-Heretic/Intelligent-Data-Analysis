##
# The k-means clustering algorithm makes a lumping error whenever it assigns two objects of different classes to the
# same cluster. Each pair of objects can give an error. (With n objects, there are n(n − 1)/2 different pairs).
##

lumping_errors <- function(class_vector, cluster_vector){
  
  x <- t(combn(class_vector, 2))
  y <- t(combn(cluster_vector, 2))
  
  indx <- which(x[,1]!=x[,2])
  indy <- which(y[indx,1]==y[indx,2])
  
  lump.errors <- length(indy)
  
  return(lump.errors)
}
