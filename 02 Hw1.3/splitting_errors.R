##
# The k-means clustering algorithm makes a splitting error when it puts two objects of the same class in different clusters.
# Each pair of objects can give an error. (With n objects, there are n(n − 1)/2 different pairs).
##

splitting_errors <- function(class_vector, cluster_vector){
  
  x <- t(combn(class_vector, 2))
  y <- t(combn(cluster_vector, 2))
  
  indx <- which(x[,1]==x[,2])
  indy <- which(y[indx,1]!=y[indx,2])
  
  split.errors <- length(indy)
  
  return(split.errors)
}
