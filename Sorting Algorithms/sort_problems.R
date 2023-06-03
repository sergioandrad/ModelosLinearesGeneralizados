#####################################################
# Sorting problems                                  #
# Author: Sergio Andrade                            #
# Creation Date: 15-05-2023                         #
# Description: Brief stufy of sorting algorithms.   #
# based on Cormen at al. book Algorithms.           #
#####################################################
# My attempt
unsorted <- c(4,3,2,7,-1,-1,-1,0)

sorted    <- c()
sorted[1] <- unsorted[1]
for (i in 2:length(unsorted)) {
  ns     <- length(sorted)
  marker <- 0
  for (j in 1:ns) {
    if(sorted[j] > unsorted[i]) {
      for (k in ns:j) {
        sorted[k+1] <- sorted[k] 
      }
      sorted[j] <- unsorted[i]
      break
    } else {
      marker <- marker + 1
    }
  }
  if(marker == ns){
    sorted[ns+1] <- unsorted[i]
  }
  cat(sorted, "\n")
}
sorted

# Coding the book's solution
# We may notice that the book's solution is more memory efficient
# as it uses the same object for the unsorted and sorted vectors.

a_vec <- c(4,3,2,7,-1,-1,-1,0)
n     <- length(a_vec)
for (j in 2:n) {
  i   <- j-1
  key <- a_vec[j]
  while(i>0 && a_vec[i]>key){
    a_vec[i+1] <- a_vec[i]
    i          <- i - 1
  }
  a_vec[i+1] <- key
}
a_vec

# Rewrite the INSERTION-SORT procedure to sort into 
# nonincreasing instead of nondecreasing order.
## Notice that it suffices to flip the inequality inside 
## the while loop condition.

a_vec <- c(4,3,2,7,-1,-1,-1,0)
n     <- length(a_vec)
for (j in 2:n) {
  i   <- j-1
  key <- a_vec[j]
  while(i>0 && a_vec[i]<key){
    a_vec[i+1] <- a_vec[i]
    i          <- i - 1
  }
  a_vec[i+1] <- key
}

# Linear search

a_vec <- c(4,3,2,7,-1,-3,-2,0)
output <- NULL
obj_val <- -1
for (i in 1:length(a_vec)) {
  if(a_vec[i]==obj_val){
    output <- i
  }
}
output
a_vec[output]




