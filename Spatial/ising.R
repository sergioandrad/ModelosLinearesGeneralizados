# Ising model
  library(tidyverse)

 # Potential and conditional distribution
 potential <- function(x, neighbors, alpha, beta){
   u_lambda <-  exp(alpha*sum(neighbors)+ x*beta %*% neighbors)
  return(u_lambda)
 }   
 
 conditional_distribution <- function(x, neighbors, alpha, beta){
   prob <- potential(x, neighbors, alpha, beta)/(potential(x, neighbors, alpha, beta) + potential(-x, neighbors, alpha, beta))
   return(prob[1,1])
 }
 
 # Simulations
 n      <- 100
 states <- cbind(expand.grid(1:n,1:n),x=rep(-1,n^2)) %>% rename(x=1,y=2,state=3)
 ggplot(data=states) + geom_tile(aes(x=x,y=y,fill=state))
 
 # Gibbs sampler
 alpha     = 2
 beta      = c(.8,.8,-.8,-.8)
 maxiter   = 5000
 acc_count = 0 
 neighbors <- matrix(nrow=4,ncol=1)
 for (i in 1:maxiter) {
   # selecting a pixel and its neighbors
   xnew <- sample(2:99, size=1)
   ynew <- sample(2:99, size=1)
   selected_state <- states[states['x']==xnew  & states['y']==ynew, 3]
   neighbors[1,1] <- states[states$x==(xnew-1) & states$y==ynew,    3]
   neighbors[2,1] <- states[states$x==(xnew+1) & states$y==ynew,    3]
   neighbors[3,1] <- states[states$x==xnew     & states$y==(ynew-1),3]
   neighbors[4,1] <- states[states$x==xnew     & states$y==(ynew+1),3]
   
   # Prob and new state
   prob <- conditional_distribution(x=selected_state, neighbors=neighbors, alpha=alpha, beta=beta)
   states[states['x']==xnew & states['y']==ynew,3] <- sample(x = c(-1,1), size=1, prob = c(1-prob,prob))
   # Prints counter on the console
   cat('\014 Iteration: ', i, 'Prob: ', prob)
 }

 ggplot(data=states) + geom_tile(aes(x=x,y=y,fill=state))

 
 # Updating using Metropolis-Hastings
 alpha  =  0.1
 beta1  =  1
 beta2  = -2
 maxiter= 10000
 acc_count = 0 
 for (i in 1:maxiter) {
    xnew <- sample(2:99, size=1)
    ynew <- sample(1:99, size=1)
    
    selected_state <- states[states['x']==xnew & states['y']==ynew,3]
    neighbor1      <- states[states$x==(xnew-1) & states$y==ynew,3]
    neighbor2      <- states[states$x==(xnew+1) & states$y==ynew,3]
    neighbor3      <- states[states$x==xnew & states$y==(ynew-1),3]
    neighbor4      <- states[states$x==xnew & states$y==(ynew+1),3]
    
    # ratio of the new and old state probabilities
    # exp(-beta*Unew)/exp(-beta*U) = exp(-beta*Unew + beta*U) = exp(beta*(U-Unew))
    acc <- min(exp(beta1*(-selected_state*(neighbor1+neighbor2)+new_state*(neighbor1+neighbor2))+
                   beta2*(-selected_state*(neighbor3+neighbor4)+new_state*(neighbor3+neighbor4))
                   ),1)
    
    if(runif(n=1) < acc){
       states[states['x']==xnew & states['y']==ynew,3] <- -selected_state
       cat('\014 Iteration: ', i, '\n Acc. rate: ', acc,'\n Acceptance count: ', acc_count, '\n Accepted')
       acc_count = acc_count + 1 
    } else {
       cat('\014 Iteration: ', i, '\n Acc. rate: ', acc,'\n Acceptance count: ', acc_count, '\n Rejected')
    }
 }
 ggplot(data=states) + geom_tile(aes(x=x,y=y,fill=state))
 