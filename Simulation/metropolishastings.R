##################################################################################
# Simulando de uma densidade bivariada por Metropolis-Hastings
# Author: Sergio Henrique Andrade de Azevedo e Matheus Eduardo Baruta Lima;
# Last updated: 08-12-2022.
# Description: Gera uma amostra de exp(-x*(y^2)) usando MH com uma proposta
# simétrica.
##################################################################################
library(tidyverse)
jointDens <- function(x,y){
  exp(-x*y^2)*(x>1)*(y>1)
}

set.seed(1)
Nsim       <- 50000
simDF      <- data.frame()
simDF[1,1] <- 3
simDF[1,2] <- 3
for (i in 2:Nsim){
  # Gerando as exponenciais por inversao
  w1     <- runif(1)
  w2     <- runif(1)
  x      <- -2*log(2*w1)
  y      <- -2*log(2*w2)
  # Avaliando a razao de aceitacao
  rho    <-  jointDens(x,y)/jointDens(simDF[i-1,1],simDF[i-1,2])
  u_test <- runif(1)
  # Checagem de aceitacao dos valores conjuntamente
  simDF[i,1] <- simDF[i-1,1] + (x-simDF[i-1,1])*(u_test<rho)
  simDF[i,2] <- simDF[i-1,2] + (y-simDF[i-1,2])*(u_test<rho)
}

# Histograma da simulacao
colnames(simDF) <- c('x','y') 
simDF %>% pivot_longer(-c()) %>%
ggplot() + geom_histogram(aes(x=value)) + facet_wrap(~name)

