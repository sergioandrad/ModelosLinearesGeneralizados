##################################################################################
# Partial Least Squares
# Autor: Sergio Henrique Andrade de Azevedo;
# Data de atualizacao: 12-03-2022.
# Descricao: Estima fatores por partial least squares.
##################################################################################
  # Pacote utilizados
  library(tidyverse)

  # Simulando dados
  set.seed(1)
  covs  <- cbind(rnorm(n,mean=0,sd=1),rnorm(n,mean=0,sd=1),rnorm(n,mean=0,sd=1))
  y     <- covs %*% matrix(c(0.1, 1.7, -0.4)) 
  dados <- as.data.frame(cbind(covs,y)) %>% rename(y=1,x1=2,x2=3,x3=4)

  # Partial Least Squares
  
  lm1 = lm(formula = y ~ x1+x2+x3-1, data=dados)
  z1 <- as.matrix(dados[,2:4]) %*% matrix(lm1$coefficients)
  