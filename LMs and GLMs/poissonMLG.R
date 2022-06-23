########################################################################################
# Modelos Lineares Generalizados; 
# Autor: Sergio Henrique Andrade de Azevedo;
# Data de atualizacao: 15-08-2021.
# Descricao: Ajusta o modelo Poisson com ligacao canonica e efeitos fixos usando Fisher
# Scoring.
########################################################################################
  # Pacotes e dados
  library(AER)
  library(tidyverse)
  data('CreditCard')

  # Ajuste com a funcao do R  
  poissonR <- glm(data = CreditCard, family = poisson, formula = active~income+age)
  poissonR$coefficients
  
  # Ajustando por Fisher Scoring
  iter <- 1
  X    <- cbind(intercepto= rep(1,n), CreditCard[,c(3,4)]) %>% as.matrix()
  y    <- as.vector(CreditCard$active)
  n    <- dim(CreditCard)[1]
  W    <- matrix( rep(0,n), ncol = n, nrow = n)
  beta      <- c(0.1,0.1,0.1)
  betaIters <- data.frame()
  betaIters[1,1:3] <- c(0.1,0.1,0.1)
  tol  <- 10^(-3)
  
  while (iter < 2 || sum(abs(betaIters[iter-1,]-betaIters[iter,])) > tol) {
    iter <- iter + 1
    eta  <- X %*% beta                    # preditor linear
    z    <- eta + (y-exp(eta))*exp(-eta)  # working variable
    W    <- diag(c(exp(X %*% beta)))      # matriz de pesos
    beta <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% z # MQG com W e z
    betaIters[iter,1:3] <- beta
    print(beta)
  }
  