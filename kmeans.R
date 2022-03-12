##################################################################################
# K-means.
# Autor: Sergio Henrique Andrade de Azevedo;
# Data de atualizacao: 12-03-2022.
# Descricao: Implementa uma funcao de ajuste de k-means baseado na distancia
# e uma classe S4 que retorna o data.frame rotulado e o df de medias estimadas.
##################################################################################
  # Pacote utilizados
  library(tidyverse)
  
  # Simulando dados
  n = 100
  label <-c(rep('A',n),rep('B',n),rep('C',n))
  
  seed(1)
  x1 <- c(rnorm(n, mean=0, sd=1),rnorm(n, mean=4, sd=1),rnorm(n, mean=10, sd=1))
  x2 <- c(rnorm(n, mean=-1, sd=1),rnorm(n, mean=1, sd=1),rnorm(n, mean=8, sd=1))
  
  data <- cbind(x1,x2) %>% as.data.frame()
  
  ggplot(data=data) + geom_point(aes(x=x1,y=x2,col=label)) + ggtitle('Gráfico de dispersão') + xlab('Covariável 1')+ ylab('Covariável 2')
  

  # Implementando a classe do modelo e a funcao de ajuste
    setClass(Class="kmeansModel",
           representation(
             df="data.frame",
             means="data.frame"
           )
  )
  
  kmeans <- function(df,k,reps){
    means      <- df[sample(dim(df)[1],k),]
    covs       <- dim(df)[2]
    df['labs'] <- NA
    dist       <- rep(0,k)
    for (iter in 1:reps) {
      cat('Iteracao: ', iter, '\n')
      for (i in 1:dim(df)[1]) {
        for (j in 1:k) {
          dist[j] <- (df[i,1:covs]-means[j,1:covs])^2 %>% sum()
          df['labs'][i,] <- which(min(dist)==dist)[[1]]
        }
      }
      means <- df %>% group_by(labs) %>% summarise_all(mean) %>% select(-labs)
      print(means)
    }
    return(new('kmeansModel',df=df,means=means))
  }
  
  clusters <- kmeans(df=data, k=3,reps=10)
  clusters@means
  
  ggplot(data=data) +
    geom_point(aes(x=x1,y=x2,col=label),shape=20,size=3) +
    geom_point(data=clusters@means, aes(x=x1,y=x2), size = 3) +
    ggtitle('Gráfico de dispersão e médias estimadas') +
    xlab('Covariável 1')+ ylab('Covariável 2') +
    geom_text(aes(x=x1,y=(x2+0.25),label=labs),data=clusters@df)
  

