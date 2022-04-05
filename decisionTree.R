##################################################################################
# CART: Classification and regression trees.
# Autor: Sergio Henrique Andrade de Azevedo;
# Data de atualizacao: 05-04-2022.
# Descricao: Implementa uma funcao de desvios utilizando currying e uma funcao 
# que recebe uma variavel e um conjunto de dados retornando o melhor corte naquela
# variavel e printa quantas observacoes restaram em cada vértice filho.
# Roadmap: Utilizar as funcoes rss e findCut para construir um modulo de ajuste.
# que retorna a arvore inteira.
##################################################################################
  # Carregando dados e pacotes
  library(ISLR)
  library(tidyverse)
  library(tree)
  data("Hitters")
  
  # Limpando dados
  data <- Hitters %>%
    select(Hits, Years, Salary) %>%
    drop_na() %>%
    mutate(logSalary = log(Salary)) %>%
    select(-Salary)
  
  # Calculo dos desvios
  rss <- function(var, data){
    return(
      function(corte){
        child1 <- data %>% filter({{var}} <  corte) %>% select(logSalary) %>% pull()
        child2 <- data %>% filter({{var}} >= corte) %>% select(logSalary) %>% pull()
        rss    <- sum((child1 - mean(child1))^2) + sum((child2 - mean(child2))^2)
          return(rss)
      }
    )
  }
  
  # Obtencao do corte
  findCut <- function(var, data){
    low <- data %>% select({{var}}) %>% min()
    upp <- data %>% select({{var}}) %>% max()
    opt <- optim(par=c(low+upp/2),fn=rss({{var}},data),method='Brent',lower=low,upper=upp)  
  cat('Child L: ', data %>% filter({{var}} <  opt$par) %>% select(logSalary) %>% pull() %>% length(), '\n',
      'Child R: ', data %>% filter({{var}} >= opt$par) %>% select(logSalary) %>% pull() %>% length(), '\n',
      'Min/max: ',low,', ',upp,
      sep = "")
  return(opt)    
  }
  
  # Comparando com os cortes obtidos via pacote tree do Brian Ripley
  cut0 <- findCut(Years, data)
  cut1 <- findCut(Years, data %>% filter(Years < cut0$par))
  cut2 <- findCut(Hits, data %>% filter(Years < cut0$par) %>% filter(Years < cut1$par))

  treeRef <- tree(logSalary~.,data, split='deviance')
  plot.tree(treeRef)
  
  data %>% select(Years) %>% arrange(Years)
  all(data %>% filter(Years <  4.5) == data %>% filter(Years <  4.036))
  all(data %>% filter(Years >= 4.5) == data %>% filter(Years >= 4.036))
  