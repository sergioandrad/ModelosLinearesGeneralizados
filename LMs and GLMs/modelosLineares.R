##################################################################################
# Modelos Lineares Generalizados; Lista 3.
# Autor: Sergio Henrique Andrade de Azevedo;
# Data de atualizacao: 15-08-2021.
# Descricao: Cria matrizes de desenho, obtem postos ajustas modelos propostos nos
# exercicios da Lista 3 da disciplina MLG.
##################################################################################
## Pacotes  
  library(tidyverse) 

## Exercicio 1 ###################################################################

dados1 <- matrix(c(2.23, 9.66, 12.37, 
                   2.57, 8.94, 12.66, 
                   3.87, 4.40, 12.00, 
                   3.10, 6.64, 11.93, 
                   3.39, 4.91, 11.06, 
                   2.83, 8.52, 13.03, 
                   3.02, 8.04, 13.13, 
                   2.14, 9.05, 11.44,
                   3.04, 7.71, 12.86,
                   3.26, 5.11, 10.84,
                   3.39, 5.05, 11.20,
                   2.35, 8.51, 11.56,
                   2.76, 6.59, 10.83,
                   3.90, 4.90, 12.63,
                   3.15, 6.96, 12.46), 
                   byrow =T, ncol=3) %>% data.frame() %>% rename(y=X3)

#1. Faca o grafico de y vs x1. Comente sobre alguma relacao ou algo n~ao usual no grafico.

ggplot(data = dados1) + geom_point(aes(x=X1, y=y))
ggplot(data = dados1) + geom_point(aes(x=X2, y=y))
ggplot(data = dados1) + geom_point(aes(x=X1, y=X2))

lm(y~X1, data = dados1)
lm(y~X2, data = dados1)
lm(y~X1+X2, data= dados1)
#2. Faca o grafico de y vs x2. Comente sobre alguma relacao ou algo n~ao usual no grafico.

#3. Faca o grafico de x1 vs x2. Comente sobre alguma relacao ou algo n~ao usual no grafico.

#4. Faca a regress~ao de y em x1. Faca o grafico dos residuos contra x2.

#5. Faca a regress~ao de y em x2. Faca o grafico dos residuos contra x1.

#6. Faca a regress~ao de y em x1 e x2 simultaneamente. Compare os coeficientes obtidos na
#regressao conjunta com os das regress~oes marginais. Compare tambem os coeficientes de
#correlacao (multipla).

## Exercicio 4 ###################################################################
   # Criando X com a restricao i >= j nos niveis de A e B
   X <- data.frame()
   iter <- 1
   for (i in 1:5) {
     for (j in 1:5) {
       if(i >= j){
       X[iter,1] <- rnorm(n=1)
       X[iter,2] <- i
       X[iter,3] <- j
       iter <- iter + 1
       } #if
     } # for i
   } # for j
   
   # Dataset - A B C fatores
   X1 <- X %>%
          mutate(A=as.factor(V2), B=as.factor(V3), C=as.factor((V2-V3+1))) %>%
          select(-c(V2,V3)) %>%
          rename(Y=V1)
   
   X1 %>% filter(B==1)
   X1 %>% filter(C==1)
   
   
   # Dataset - A B C covariaveis
   X2 <- X %>% as.data.frame() %>%
     mutate(A=V2, B=V3, C= (V2-V3+1)) %>%
     select(-c(V2,V3)) %>%
     rename(Y=V1)
   
   # Ajustando modelos fatoriais
   options(contrasts = rep("contr.treatment", 2))
   lmAB  <- lm(Y ~ A+B-1,  data = X1)
   lmAC  <- lm(Y ~ A+C-1,  data = X1)
   lmBC  <- lm(Y ~ B+C-1,  data = X1)
   lmABC <- lm(Y ~ A+B+C-1,data = X1)
  
   # Verificando os \hat{eta}
   cbind(AB=lmAB$fitted.values,
         AC=lmAC$fitted.values,
         BC=lmBC$fitted.values,
         ABC=lmABC$fitted.values)
   
   # Verificando as somas de quadrados
   lapply(list(lmAB,lmAC,lmBC,lmABC), anova)

   # Ajustando os modelos com A B C covariaveis
   lmAB2  <- lm(Y ~ A+B,  data = X2) 
   lmAC2  <- lm(Y ~ A+C,  data = X2)
   lmBC2  <- lm(Y ~ B+C,  data = X2)
   lmABC2 <- lm(Y ~ A+B+C,data = X2)
   
   cbind(AB=lmAB2$fitted.values,
         AC=lmAC2$fitted.values,
         BC=lmBC2$fitted.values,
         ABC=lmABC2$fitted.values)
   
   # Verificando as somas de quadrados
   lapply(list(lmAB2,lmAC2,lmBC2,lmABC2), anova)
   
   # Obtendo o posto das matrizes
   contrastes <- lapply(X1[,sapply(X1, is.factor)], contrasts, contrasts=FALSE)
   XAB  <- model.matrix(Y~A+B-1,  data = X1, contrasts.arg = contrastes)
   XAC  <- model.matrix(Y~A+C-1,  data = X1, contrasts.arg = contrastes)
   XBC  <- model.matrix(Y~B+C-1,  data = X1, contrasts.arg = contrastes)
   XABC <- model.matrix(Y~A+B+C-1,data = X1, contrasts.arg = contrastes)
   
   postos <- lapply(list(XAB,XAC,XBC,XABC), (function(x) qr(x) %>% pluck('rank')))
   
## Exercicio 5 ###################################################################
  # Criando o dataset
  X5  <- data.frame()
  iter <- 1
  levels <- 1:4
  for (i in levels) {
    for (j in levels) {
      for (k in levels) {
        for (l in levels) {
          if(all(c(i,j,k,l)[-1] != i) & all(c(i,j,k,l)[-2] != j) & all(c(i,j,k,l)[-3] != k)){
            X5[iter,1] <- i
            X5[iter,2] <- j
            X5[iter,3] <- k
            X5[iter,4] <- l
            X5[iter,5] <- rnorm(n=1)
            iter <- iter + 1
            } #if 
          } # for i
        } # for j
      } # for k
    } # for l
  
  X5 <- X5 %>% rename(A=V1,B=V2,C=V3,D=V4,Y=V5) %>% mutate_at(-5, as.factor) 
  
  # Criando as matrizes de desenho irrestritas e obtendo o posto via QR
  cont_arg = lapply(X5[,sapply(X5, is.factor)], contrasts, contrasts=FALSE)
  model.matrix(Y ~ A+B+C,   data=X5, contrasts.arg = cont_arg)  %>% qr()
  model.matrix(Y ~ A+B+C+D, data=X5, contrasts.arg = cont_arg)     %>% qr()
  model.matrix(Y ~ (A+B+C+D)^2, data=X5, contrasts.arg = cont_arg)  %>% qr()
  
## Exercicio 6 ###################################################################
  # Criando o dataset
  dados <- rbind(
              c(0,2,45),
              c(0,4,47),
              c(0,6,46),
              c(0,8,46),
              c(10,2,46),
              c(10,4,43),
              c(10,6,41),
              c(10,8,37),
              c(20,2,34),
              c(20,4,28),
              c(20,6,21),
              c(20,8,16)) %>%
                data.frame() %>%
                mutate(X1=as.factor(X1)) %>%
                rename(tempe = X1, semanas = X2, Y =X3)
  
  # Ajustando o modelo só com a interacao temperatura x semanas
  modelo1 <- glm(Y ~ 1+ tempe*semanas -tempe -semanas,data= dados, family = Gamma(link='log')) 
  
  # Produzindo as curvas geradas pelo modelo com link log para o decaimento
  ggplot(data.frame(x=0:500),aes(x),) +
    geom_function(fun = (function(x) exp(modelo1$coefficients[1]+modelo1$coefficients[2]*x)), aes(colour = '0'), size = 2) +
    geom_function(fun = (function(x) exp(modelo1$coefficients[1]+modelo1$coefficients[3]*x)), aes(colour = '10'), size = 2) +
    geom_function(fun = (function(x) exp(modelo1$coefficients[1]+modelo1$coefficients[4]*x)), aes(colour = '20'), size = 2) +
    ggtitle('Decaimento da concentracao de acido ascorbico') +
    xlab('Semanas de Armazenamento') + ylab('Acido Ascorbico') +
    scale_colour_discrete(guide = guide_legend(title.position = "top", label.position = "right"), name = 'Temp. F°')
  
  
  dados3x4 <- dados %>% mutate(semanas = as.factor(semanas))


  # Ajustando os modelos lineares 3x4 e com contraste linear e quadratico
  # O livro nos da as somas de tres sacos, logo divindo por 3 e abrindo em linhas
  # obtemos o seguinte dataset aproximado:
  dadosAgregados <- rbind(
    c(0,2,45),
    c(0,4,47),
    c(0,6,46),
    c(0,8,46),
    c(10,2,46),
    c(10,4,43),
    c(10,6,41),
    c(10,8,37),
    c(20,2,34),
    c(20,4,28),
    c(20,6,21),
    c(20,8,16)) 
  
  dadosCochran <- rbind(dadosAgregados,dadosAgregados,dadosAgregados) %>%
    data.frame() %>%
    mutate(X1=as.factor(X1), X2=as.factor(X2), X3=X3/3) %>%
    rename(tempe = X1, semanas = X2, Y =X3)
  
  # Ajustando com desenho fatorial 3x4
  options(contrasts = rep("contr.treatment", 2)) # Coloca contraste polinomial ortogonal
  modelo3  <- lm(Y ~ 1+ tempe*semanas,data= dadosCochran)
  modelo3 %>% summary()
  
  # Configura contraste associado aos fatores
  attr(dadosCochran$semanas, "contrasts") <- contr.poly(4)
  attr(dadosCochran$tempe, "contrasts")   <- contr.poly(3)
  lmCochran <- lm(Y ~ 1 + tempe*semanas, data= dadosCochran) 
  lmCochran %>% summary()
  lmCochran %>% anova()
  