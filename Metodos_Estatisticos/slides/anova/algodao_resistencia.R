#Dados

#Dados de um experimento com objetivo de identificar a melhor concentração de algodão - melhor = aquela que confere maior resistência ao tecido fabricado; 

dados <- NULL

dados$Concentracao <- c(15,15,15,15,15,20,20,20,20,20,25,25,25,25,25,30,30,30,30,30,35,35,35,35,35)
dados$Repeticao <- c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5)
dados$Resistencia <- c(7,7,15,11,9,12,17,12,18,18,14,18,18,19,19,19,25,22,19,23,7,10,11,15,11)

dados <- as.data.frame(dados)

install.packages("stats")
library(stats)

anova = aov(Resistencia~factor(Concentracao), data = dados)
summary(anova)


install.packages("agricolae")
library(agricolae)

tukey_res<- HSD.test(anova, trt = 'factor(Concentracao)')


tukey_res
