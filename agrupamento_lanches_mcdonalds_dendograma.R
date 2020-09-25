#######################

#ANALISE DE CLUSTER EM R

#AGRUPAMENTO DOS LANCHES DO MC DONALDS

#CLUSTER HIERARQUICO

######################

#Carregando as biliotecas
library(tidyverse) #manipulação de dados
library(cluster)   #cluster
library(factoextra)#metodo elbow
library(gridExtra) #grafico de dispersao

#Carregando a base de dados
dados <- read.csv('MCDONALDS.csv',
                  sep=';',
                  dec = ',')
view(dados)

#Transformando a primeira coluna em índice da tabela
row.names(dados) <- dados[,1]
dados <- dados[,-1]
view(dados)

#padronizando os dados
dados.padronizados <- scale(dados)


#calculando a matriz de distancias
#distancia de cada observacao contra todas as outras

distancias <- dist(dados.padronizados,
                   method='euclidean')

#aplicando a tecnica de cluster hierarquico
h1 <- hclust(distancias, method = 'single')

#visualizando o dendograma
plot(h1, cex = 0.6, hang = -1)

#de modo geral, 4 grupos ficam bem definidos
#depende do objetivo do negócio, poderíamos reduzir para 3 ou aumentar para 5 grupos
#Criar o grafico e destacar os grupos
rect.hclust(h1, k = 4)
