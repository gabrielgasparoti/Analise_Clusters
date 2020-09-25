#############################

#ANALISA DE CLUSTER EM R

#TECNICA NAO-HIERARQUICA KMEANS

#AGRUPAMENTO DE MUNICÍPIOS

############################

#Carregando as bibliotecas necessárias

#manipulação de dados
library(tidyverse)

#tecnica de cluster
library(cluster)

#Visualização da quantidade ideal de cluster (elbow method)
library(factoextra) 

#Gráficos de dispersão de dados
library(gridExtra)


#Carregando os dados em arquivo csv
dados <- read.csv('municipios.csv',
                  sep = ';',
                  dec = ',')
view(dados)

#Retirando Variáveis que não são importantes para o agrupamento
#O objetivo é agrupar com base nos atributos direcionados ao desenvolvimento econômico
#portanto as variáveis que tratam da idade das pessoas serão retiradas

dados$Pop_Menos_15Anos_2014 <-NULL
dados$Pop_com_60Anoso_2014 <-NULL
dados$taxa_natalidade <- NULL
dados$Esgoto_2010<-NULL
view(dados)

#Transformando a primeira coluna em índice da tabela (código dos clientes)
row.names(dados) <- dados[,1]
dados <- dados[,-1]
view(dados)


#padronizando os dados
#trazendo as variáveis para uma mesma escala
dados.padronizado <- scale(dados)


#Aplicando o metodo ELBOW para definir a quantidade ideal de cluster
fviz_nbclust(dados.padronizado, 
             kmeans,
             method = "wss")

#Concluimos que 3 cluster são ideiais

#Agora vamos aplicar a tecnica kmeans com 3 centros e formar 3 grupos
dados_3 <- kmeans(dados.padronizado, 
                  centers = 3)


#Graficos
#visualização da dispersão dos dados nos 3 grupos
G1 <- fviz_cluster(dados_3, 
                   geom = "point", 
                   data = dados.padronizado) + ggtitle("k = 3"); G1


#PARA EXPLORAR OS DADOS, JUNTANDO TABELAS:
dados2 <- data.frame(dados_3$cluster)
dados <-  cbind(dados, dados2)
view(dados)

#A cidade de são paulo ficou isolada em um único cluster, uma vez que está muito distante da maioria
#em relação aos outros dois grupos, cada cidade dentro do cluster tem "scores" parecidos entre si e diferentes do outro cluster