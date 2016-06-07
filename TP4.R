dados <- read.csv("heart.txt", sep="")

str(dados)
padronizar <- function(x){
  return((x - min(x))/(max(x)-min(x)))
}
dados.std <- NULL

dados.std <- as.data.frame(lapply(dados, padronizar)) #exclui a col de categorias

dados.std <- data.frame(dados.std)
colnames(dados.std) <- colnames(dados)


set.seed(13)
dados.std.teste <- dados.std[sample.int(nrow(dados.std), 100),]

vizinhanca <- lapply(distancias.matriz, sort.list)






























dados <- read.csv("heart.txt", sep="", header=FALSE)

padronizar <- function(x){
  return((x - min(x))/(max(x)-min(x)))
}
dados.std <- NULL

dados.std <- as.data.frame(lapply(dados, padronizar))
# NOTAR QUE PADRONIZAMOS INCLUSIVE A CATEGORIA 
# COLUNA 14, QUE ERA 1-2
# E PASSOU A SER 0-1


dados.std <- data.frame(dados.std)
colnames(dados.std) <- colnames(dados)

set.seed(13)
dados.std.validacao <- head(dados.std, n = 100)
dados.std.treino <- tail(dados.std,n = 170)

knn.ingenuo <- function(treino, validacao, k = 5){
  for(i in 1:length(validacao)){
    mat.distancias <- dist(rbind(validacao[i,], treino))
    minimo <- which(mat.distancias == min(mat.distancias), arr.ind = TRUE)
  }
}
