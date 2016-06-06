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


