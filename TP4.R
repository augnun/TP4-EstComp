dados <- read.csv("heart.txt", sep="")


padronizar <- function(x){
  return((x - min(x))/(max(x)-min(x)))
}
dados.std <- NULL

for(i in 1:length(dados)){
  dados.std[i] <- padronizar(dados[i])
}

dados.std <- data.frame(dados.std)
colnames(dados.std) <- colnames(dados)


set.seed(13)
dados.teste <- dados.std[sample.int(nrow(dados.std), 100),]

distancia.mat <- data.frame(nrow = length(dados.teste),
                        ncol = length(dados.teste))
distancia <- function(x){
  for(i in 1:length(x)){
    for(j in 1:length(x)){
      distancia.mat[i,j] <- sum(sqrt((x[i] - x[j])^2))
      
      
    }
  }
  return(distancia.mat)
}
