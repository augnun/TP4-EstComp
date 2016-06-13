dados <- read.csv("heart.txt", sep = "", header = FALSE)

padronizar <- function(x){
  return((x - min(x))/(max(x)-min(x)))
}

dados.std <- as.data.frame(lapply(dados[1:13], padronizar))
dados.std <- cbind(dados.std, dados$V14, dados$V14)

set.seed(13)
dados.std.validacao <- head(dados.std, n = 100)
dados.std.treino <- tail(dados.std,n = 170)

names(dados.std) <- c("V1", "V2", "V3", "V4",
                         "V5", "V6", "V7", "V8",
                         "V9", "V10", "V11", "V12",
                         "V13",  "Class", "knn")


names(dados.std.validacao) <- c("V1", "V2", "V3", "V4",
                      "V5", "V6", "V7", "V8",
                      "V9", "V10", "V11", "V12",
                      "V13",  "Class", "knn")

vizinhos <- function(dados.validacao, k){
  n <- nrow(dados.validacao)
  if (n <= k) stop("k não pode ser maior que n - 1")
  mat.vizinhos <- matrix(0, nrow = n, ncol = k)
  for(i in 1:n) {
    dist.euclidiana <- colSums((dados.validacao[i, ] - t(dados.validacao)) ^ 2)  
    mat.vizinhos[i, ] <- order(dist.euclidiana)[2:(k + 1)]
  }
  return(mat.vizinhos)
}


mat.vizinhos <- vizinhos(dados.std.validacao,5)
p <- 0
p2 <- 0

classificador.knn <- function(dados.validacao, mat.vizinhos) {
  for (i in 1:nrow(mat.vizinhos)) {
      p = sum(dados.validacao[mat.vizinhos[i,], 14] == 2)
    if( p > 2){
      dados.validacao[i,15] = 2
    }
    else{
      dados.validacao[i,15] = 1
    }
  }
  return(dados.validacao[,15])
}

dados.std.validacao[,15] <- classificador.knn(dados.std.validacao, mat.vizinhos)
sum(dados.std.validacao[,14] == dados.std.validacao[,15])


####
# Classificador Bayesiano

c_1 <- mean(dados.std.validacao[,14] == 1)
c_2 <- 1 - c_1


prod(mean(which(dados.std.validacao[,14] == 2)))



