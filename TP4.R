dados <- read.csv("heart.txt", sep = "", header = FALSE)

padronizar <- function(x, tipo = "minmax"){
  if(tipo == "minmax"){  return((x - min(x))/(max(x)-min(x)))}
  if(tipo == "escore-z"){ return((x - mean(x))/sd(x))}
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

c_1 <- mean(dados.std.validacao[,14] == 1) #0.57
c_2 <- 1 - c_1 #0.63

dados.std.validacao.c1 <- dados.std.validacao[which(dados.std.validacao$Class == 1),]
dados.std.validacao.c2 <- dados.std.validacao[which(dados.std.validacao$Class == 2),]

dados.std.treino$NB = numeric(length = 170)

prod = numeric()
i = 1
j = 1
# prob1 = vector()
# Classe 1
for(i in 1:170){
  for(j in 1:15){
    prod = mean(dados.std.treino[i,j] == dados.std.validacao.c1[,j])
    prob1 <- append(prob1, prod)
  
    
    prod = mean(dados.std.treino[i,j] == dados.std.validacao.c2[,j])
    prob2 = append(prob2,prod)
    
    prob1 <- prod(prob1[which(prob1 != 0)])
    prob2 <- prod(prob2[which(prob2 != 0)])
    
    if(prob1 > prob2){dados.std.treino[i,16] = 1}
    else(dados.std.treino[i,16] = 2)
           
  }
}



# K-means clustering

