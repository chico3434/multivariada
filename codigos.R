R <- matrix(c(1,0.02,0.96,0.42,0.01,0.02,1,0.13,0.71,0.85,0.96,0.13,1,0.5,0.11,0.42,0.71,0.5,1,0.79,0.01,0.85,0.11,0.79,1), ncol=5)

# 1 - Sim, F1 = 2,4,5; F2=1,3
# 2 - 2
# 3 - 
av <- eigen(R)
L <- sqrt(av$values)[1] %*% av$vectors[,1] %>% cbind(sqrt(av$values)[2] %*% av$vectors[,2])
L <- matrix(L, ncol = 2)
L
h2 <- matrix(L[1:5,1]^2+L[1:5,2]^2,ncol=1)
h2
psi <- 1-h2
psi
L%*%t(L)
R-L%*%t(L)
princomp(R)$loading
L%*%t(L)
wine <- read.csv("C:/Users/chico/Downloads/wine.data", header=FALSE)
dados <- wine[,-1]
cor(dados)
eigen(cor(dados))

sqrt(av$values)[1] %*% av$vectors[,1]

pc <- princomp(dados)
pc$loadings

pr <- princomp(R)
pr$loadings

sqrt(av$values)[1]*av$vectors[4,1]

eigen(matrix(c(1,2,2,1),ncol=2))
