library(rstatix)
#____________________________________________________
# EX  Transparent 
x1  <- c(35,35,40,10,6,20,35,35,35,30)
x2  <- c(3.5,4.9,3,2.8,2.7,2.8,4.6,10.9,8,1.6)
x3  <- c(2.8,2.7,4.38,3.21,2.73,2.81,2.88,2.9,3.28,3.2)
fac <-c(1,1,1,1,1,2,2,2,2,2)
df  <- data.frame(x1,x2,x3,fac)

#---1. Transformation de la variable fac en facteur
#-> variable factorielle
df$fac <- as.factor(df$fac) 

#-> nb de facteur (= 2)
k <- nlevels(df$fac)

#-> nb de variables
n_var <- ncol(df) - 1

#-> Nombre d'observations 
N <- nrow(df)

df_data = data.frame(x1,x2,x3)
test = colMeans(df_data)
test

df_cov <- cov(df_data)
df_cov

df_g1 <- df_data[1:5,1:3]
colMeans(df_g1)

df_g2 <- df_data[6:10,1:3]
colMeans(df_g2)

G1 <- list(data = df_g1,n = nrow(df_g1) , mean = colMeans(df_g1), S2 = cov(df_g1))
G1

G2 <- list(data = df_g2,n= nrow(df_g2), mean = colMeans(df_g2), S2 = cov(df_g2))
G2

L <- list(G1=G1, G2=G2)


#####3.2 Construction du test
####3.2.1 Estimation de la variance commune

### Test M de Box
test <- box_m(df[,1:3],df$fac)
test

Upper_S_d2_P1 <- (G1$n -1)*(G1$S2^2)
Upper_S_d2_P1

Upper_S_d2_P2 <- (G2$n -1)*(G2$S2^2)
Upper_S_d2_P2

Under_S_d2 <- G1$n + G2$n -2

S_d2 <- (Upper_S_d2_P1+Upper_S_d2_P2)/Under_S_d2
S_d2

S_d= sqrt(S_d2)

MatMoy <- data.matrix(G1$mean - G2$mean)
MatMoy

Midd_T2 <- 1/(((1/G1$n)+(1/G2$n))*S_d)
Midd_T2

T_2 <- t(MatMoy) %*% (Midd_T2) %*% MatMoy
T_2

