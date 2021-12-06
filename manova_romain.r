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

print(df)

moyenne = mean(df[x1])
