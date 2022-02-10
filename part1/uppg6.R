library(MASS)
library(readxl)
library(ggplot2)
library('reshape2')
library(ggExtra)
library(scatterplot3d)
library(mvtnorm) 
library(ellipse) 
#assignment 1

#uppgift 6

path = "data-ha-6.xlsx"
data = read_excel(path, col_names = c("y1", "y2", "y3", "y4", "y5", "x1", "x2"), skip=1)
X = cbind(rep(1,nrow(data) ),data$x1, data$x2)
Y = cbind(data$y1, data$y2, data$y3, data$y4, data$y5)

Beta_hat = ginv( t(X)%*%X)%*%t(X)%*%Y

Y_hat = X%*%Beta_hat

E_hat = Y - Y_hat

#uppgift b)
#stickprovskorrelation mellan variabler
R = cor(data)

#uppgift c)
# testar om koeff framf??r x2 ??r noll f??r alla responsvariabler (death rates)

m = 1; q = dim(X)[2]; p = dim(Y)[2]; n= dim(Y)[1]
C = matrix(c(0,0,1), nrow=m, ncol=q)

V = t(Y)%*%(diag(1,n) - X%*%ginv(t(X)%*%X)%*%t(X))%*%Y
W = t(Beta_hat)%*%t(C)%*% ginv(C%*% ginv(t(X)%*%X) %*%t(C)) %*%C%*%Beta_hat

Lambda = det(V)/(det(V+W))

z = -(n-q-0.5*(p-m+1))*log(Lambda)
f = p*m
gamma_2 = p*m*(p^2  + m^2 -5)/48
nu = n - (p-m+1)/2

p_value = pchisq(z, f, lower.tail = FALSE) + 
  (gamma_2/(nu^2))*(pchisq(z, f+4, lower.tail = FALSE) - pchisq(z, f, lower.tail = FALSE))

# kan f??rkasta att koefficienterna framf??r x2 ??r noll f??r responsvariablerna
