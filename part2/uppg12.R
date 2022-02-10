library(MASS)
library(readxl)
library(ggplot2)
library('reshape2')
library(ggExtra)
library(scatterplot3d)
library(mvtnorm) 
library(ellipse) 
library(expm)
#uppgift 12

path = "data-ha-12.xlsx"
data = read_excel(path)
X = scale(data) # standardiserar
n = nrow(X)

# a)
X1 = X[,c(1,2,3)]
X2 = X[,c(4,5,6)]
R = cov(X)
pairs(X)

# b)

cca = cancor(X1, X2)
cca$cor
round(cca$xcoef[,1],4)
round(cca$ycoef[,1],4)

R11 = R[seq(1,3), seq(1,3)]
R12 = R[seq(1,3), seq(4,6)]
R21 = R[seq(4,6), seq(1,3)]
R22 = R[seq(4,6), seq(4,6)]

# om explicit som egenv??rdesproblem ??r koefficienterna f??r X1 egenvektorerna till
# ginv(R11)%*%R12%*%ginv(R22)%*%R21

A = ginv(sqrtm(R11))%*%R12%*%ginv(sqrtm(R22))

#f??r grupp 1, alpha
eig1 = eigen(A%*%t(A))
cors = sqrt(eig1$values)
xcoefs = ginv(sqrtm(R11))%*%eig1$vectors

# Dessa xcoefs ??r ej samma som cancor-funktionen ger, men korrelationerna ??r samma. Varf??r?


 # c)
p=ncol(X1); q=ncol(X2);

alpha = 0.95;

rho2 = cca$cor^2


for (k in 0:2)
{
  df = (p-k)*(q-k)
  crit = qchisq(alpha, df)
  lnLambda = sum(log(1-rho2[(k+1):p])) 
  #print(lnLambda)
  m = -(n-k-(1/2)*(p+q+3) - sum(rho2[1:(k+1)]))
  result = m*lnLambda > crit
  cat('\n', 'Test f??r k=', k, ' gav ', result, ' med teststorhet', m*lnLambda ,' och gr??ns' ,crit)
}
