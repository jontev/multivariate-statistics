library(MASS)
library(readxl)
library(ggplot2)
library('reshape2')
library(ggExtra)
library(scatterplot3d)
library(mvtnorm) 
library(ellipse) 

#uppgift 8

# a)
path = "data-ha-8.xlsx"
data = read_excel(path)

n1 = 21
n2 = 31
n3 = 22

X1 = t(data[seq(1,n1),])
X2 = t(data[seq(n1+1,n1+n2),])
X3 = t(data[seq(n1+n2+1,n1+n2+n3),])

p = ncol(data)

V1 = X1%*% (diag(n1) - (1/n1)*rep(1,n1)%*%t(rep(1,n1)))%*%t(X1)
V2 = X2%*% (diag(n2) - (1/n2)*rep(1,n2)%*%t(rep(1,n2)))%*%t(X2)
V3 = X3%*% (diag(n3) - (1/n3)*rep(1,n3)%*%t(rep(1,n3)))%*%t(X3)

V = V1 + V2 + V3

n = n1 + n2 + n3
f1 = n1 - 1; f2 = n2 - 1; f3 = n3 - 1;
f = f1 + f2 + f3

lnLambda =  ( (f1/2)*log(det(V1)) + (f2/2)*log(det(V2)) + (f3/2)*log(det(V3)) + (p*f/2)*log(f) ) -
            ( (f/2)*log(det(V)) + (p*f1/2)*log(f1) + (p*f2/2)*log(f2) + (p*f3/2)*log(f3) )

r=3 
r1 = f1/f; r2 = f2/f; r3 = f3/f;
alpha = ( (1/r1) + (1/r2) + (1/r3) - 1 )*(2*(p^2) + 3*p -1)/(12*(p + 1)*(r - 1))
m = f-2*alpha
df = (1/2)*p*(p+1)*(r-1)

z = -2*(1/f)*m*lnLambda

z > qchisq(0.95, df)

# b)


# c)
f = f-f3
x1_bar = rowMeans(X1)
x2_bar = rowMeans(X2)
Sp = (V1 + V2)/(n1+n2-2)

Delta2_hat = ((f-p-1)/f)* t(x1_bar - x2_bar)%*%ginv(Sp)%*%(x1_bar - x2_bar)

a1 = Delta2_hat + 12*(p-1)
a2 = Delta2_hat - 4*(p-1)
a3 = (1/4)*sqrt(Delta2_hat)*(p-1)

e1 = pnorm(-sqrt(Delta2_hat)/2) + dnorm(sqrt(Delta2_hat)/2)*((1/(16*sqrt(Delta2_hat)))*((a1/n1) + (a2/n2)) + (a3/f))

e2 = pnorm(-sqrt(Delta2_hat)/2) + dnorm(sqrt(Delta2_hat)/2)*((1/(16*sqrt(Delta2_hat)))*((a2/n1) + (a1/n2)) + (a3/f))


  


