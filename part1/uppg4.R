library(MASS)
library(readxl)
library(ggplot2)
library('reshape2')
library(ggExtra)
library(scatterplot3d)
library(mvtnorm) 
library(ellipse) 
#assignment 1

#uppgift 4
path = "data-ha-4.xlsx"
data = read_excel(path, col_names=FALSE)
X = as.matrix(data)
p = ncol(X)
Control = X[1:6,]
Group1 = X[7:20,]#radiation level 37.5
Group2 = X[21:35,]#radiation level 85.7 
Group3 = X[36:45,]# radiation level 187.5

muhatControl = colMeans(Control)
muhat1 = colMeans(Group1)
muhat2 = colMeans(Group2)
muhat3= colMeans(Group3)

means = as.data.frame(cbind(muhatControl, muhat1, muhat2, muhat3))
means$days = seq(1,3)
melted =  melt(means,id=c("days"))

ggplot(data=melted, aes(x=days, y=value, group=variable , color=variable
)) + geom_line() 


# a) parallellism
X = t(data)
n1 = 6; n2 = 14; n3 = 15; n4 = 10;
a1 = append(rep(1,n1), rep(0, n2+n3+n4))
a2 = append(rep(0,n1) , append( rep(1,n2), rep(0, n3+n4)))
a3 = append(rep(0,n1+n2) , append( rep(1,n3), rep(0, n4)))
a4 = append(rep(0,n1+n2+n3), rep(1,n4))
At = cbind(a1,a2,a3,a4)
A = t(At)

n=n1+n2+n3+n4

V = X%*%(diag(n) - t(A)%*%ginv(A%*%t(A))%*%A )%*%t(X)

group1 = X[, seq(1,n1)]
group2 = X[, seq(n1+1, n2 + n1 )]
group3 = X[, seq(n1+n2+1, n1+n2+n3)]
group4 = X[, seq(n1+n2+n3+1, n)]
x1_bar = rowMeans(group1)
x2_bar = rowMeans(group2)
x3_bar = rowMeans(group3)
x4_bar = rowMeans(group4)

x_bar = rowMeans(X)

Y = cbind(x1_bar-x4_bar, x2_bar-x4_bar, x3_bar-x4_bar)

nk_1 = c(n1,n2,n3)
Xi_inv = diag(c(n1,n2,n3)) - (1/n)*nk_1%*%t(nk_1)

H = Y%*%Xi_inv%*%t(Y)
C = matrix(c(1,0, -1,1,0,-1), nrow=2, ncol=3)

Lambda_H1 = det(C%*%V%*%t(C))/ det( C%*%V%*%t(C) + C%*%H%*%t(C))
k = 4 
test_stat = -(n- (1/2)*(k+p+1))*log(Lambda_H1)
test_stat > qchisq(0.95,(p-1)*(k-1))

#testar likhet
Lambda_H2 = (det( C%*%V%*%t(C) + C%*%H%*%t(C))* det(V) ) / ( det(C%*%V%*%t(C))*det(V+H))
F_H2 = (1-Lambda_H2)/(Lambda_H2)

critical_bound = (1/( (n-k-p+1)/(k-1)) )*qf(0.95, k-1, n-k-p+1)

F_H2 > critical_bound

# testar flatnsess

test_stat = n*t(x_bar)%*%t(C)%*% ginv((C%*%V%*%t(C) + C%*%H%*%t(C))) %*%C%*%x_bar 

test_stat > ((p-1)/(n-p+1))*qf(0.95, p-1, n-p+1)