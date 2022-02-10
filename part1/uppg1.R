library(MASS)
library(readxl)
library(ggplot2)
library('reshape2')
library(ggExtra)
library(scatterplot3d)
library(mvtnorm) 
library(ellipse) 
#assignment 1

#uppgift 1
path = "data-ha-1.xlsx"
data = read_excel(path, col_names=c("x1", "x2"), skip=1)
#f??rsta raden inneh??ller "x1", "x2" s?? skip=1 hoppar f??rsta raden

#skattar mu med x-streck
x_bar = colMeans(data)

#skattar kovariansmatrisen S
S = cov(data)
S_inv = ginv(S)
n =nrow(data) # 45 observationer
p = ncol(data) # 2 variabler
mu_0 = c(190, 275)
#teststorhet under H
T2 = n*t((x_bar - mu_0))%*%S_inv%*%(x_bar - mu_0)

# teststorheten F-f??rdelad 
#f??rkasta d?? 
T2 > (((n-1)*p)/(n-p))*qf(0.95,p,n-p)

#b)  
#konfidensellips f??r mu1 o mu2 p?? 95%
X = as.matrix(data)
plot(X, xlab = 'tail length', ylab = 'wing length', pch = 8, cex = 1.5)
lines(ellipse(S, centre = x_bar, t = sqrt((n-1)*p/(n-p)*qf(0.95,p,n-p)/n)), col='black', type='l')


#c)
#simultana 95% T2 intevall f??r mu1 o mu2 och bonferroni, j??mf??r intervallen
a1 = c(1, 0)
a2 = c(0, 1)

# Roy
T = sqrt((n-1)*p/(n-p)*qf(0.95,p,n-p))
Imu1 = a1%*%x_bar %*% c(1,1) + T*sqrt(t(a1)%*%S%*%a1/n) %*% c(-1,1)
Imu2 = a2%*%x_bar %*% c(1,1) + T*sqrt(t(a2)%*%S%*%a2/n) %*% c(-1,1)
abline(v=Imu1[1], col='red'); abline(v=Imu1[2], col='red')
abline(h=Imu2[1], col='red'); abline(h=Imu2[2], col='red')

# t (Bonferroni)
t = qt(1-0.05/4,n-1)
Imu1 = a1%*%x_bar %*% c(1,1) + t*sqrt(t(a1)%*%S%*%a1/n) %*% c(-1,1)
Imu2 = a2%*%x_bar %*% c(1,1) + t*sqrt(t(a2)%*%S%*%a2/n) %*% c(-1,1)
abline(v=Imu1[1], col='blue'); abline(v=Imu1[2], col='blue')
abline(h=Imu2[1], col='blue'); abline(h=Imu2[2], col='blue')
