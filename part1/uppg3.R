library(MASS)
library(readxl)
library(ggplot2)
library('reshape2')
library(ggExtra)
library(scatterplot3d)
library(mvtnorm) 
library(ellipse) 
#assignment 1

# uppgift 3
n = 6
Sigma = matrix(c(2,-1,-1,2),nrow = 2)
x_bar = c(1, 1/2)

# a)
mu0 = c(2, 2/3)
z = n*t((x_bar-mu0))%*%ginv(Sigma)%*%(x_bar -mu0)
z > qchisq(0.95,2)

# b)
a = c(1,1)
z = (t(a)%*%x_bar -(7/2))/(sqrt(t(a)%*%Sigma%*%a/n))
abs(z) > qnorm(0.975)

# c)
a = c(1,-1)
z = (t(a)%*%x_bar -(1/2))/(sqrt(t(a)%*%Sigma%*%a/n))
abs(z) > qnorm(0.975)

# d)
a = c(1,0)
z = (t(a)%*%x_bar -2)/(sqrt(t(a)%*%Sigma%*%a/n))
abs(z) > qnorm(0.975)
