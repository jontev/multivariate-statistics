library(MASS)
library(readxl)
library(ggplot2)
library('reshape2')
library(ggExtra)
library(scatterplot3d)
library(mvtnorm) 
library(ellipse) 

#uppgift 11

# a)

R11 = matrix(c(1, 0.615, 0.615, 1),ncol=2)

R12 = matrix(c(-0.111, -0.195,-0.266, -0.085),nrow=2)

R21 = t(R12)

R22 = matrix(c(1, -0.269, -0.269,1), ncol=2)

M = ginv(R11)%*%R12%*%ginv(R22)%*%R21

eig = eigen(M)

#correlations
sqrt(eig$values)
# coeffs
eig$vectors

# P?? samma s??tt f??r b, koefficienterna i linj??rkombinationerna av grupp 2
N = ginv(R22)%*%R21%*%ginv(R11)%*%R12

eigN = eigen(N)
sqrt(eigN$values)
eigN$vectors
