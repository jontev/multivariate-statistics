library(MASS)
library(readxl)
library(ggplot2)
library('reshape2')
library(ggExtra)
library(scatterplot3d)
library(mvtnorm) 
library(ellipse) 

#uppgift 10

# a)

path = "data-ha-10.xlsx"
data = read_excel(path, col_names = FALSE)

R = as.matrix(data)


# b)
# Antal egenv??rden av R st??rre ??n 1 = k antalet faktorer att anv??nda

k = sum(eigen(R)$values > 1) # 3 stycken


fa = factanal(factors=k, covmat=R, nobs=210,rotation="varimax")

# c)

L = fa$loadings #Communality coefficients/loadings

psi = diag(fa$uniquenesses) # uniqueness (residuals)

Sigma_hat = L%*%t(L) + psi # skattning av kovariansmatrisen (korr pga stand) givet faktormodellen

n = 210 # individer
p = 15  # variabler

bartlett = -(n-(2*p + 4*k +11)/6) 
g = (1/2)*((p-k)^2 -(p+k))

crit = qchisq(0.95, g)

teststatistic = bartlett*(log(det(R)) - log(det(Sigma_hat)))

teststatistic > crit # om sant f??rkasta att tillr??ckligt m??nga faktorer
#Funkar med tre, f??rkastar med k=2 dock



