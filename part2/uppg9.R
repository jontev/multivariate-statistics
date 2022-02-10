library(MASS)
library(readxl)
library(ggplot2)
library('reshape2')
library(ggExtra)
library(scatterplot3d)
library(mvtnorm) 
library(ellipse) 
install.packages("factoextra")
library(factoextra)
#uppgift 9

path = "data-ha-9.xlsx"
data = read_excel(path)

# a)
x_bar = colMeans(data)
S = cov(data)
R = cor(data)

pairs(data)

# b)
#PCA = eigen(R)
#pcomps = PCA$vectors
#pvar = PCA$values

pca = princomp(data, cor=TRUE)
summary(pca, loadings=TRUE)
fviz_eig(pca)
fviz_pca_biplot(pca, repel = TRUE, col.var = "#2E9FDF", col.ind = "#696969")

values = pca$sdev^2
# c)
lambda1_hat = values[1] # vet inte hur man plockar ut ur summaryn...
n = nrow(data)
CI_upper = (lambda1_hat*sqrt(n))/(sqrt(n) - 1.96*sqrt(2))
CI_lower = (lambda1_hat*sqrt(n))/(sqrt(n) + 1.96*sqrt(2))

# d)

# e)



sqrt_lambda = sqrt(values) # eller pca$sdev bara
L = sqrt(diag(diag(R))) # plockar ut sqrt(skk) f??r alla ursprungliga variabler
L = sqrt_lambda*ginv(L)
H = matrix(pca$loadings, nrow=6)
correl = H%*%L
# den f??rsta kolumnen ??r korrelationerna mellan PC1 och variablenra x1,...,x6 ,andra ??r korr mellan PC2 och variablerna osv

fviz_pca_var(pca, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, 
             select.ind=c("Comp.1", "Comp.2"))




