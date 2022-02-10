library(MASS)
library(readxl)
library(ggplot2)
library('reshape2')
library(ggExtra)
library(scatterplot3d)
library(mvtnorm) 
library(ellipse) 
#assignment 1

#uppgift 5
path = "data-ha-5.xlsx"
data = read_excel(path, col_names = TRUE)
pairs(data)
# b)
x_bar = colMeans(data)
S = cov(data)
R = cor(data)

# c)
y = cbind(data$NO2)
n = nrow(y)
X = cbind(rep(1,n),data$Wind, data$SR)
Beta_hat = ginv(t(X)%*%X)%*%t(X)%*%y
y_hat = X%*%Beta_hat
E_hat = y-y_hat

hist(E_hat)
qqnorm(E_hat)
qqline(E_hat)


# testar med linj??ra modellen log(y1) ~ [1 x1 x2]B ist??llet efteersom residualerna
# inte s??g s??rskilt normalf??rdelade ut

y = log(y)
Beta_hat = ginv(t(X)%*%X)%*%t(X)%*%y
y_hat = X%*%Beta_hat
E_hat = y-y_hat
hist(E_hat)
qqnorm(E_hat)
qqline(E_hat)

# plottar b??da modellerna, med o utan logtransformation
x1 = X[,2]
x2 = X[,3]
lm = lm(y ~ x1 + x2)
summary(lm)
s3d = scatterplot3d(x1, x2, y, pch = 19, type = "p",
                    color = "darkgrey",main = "Regression Plane",
                    grid = TRUE, box = FALSE, mar = c(2.5, 2.5, 2, 1.5), angle = 7)
s3d$plane3d(lm, draw_polygon = TRUE, draw_lines = TRUE,
            polygon_args = list(col = rgb(.1, .2, .7, .5)))
wh = resid(lm) > 0
s3d$points3d(x1[wh], x2[wh], y[wh], pch = 19)


lm = lm(log(y) ~ x1 + x2)
summary(lm)
s3d = scatterplot3d(x1, x2, log(y), pch = 19, type = "p",
                    color = "darkgrey",main = "Regression Plane",
                    grid = TRUE, box = FALSE, mar = c(2.5, 2.5, 2, 1.5), angle = 7)
s3d$plane3d(lm, draw_polygon = TRUE, draw_lines = TRUE,
            polygon_args = list(col = rgb(.1, .2, .7, .5)))
wh = resid(lm) > 0
s3d$points3d(x1[wh], x2[wh], log(y)[wh], pch = 19)

# prediktionsintervall f??r x0 out of sample obseration
x0 = matrix(c(1, 10, 80), ncol=1)
y = cbind(data$NO2)
n = nrow(y)
X = cbind(rep(1,n),data$Wind, data$SR)
Beta_hat = ginv(t(X)%*%X)%*%t(X)%*%y
r = 3 # oberoende kolumnvektorer i X
s2= t(y)%*%(diag(n) -X%*%ginv(t(X)%*%X)%*%t(X))%*%y / (n-r)
s = sqrt(s2)
y0_hat = t(x0)%*%Beta_hat
t = qt(0.975, n-r-1)

PI = c(y0_hat - s*t*sqrt(1+ t(x0)%*%ginv(t(X)%*%X)%*%x0), y0_hat + s*t*sqrt(1+ t(x0)%*%ginv(t(X)%*%X)%*%x0))


# d)
Y = cbind(data$NO2, data$O3)
n = nrow(Y)
X = cbind(rep(1,n),data$Wind, data$SR)
Beta_hat = ginv(t(X)%*%X)%*%t(X)%*%Y

Y_hat = X%*%Beta_hat
E_hat = Y - Y_hat
E_hat = as.data.frame(E_hat)
colnames(E_hat) = c("e1", "e2")
ggplot(E_hat, aes(x=e1, y=e2)) + geom_point()
hist(E_hat$e1)
hist(E_hat$e2)
qqnorm(E_hat$e1); qqline(E_hat$e1)
qqnorm(E_hat$e2); qqline(E_hat$e2)
# inte uppenbart normalf??rdelat


# Testar [log(y1) log(y2)] ~ XB ist??llet
Y = log(cbind(data$NO2, data$O3))
X = cbind(rep(1,n),data$Wind, data$SR)
Beta_hat = ginv(t(X)%*%X)%*%t(X)%*%Y

Y_hat = X%*%Beta_hat
E_hat = Y - Y_hat
E_hat = as.data.frame(E_hat)
colnames(E_hat) = c("e1", "e2")
ggplot(E_hat, aes(x=e1, y=e2)) + geom_point()
hist(E_hat$e1)
hist(E_hat$e2)
qqnorm(E_hat$e1); qqline(E_hat$e1)
qqnorm(E_hat$e2); qqline(E_hat$e2)


# prediktionsellips f??r 
x0 = matrix(c(1, 10, 80), ncol=1)
# med de linj??ra responsvariablerna

Y = cbind(data$NO2, data$O3)
Beta_hat = ginv(t(X)%*%X)%*%t(X)%*%Y

p=2
r = 3 # oberoende kolumnvektorer i X
n = nrow(Y)
S = t(Y)%*%(diag(42) - X%*%ginv(t(X)%*%X)%*%t(X))%*%Y/(n-r-1)
Y0_hat = t(x0)%*%Beta_hat

td = sqrt( (1+t(x0)%*%ginv(t(X)%*%X)%*%x0)*(p*(n-r-1)/(n-r-p))*qf(0.95, p,n-r-p) )

plot(ellipse(S, centre = as.vector(Y0_hat), t =td, col='black', type='l'))

lines(ellipse(S, centre = as.vector(Y0_hat), t =td, col='black', type='l'))
points(Y, xlab = 'Wind', ylab = 'SR', pch = 8, cex = 1.5)


