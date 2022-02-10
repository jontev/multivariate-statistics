library(MASS)
library(readxl)
library(ggplot2)
library('reshape2')
library(ggExtra)
library(scatterplot3d)
library(mvtnorm) 
library(ellipse) 
#assignment 1

#uppgift 2
path = "data-ha-2.xlsx"
data = read_excel(path, col_names=c("x1", "x2"), skip=1)

#a) scatterplot
ggplot(data, aes(x=x1, y=x2)) + geom_point()

# outlier med x1 ??ver 280

#f??r hannar
X_males = data
x_bar_males = colMeans(data)
S_males = cov(data)
n_males=nrow(data)
p=ncol(data)

V_males = (n_males - 1)*S_males

#f??r honor
path = "data-ha-1.xlsx"
data_females = read_excel(path, col_names=c("x1", "x2"), skip=1)
X_females = data_females
x_bar_females = colMeans(data_females)
S_females = cov(data_females)
n_females =nrow(data_females)
p = ncol(data_females)

V_females = (n_females - 1)*S_females



n = n_males + n_females
V = V_males + V_females 

#givet oberoende normalf??rdelade samples anv??nd (vanlig) LRT med Box-korrektion f??r snabbare asymptotik

#numeriskt kass
#Lambda = (det(V_males)^(n_males/2) * det(V_females)^(n_females/2) * n^(p*n/2)) / 
#          (det(V)^(n/2) * n_males^(p*n_males/2) * n_females^(p*n_females/2))
#Logaritmera ist??llet

ln_Lambda = ( ( (n_males/2)*log(det(V_males)) + (n_females/2)*log(det(V_females)) + (p*n/2)*log(n)) -
                ( (n/2)*log(det(V)) + (p*n_males/2)*log(n_males) + (p*n_females/2)*log(n_females)  ) ) 

f_males = n_males -1
f_females = n_females -1
f = f_males + f_females 
r_males = f_males/f
r_females = f_females/f
r=2 

alpha = (1/r_males + 1/r_females - 1)*(2*p^2 + 3*p-1)/(12*(p+1)*(r-1))
m = f - 2*alpha

df = p*(p+1)*(r-1)/2

#box correction
test_stat = -2*(1/f)*m*ln_Lambda

critical_bound = qchisq(0.95,df)

#reject null hypothesis if 
test_stat > critical_bound

#b)

#Testar f??rst likhet mellan kovariansmatriserna

#alternative 1 change observation 31
data[31,1] = 184
S_males = cov(data)
V_males = (n_males - 1)*S_males
V = V_males + V_females 
ln_Lambda = ( ( (n_males/2)*log(det(V_males)) + (n_females/2)*log(det(V_females)) + (p*n/2)*log(n)) -
                ( (n/2)*log(det(V)) + (p*n_males/2)*log(n_males) + (p*n_females/2)*log(n_females)  ) ) 
test_stat = -2*(1/f)*m*ln_Lambda
test_stat > critical_bound

#Proceed with equality of means test
# two samples so use Hotelling T test
Spooled = V/(n_males + n_females -2)
x_bar_males = colMeans(data)
x_bar_females = colMeans(data_females)

T2 = (n_males*n_females)/(n_males + n_females)*t((x_bar_males - x_bar_females))%*%ginv(Spooled)%*%(x_bar_males - x_bar_females)

f = n_males + n_females -2

T2 > ( p*f/(f-p+1)) *qf(0.95,p,f-p+1)

#alternative 2 remove observation 31 (unequal number of sample sizes so use modified test statistic (unbiased))
data = data[-31,]
S_males = cov(data)
n_males = nrow(data)
f_males = n_males -1
f = f_males + f_females
V_males = (n_males - 1)*S_males
V = V_males + V_females 

ln_Lambda = ( ( (f_males/2)*log(det(V_males)) + (f_females/2)*log(det(V_females)) + (p*f/2)*log(f)) -
                ( (f/2)*log(det(V)) + (p*f_males/2)*log(f_males) + (p*f_females/2)*log(f_females)  ) ) 
r_males = f_males/f
r_females = f_females/f

alpha = (1/r_males + 1/r_females - 1)*(2*p^2 + 3*p-1)/(12*(p+1)*(r-1))
m = f - 2*alpha

test_stat = -2*(1/f)*m*ln_Lambda
test_stat > critical_bound


# F test

Spooled = V/(n_males + n_females -2)
x_bar_males = colMeans(data)
x_bar_females = colMeans(data_females)

T2 = (n_males*n_females)/(n_males + n_females)*t((x_bar_males - x_bar_females))%*%ginv(Spooled)%*%(x_bar_males - x_bar_females)

f = n_males + n_females -2

T2 > ( p*f/(f-p+1)) *qf(0.95,p,f-p+1)

# d)
muhat=x_bar_males - x_bar_females
plot((X_males-X_females),   xlab = 'Tail length', ylab = 'Wing length', pch = 8, cex = 0.05)
lines(ellipse(Spooled, centre = (muhat), t = sqrt(qf(0.95,p,f-p+1)*(f*p)/(f-p+1)/((n2*n)/((n+n2)))), col='black', type='l'))


a1 = c(1, 0)
a2 = c(0, 1)
T = sqrt(qf(0.95,p,f-p+1)*(f*p)/(f-p+1))
Imu1 = a1%*%muhat %*% c(1,1) + T*sqrt(t(a1)%*%Spooled%*%a1/((n2*n)/((n+n2)))) %*% c(-1,1)
Imu2 = a2%*%muhat %*% c(1,1) + T*sqrt(t(a2)%*%Spooled%*%a2/((n2*n)/((n+n2)))) %*% c(-1,1)
abline(v=Imu1[1], col='red'); abline(v=Imu1[2], col='red')
abline(h=Imu2[1], col='red'); abline(h=Imu2[2], col='red')
abline(v=0, col='green');
abline(h=0, col='green');