library(MASS)
library(readxl)
library(ggplot2)
library('reshape2')
library(ggExtra)
library(scatterplot3d)
library(mvtnorm) 
library(ellipse) 
#assignment 1

#uppgift 7
path = "data-ha-7.xlsx"
data = read_excel(path, col_names=c("Before", seq(1,10)), skip=1)
group1 = data[seq(1,6),]
group2 = data[seq(7,20),]
group3 = data[seq(21,35),]
group4 = data[seq(36,45),]
# a) one way anova
mu1 = colMeans(group1)[1]
mu2 = colMeans(group2)[2]
mu3 = colMeans(group3)[3]
mu4 = colMeans(group4)[4]
mu = colMeans(data[,1])
n1 = nrow(group1); n2 =nrow(group2); n3= nrow(group3); n4 = nrow(group4); 
SST = n1*(mu1-mu)^2 + n2*(mu2-mu)^2 + n3*(mu3-mu)^2 + n4*(mu4-mu)^2

s1 = (var(group1[,1]))
s2 = (var(group2[,1]))
s3 = (var(group3[,1]))
s4 = (var(group4[,1]))
SSE = (n1-1)*s1 + (n2-1)*s2 + (n3-1)*s3 + (n4-1)*s4 
k=4
n = n1+n2 + n3 +n4
F = (SST/(k-1))/(SSE/(n-k))
F > qf(0.95, k-1, n-k)

# b)
#Bilda designmatriserna
a1 = rep(1,11)
a2 = seq(0,10)
a3 = seq(0,10)^2

A = cbind(a1,a2,a3)

n1 = nrow(group1); n2=nrow(group2);n3=nrow(group3);n4=nrow(group4)
c1 = append(rep(1,n1), rep(0, n2+n3+n4))
c2 = append(rep(0,n1) , append( rep(1,n2), rep(0, n3+n4)))
c3 = append(rep(0,n1+n2) , append( rep(1,n3), rep(0, n4)))
c4 = append(rep(0,n1+n2+n3), rep(1,n4))
C_transpose = cbind(c1,c2,c3,c4)
C = t(C_transpose)

Y = t(as.matrix(data))

V = Y%*%(diag(45) - t(C)%*%ginv(C%*%t(C))%*%C)%*%t(Y)
V_inv = ginv(V)
Beta_hat = ginv( t(A)%*%V_inv%*%A)%*%t(A)%*%V_inv%*%Y%*%t(C)%*%ginv(C%*%t(C))

#Plotta varje kolumn i Y, v??ntev??rde f??r varje grupp och de estimerade tillv??xtkurvorna

means1 = colMeans(group1)
means2 = colMeans(group2)
means3 = colMeans(group3)
means4 = colMeans(group4)
group_means = cbind(means1, means2, means3, means4)
#plotting

#  fitted growth curves
df_curves = as.data.frame(A%*%Beta_hat)
colnames(df_curves) = c(c("group1", "group2", "group3", "group4"))
df_curves$time = seq(0,10)

df = df_curves
#Means for each group
df_means =as.data.frame(group_means)

df[, seq(6,9)] = group_means
names(df)[seq(6,9)] = c("mean group1", "mean group2", "mean group3", "mean group4")
#varje kolumn i Y (varje observation)
df_obs = as.data.frame(Y)
df_obs$time  = seq(0,10)

melted_df =  melt(df,id=c("time"))
melted_df_obs = melt(df_obs, id=c("time"))

ggplot(data=melted_df, aes(x=time, y=value, group=variable 
                           , color=variable)) + geom_line() + 
  geom_line(data=melted_df_obs, aes(x=time, y=value, color="blue")  )


ggplot(data=melted_df_obs, aes(x=time, y=value, group=variable
                           ), color="black") + geom_line() + 
  geom_line(data=melted_df, aes(x=time, y=value, group=variable, color =variable
), size=1.3  )  + scale_color_manual(values=c("red", "blue", "green", "orange", "purple", "brown", "pink", "cyan"))

# grupp 1 f??ljer inte s??rskilt bra
# grupp 2 f??ljer allt bra
# grupp 3 har n??n outlier som g??r att mean kurvan avviker fr??n tillvx??tkurvan mitt i
# grupp 4 har v??ntev??rden som ??r konsistent st??rre ??n tillv??xtkurvan

# c)
# Testar GBH = GB = 0 (H enhetsmatrisen)
G = matrix(c(0,0,1), nrow=1)

L = ginv(t(A)%*%ginv(V)%*%A)
K = ginv(C%*%t(C))
R = K + K%*%C%*%t(Y)%*%(ginv(V) - ginv(V)%*%A%*%L%*%t(A)%*%ginv(V))%*%Y%*%t(C)%*%K


Lambda = det(G%*%L%*%t(G))/ det( G%*%L%*%t(G) + G%*%Beta_hat%*%ginv(R)%*%t(Beta_hat)%*%t(G) )

#Box corrected
n =ncol(Y)
p = nrow(Y)
k = dim(Beta_hat)[2]
q = dim(Beta_hat)[1]
r = dim(G)[1]
t = k
z = -(n-k+q -p-(1/2)*(r-t+1))*log(Lambda)
#reject H if
z > qchisq(0.99, r*t)
# kan f??rkasta att koefficienterna framf??r de kvadratiska termerna ??r noll, dvs linj??r modell ??r inte garanterat b??ttre
