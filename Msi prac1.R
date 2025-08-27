#Q1)
install.packages("prevtoinc")
library(stats4)
library(methods)
set.seed(8421)
x1 <- 0 : 10
n1 <- 10
p1 <- 0.5
#therotical pmf pf bin(n,p)
px1 <- dbinom(x1,n1,p1)
px1
#generate random samples

rv1 <- rbinom(20,n1,p1)
rv11 <- rbinom(500,n1,p1)

library(prevtoinc)
emppmf1 <- epmf(rv1)
emppmf11 <- epmf(rv11)
par(mfrow = c(1,3))
plot(x1,px1,type = "h",lwd = 2, main = paste("Therotical Distribution \n ( n = ",n1,",p = ",p1," "))
points(x1,px1,pch = 66)
plot(emppmf1,type = "h", lwd = 2, main = "Emperical pmf for sample of 20")
points(emppmf1,pch = 76)
plot(emppmf11,type = "h", lwd = 2, main = "Emperical pmf for sample of 500")
points(emppmf11,pch = 76)
# we observe that as sample size increases therotical and empirical distributions become similar
cx <- pbinom(x1,n1,p1)
empcdf1 <- ecdf(rv1)
empcdf1
empcdf11<- ecdf(rv11)
par(mfrow = c(1,1))
plot(x1,cx,type = "s", col = "purple",lwd = 2)
lines(empcdf1,col = "blue")
lines(empcdf11,col = "green")
legend("topleft", lty = 1,col = c("purple", "blue", "green"),c("Therotical","n = 20", "n = 500") )
