library(np)
set.seed(777)
n<-200
X1<-runif(n)
X2<-rexp(n)
epsilon<-rnorm(n,0,0.2)
Y<-log(0.6*X1+0.8*X2)+epsilon
X <- cbind(X1, X2)
bw <- npindexbw(xdat=X, ydat=Y, method="ichimura")
summary(bw)
ugrid<-seq(0,3.5,0.01)
ygrid<-log(ugrid)
plot(bw,xlim=c(0,3.5), col = "red")
lines(ugrid,ygrid)
#######
beta<-bw$beta
newX<-X/sqrt(sum(beta^2))
newbw <- npindexbw(xdat=newX, ydat=Y, method="ichimura")
summary(newbw)
plot(newbw,xlim=c(0,3.5), col = "red",lty =2)
lines(ugrid,ygrid,lty =1)
legend("bottomright", c("Estimator","Real line"),
       lty = c(2,1), col = c("red","black"))
###
x<-seq(0,1,0.01)
y<-log(x/(1-x))
plot(x,y,lty=1)
lf<-function(x) log(x/(1-x))
lf(0.06)-lf(0.01)
lf(0.11)-lf(0.06)
lf(0.99)-lf(0.94)
