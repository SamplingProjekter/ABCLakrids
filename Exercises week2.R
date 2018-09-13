#-----------------
# Exercise 2.6
#-----------------
library("sampling")
# a)
set.seed(3)
N <- 100
n <- 10

sample1 <- matrix(srswor(n,N),nrow=10,ncol=10)
y1 <- c(6,2,2,2,5,6,5,6,5,4)
tau1 <- N*sum(y1)/n
sd_y1 <- (1-n/N)*sqrt(sum((y1-mean(y1))^2)/(n-1))/n

# b)
# Inclusion prob of upper left: n/N= 10/100=1/10
# Possible samples (n;N), prop of selecting specific sample (n;N)^(-1)

# c)
# repeat a for figure 2.2
set.seed(5)
sample2 <- matrix(srswor(n,N),nrow=10,ncol=10)
y2 <- c(1,5,3,2,1,6,1,1,1,0)
tau2 <- N*sum(y2)/n
sd_y2 <- (1-n/N)*sqrt(sum((y2-mean(y2))^2)/(n-1))/n

# d)
x <- rbind(c(tau1,sd_y1),c(tau2,sd_y2))
colnames(x) <-c("Total","sd") 
# på tegningen ses at der er mere variation i figur 2.2 og som forventet er spredningen større
# The total population mean is onknown but we can calculate an estimate of the mean of y.
c(mean(y1)/sd_y1,mean(y2)/sd_y2)

# e) 
#how to decrease the variance without increasing n?
# decrease N