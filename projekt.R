# Read data
Dat <- as.matrix(read.table("Data.R", sep="",head=TRUE,fill = TRUE))
#Formate data as matrix
Dat_new <- na.omit(matrix(as.numeric(Dat[,-1]),ncol=4,nrow=29))
rownames(Dat_new) <- Dat[-c(28,29),1]

# Sample size
n <- colSums(Dat_new)
n_mean <- sum(Dat_new)/4

# Plot data
mean <- apply(Dat_new,1,mean)
df <- barplot(t(Dat_new),beside=TRUE,col = rainbow(4))
lines(df,rep(mean,each=4),lty=2,lwd=1)
lines(df,rep(mean(mean),each=length(df)),lty=2,col="red")
legend("topleft",c("L. Mean","T. Mean"),lty = 2,col=c("black","red"))


# X vokal vs. konsonant 
id <- ifelse(rownames(Dat_new)%in% c("A","E","I",
                                    "O","U","Y",
                                    "AE"), TRUE,FALSE)
# y_bar
y_bar <- colSums(Dat_new[id,])/n

#Population total if distiributed according to aplhabet
y_v9 <- 9/29 #vowels in danish alphabet
y_v7 <- 7/27 #expected vowels based on what we sample 

# Empirical variance
var_emp <- (1-0)*y_bar*(1-y_bar)/(n-1)
sqtr(var_emp) #estimate the standard deviation 

# True variance
var_t_v9 <- 29/28*y_v9*(1-y_v9)
var_t_v7 <- 27/26*y_v7*(1-y_v7)
c(var_t_v9,var_t_v7)

# 95% -CI for the empirical variance
uu <- qnorm(.975)
CI <- cbind(y_bar-uu*sqrt(var_emp),y_bar+uu*sqrt(var_emp))
# both y_v9 an y_v7 are in the CI.


#Hyppige bogstaver i noveller
id2 <- ifelse(rownames(Dat_new)%in% c("E","N","D","R"), 
                 TRUE,FALSE)
y_endr <- colSums(Dat_new[id2,])/n
var_endr <- y_endr*(1-y_endr)/(n-1)

CI <- cbind(y_endr-uu*sqrt(var_endr),y_endr+uu*sqrt(var_endr))

# value for alphabet 
v_29 <- 4/29
v_27 <- 4/27
c(v_29,v_27)

# again we cannit reject that the letters occure as in the alphabet. 
