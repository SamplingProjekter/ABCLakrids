# Read data
Dat <- as.matrix(read.table("Data.R", sep="",head=TRUE,fill = TRUE))
#Formate data as matrix
Dat_new <- matrix(as.numeric(Dat[,-1]),ncol=4,nrow=29)
rownames(Dat_new) <- Dat[,1]
n_est <- sum(Dat_new,na.rm = TRUE)/4

#calculate mean
mean <- apply(Dat_new,1,mean)
# Plot data
df <- barplot(t(na.omit(Dat_new)),beside=TRUE,col = rainbow(4))
lines(df,rep(na.omit(mean),each=4),lty=2,lwd=1)
lines(df,rep(mean(na.omit(mean)),each=length(df)),lty=2,col="red")
legend("topleft",c("L. Mean","T. Mean"),lty = 2,col=c("black","red"))


#X vokal vs. konsonant 
z <- ifelse(rownames(Dat_new)%in% c("A","E","I",
                                    "O","U","Y",
                                    "AE"), TRUE,FALSE)
y_bar <- sum(Dat_new[z])/(n_est*4)
y_bar

y_bar1 <- sum(Dat_new[z,1])/sum(Dat_new[,1],na.rm = TRUE)
y_bar2 <- sum(Dat_new[z,2])/sum(Dat_new[,2],na.rm = TRUE)
y_bar3 <- sum(Dat_new[z,3])/sum(Dat_new[,3],na.rm = TRUE)
y_bar4 <- sum(Dat_new[z,4])/sum(Dat_new[,4],na.rm = TRUE)

y_barP <- c(y_bar1,y_bar2,y_bar3,y_bar4)

v9 <- 9/29 #vowels in danish alphabet
v7 <- 7/27 #expected vowels based on what we sample 


# with variance estimator
var_emp <- (1-0)*y_bar*(1-y_bar)/n_est
var_emp
var_p <- y_barP*(1-y_barP)/colSums(Dat_new,na.rm=TRUE)

# with variariance based on "true" alphabet
var_t_v9 <- 1*v9*(1-v9)/n_est
var_t_v7 <- v7*(1-v7)/n_est
# which is very close to the variation we see in the alphabeth. 


# Total number of letters pr. pag
total_b <- rowSums(Dat_new/4)
total_ABC <- sum(total_b,na.rm=TRUE)

variation <- var(t(na.omit(Dat_new)))
sd <- sqrt(diag(variation))
round(sd,2)
#for each letter we expect a variation between 1 and 3 peaces within a bag
c(max(sd),min(sd)) 
#with the variation sortet as follows - note we have omittet letters not observed
names(sort(sd,decreasing=TRUE)) #ovious we have the least variation in M and W.

# Calculate coefficients of error expressed in percentage
round(sd/na.omit(mean)*100,2)

#If I want to be able to spell my name I would want to know how many bags of candy I needed to 
# by to be quite sure all letters where their. 
# Assume total populationsize is total number of letters + Assume independens
# Is it more likely to see to different letters then to of the same, 
#(is Simon more happy then Nanna?)

# Expected inclusion probabilities based on samples
pii <- (rowSums(Dat_new))/(sum(Dat_new,na.rm=TRUE))
round(pii,4)


# Det er m?rkeligt
pij <- pii["A"]*(rowSums(Dat_new)["B"]/4)/(sum(Dat_new,na.rm = TRUE)/4-1)
pij <- t(matrix(rep(na.omit(pii),27),ncol=27))*
  matrix(rep(rowSums(na.omit(Dat_new))/4,27),ncol=27)/
  (sum(Dat_new,na.rm = TRUE)/4-1)

# vowel vs consonant