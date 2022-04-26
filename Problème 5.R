ALtitude<- c(1040 ,1230 ,1500 ,1600 ,1740 ,1950 ,2200 ,2530 ,2800 ,3100)
tmp <- c(7.4 ,6 ,4.5 ,3.8 ,2.9 ,1.9 ,1.0 ,-1.2 ,-1.5 ,-4.5)
plot(ALtitude,tmp, pch=16) 
abline(regression,col="red",lwd=2)


#a)
regression <- lm(ALtitude~tmp)
(sxy <- cov(ALtitude,tmp))
sum(t((ALtitude- mean(ALtitude)))%*%(tmp-mean(tmp))/(length(ALtitude))-1)

(r<- cor(ALtitude,tmp))
#coefficien de coorelation = -0.9936053

#b)
(b<- cov(ALtitude,tmp)/var(ALtitude))
(a<- mean(tmp)-b*mean(ALtitude) )
c(a,b)

# Y = 2342.4985 - -183.9894X

#c)

# test sur  a = 0 
test <- cor.test(ALtitude,tmp)
alpha <- 0.05
pvalue<- test$p.value
(pvalue<alpha)
# test significatif

#d)

Y <- a+b*2000
Y
# temperatue a 2000 metre est 1.8 