###################################################################
####################### Exercice 4 ###############################
##################################################################

#Question1

N<-300
G1<- 225
G2<- 195

alpha<- 0.05
#intervale de confiance  G1
phat1<- G1/N 
phat1

z_alpha1<- qnorm(1-alpha/2)
z_alpha1

(marge_erreur1 <- z_alpha1*sqrt(phat1*(1-phat1)/N))

borne_inf <- phat1 - marge_erreur1
borne_sup <- phat1 + marge_erreur1
(int_confiance <- c(borne_inf,borne_sup))

#intervale de confiance de G1 = [0.7010009 , 0.7989991]

#intervale de confiance G2
phat2<- G2/N 
phat2

z_alpha2<- qnorm(1-alpha/2)
z_alpha2

(marge_erreur2 <- z_alpha2*sqrt(phat2*(1-phat2)/N))

borne_inf <- phat2 - marge_erreur2
borne_sup <- phat2 + marge_erreur2
(int_confiance <- c(borne_inf,borne_sup))
#intervale de confiance de G2 = [0.5960268 , 0.7039732]

#Question 2 

z0 <- (phat1 - phat2)/sqrt(phat1*(1-phat1)/G1 + phat2*(1-phat2)/G2)
pvalue <- 2*(1-pnorm(abs(z0)))
pvalue

pvalue < alpha # True 

#le test est significatif 
# donc  le pourcentage de guéris dans le groupe G1 est supérieur au pourcentage de guéris dans
#le groupe G2
