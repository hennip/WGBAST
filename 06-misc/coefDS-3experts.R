###########################################################
#   Simojoen yliarvio parametrin priorijakauman luonti    #
#       3 ekspertin estimaattien yhdistely                #
###########################################################



#   Ekspertti 1
#   mediaani 1.5 90%PI 1.1-2.2

#   Ekspertti 2
#   mediaani 1.0375 90%PI 1-1.075

#   Ekspertti 3
#   mediaani 1.025 90%PI 1-1.05


#   Hyvin tiukkoja jakaumia, mutta onneksi E2 ja E2 ovat symmetrisiä.
#   vain E1 on vino

#   Haetaan vastaavat jakaumat E2 ja E3 normaalista ja E1 gammasta
#   näitä kerrottessa, normaali*normaali*gamma saadaan normaalijakauma
#   konjugaattisuuden vuoksi


#   E2 ja E3 odotusarvot tiedetään == mediaani
#   keskihajonta voidaan laskea 90%PI avulla, voitaisiin toki myös
#   hakea kokeilemalla

m2 = 1.0375
q5_2 = 1; q95_2 = 1.075


m3 = 1.025
q5_3 = 1; q95_3 = 1.05


#   kvantiilin kaava standardi normaali jakauman kautta saadaan seuraavasti
#   qX = mu +zX*sd

#   1: qX1 = mu +zX1*sd 2: qX2 = mu +zX2*sd
#   qX1 = qX2-zX2*sd + zX1*sd 
#   sd = (qX1-qX2)/(zX1-zX2)
#   

#   qX on kvantiilin arvo ja zX on standardi nomraalijakauman kvantiilifunktiosta
#   "mikä tiheysfunktion arvo on, kun X prosenttia massasta on kertynyt"

#   zX arvot
z05 <- qnorm(0.05)
z95 <- qnorm(0.95)

sd2 <- (q5_2-q95_2)/(z05-z95)
sd3 <- (q5_3-q95_3)/(z05-z95)


#   E1 pitää ottaa gammasta
quantile(rgamma(10000, 25,16), c(0.05,0.5, 0.95 ))
#   näyttää olevan aika lähellä

#   E2
quantile(rnorm(10000, m2,sd2), c(0.05,0.5, 0.95 ))

#   E3
quantile(rnorm(10000, m3,sd3), c(0.05,0.5, 0.95 ))

#############################
#   3 jakauman yhdistelmä    #
#############################

#   tehdään näistä yhdessä sample ja ammutaan fitdistiä

raw_sample <- c(rgamma(100000, 25,16),
                rnorm(100000, m2,sd2),
                rnorm(100000, m3,sd3)
                )
library(fitdistrplus)
lnormpar <- fitdist(raw_sample, "lnorm")
lnormpar

#   varmistetaan ja muutetaan cv mukaiseen muotoon
meanlog <- lnormpar$estimate[1] %>% as.numeric
sdlog <- lnormpar$estimate[2] %>% as.numeric

#   M <- log(mu)- 0.5*log(cv_k^2+1)
#   T <- (log(cv_k^2+1))^-1

cv <- sqrt(exp(sdlog^2)-1)

mu <- exp(
  meanlog+0.5*log(cv^2+1)
)


quantile(
  rlnorm(10000, log(mu)-0.5*log(cv^2+1),
         sqrt(log(cv^2+1))), 
  c(0.05,0.5, 0.95 ))

plot(density(
  rlnorm(10000, log(mu)-0.5*log(cv^2+1),
         sqrt(log(cv^2+1)))
), type = 'l')

hist(rlnorm(10000, log(mu)-0.5*log(cv^2+1),
            sqrt(log(cv^2+1))))


#   3 yhdistetyn jakauman lognormalin mu on
mu   
#   1.205355

#   ja cv 
cv
#   0.2229662



#################################
#   yhdisetään vain E2 ja E3    #
#################################



quantile(c(rnorm(100000, m2, sd2),
           rnorm(100000, m3, sd3)),
         c(0.05, 0.5, 0.95))

raw_sample2 <- c(rnorm(100000, m2, sd2),
                 rnorm(100000, m3, sd3))

lnormpar2 <- fitdist(raw_sample2, "lnorm")

lnormpar2

#   varmistetaan ja muutetaan cv mukaiseen muotoon
meanlog2 <- lnormpar2$estimate[1] %>% as.numeric
sdlog2 <- lnormpar2$estimate[2] %>% as.numeric

#   M <- log(mu)- 0.5*log(cv_k^2+1)
#   T <- (log(cv_k^2+1))^-1

cv2 <- sqrt(exp(sdlog2^2)-1)

mu2 <- exp(
  meanlog2+0.5*log(cv2^2+1)
)


quantile(
  rlnorm(100000, log(mu2)-0.5*log(cv2^2+1),
         sqrt(log(cv2^2+1))), 
  c(0.05,0.5, 0.95 ))

plot(density(
  rlnorm(10000, log(mu2)-0.5*log(cv2^2+1),
         sqrt(log(cv2^2+1)))
), type = 'l')

hist(rlnorm(10000, log(mu)-0.5*log(cv^2+1),
            sqrt(log(cv^2+1))))



#   E2 ja E3 yhdistetyn jakauman lognormalin mu on
mu2   
#   1.031237

#   ja cv 
cv2
#   0.01970493
