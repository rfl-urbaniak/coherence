library(irr)


data(diagnoses)


diagnoses

x1a <-  c(rep(1,3), rep(0, 5))
x1b <-  c(1, 1, 0, 1, 0, 0, 0, 0 )
x1c <- c(1, 0, 1, 1, 0, 0, 0, 0)


x1 <- data.frame(x1a, x1b, x1c)

kappam.fleiss(x1)
kappam.light(x1)


x2a <-  c(rep(1,3), rep(0, 5))
x2b <-  c(1, 0, 0, 1, 1, 0, 0, 0 )
x2c <- c(1, 0, 0, 0, 0, 1, 1, 0)

x2 <- data.frame(x2a, x2b, x2c)

kappam.fleiss(x2)
kappam.light(x2)

str(kf1)

round(kf1$value,3)




?kappam.fleiss

Fleiss’ Kappa for m raters

 kappam.light
Examples


kappam.fleiss(diagnoses) # Fleiss' Kappa
kappam.fleiss(diagnoses, exact=TRUE) # Exact Kappa
kappam.fleiss(diagnoses, detail=TRUE) # Fleiss' and category-wise Kappa

kappam.light Light’s Kappa for m raters