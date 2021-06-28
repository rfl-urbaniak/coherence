# #Japanese swords robbers-style with arrow from J to O
# #___________________________________________________
# # Japanese swords robbers-style
# japThreeJoDAG <- model2network("[W][J|W][O|W:J]")
# graphviz.plot(japThreeJoDAG)
# wDimnames <- c("Ja","Ow","JO","N")
# 
# Ja1 <- 1050/10e6
# Ow1 <- 1050/10e6
# JO1 <- 9/10e6
# N1 <- (10e6-2109)/10e6
# wProb1 <-  array(c(Ja1,Ow1,JO1,N1), dim = 4,
#                  dimnames = list(W =  wDimnames))
# 
# 
# 
# Ja2 <- 1/100
# Ow2 <- 1/100
# JO2 <- 9/100
# N2 <- (100-11)/100
# wProb2 <-  array(c(Ja2,Ow2,JO2,N2), dim = 4,
#                  dimnames = list(W =  wDimnames))
# 
# 
# Ja3 <- 1/12
# Ow3 <- 1/12
# JO3 <- 9/12
# N3 <- (12-11)/12
# wProb3 <-  array(c(Ja3,Ow3,JO3,N3), dim = 4,
#                  dimnames = list(W =  wDimnames))
# 
# Jprob <-  array(c(1,0,0,1,1,0,0,1), dim = c(2,4),dimnames = list(J = c("1","0"),
#                                                                  W = wDimnames))
# 
# 
# 
# 
# OprobJo <-  array(c(0,1,3,4,5,6,7,8,9,10,11,12,13,14,15,16), dim = c(2,2,4),dimnames = list(O = c("1","0"), J = c("1","0"), W = wDimnames))
# 
# OprobJo
# 
# japThreeCPT1 <-list(W=wProb1,J=Jprob,O=Oprob)
# japThreeCPT2 <-list(W=wProb2,J=Jprob,O=Oprob)
# japThreeCPT3 <-list(W=wProb3,J=Jprob,O=Oprob)
# 
# japThreeBN1 <- custom.fit(japThreeDAG,japThreeCPT1)
# japThreeBN2 <- custom.fit(japThreeDAG,japThreeCPT2)
# japThreeBN3 <- custom.fit(japThreeDAG,japThreeCPT3)
# 












# Japanese swords robbers-style
japThreeDAG <- model2network("[W][J|W][O|W]")
#graphviz.plot(japThreeDAG1)
wDimnames <- c("Ja","Ow","JO","N")

Ja1 <- 1050/10e6
Ow1 <- 1050/10e6
JO1 <- 9/10e6
N1 <- (10e6-2109)/10e6
wProb1 <-  array(c(Ja1,Ow1,JO1,N1), dim = 4,
                          dimnames = list(W =  wDimnames))

Ja2 <- 1/100
Ow2 <- 1/100
JO2 <- 9/100
N2 <- (100-11)/100
wProb2 <-  array(c(Ja2,Ow2,JO2,N2), dim = 4,
                 dimnames = list(W =  wDimnames))


Ja3 <- 1/12
Ow3 <- 1/12
JO3 <- 9/12
N3 <- (12-11)/12
wProb3 <-  array(c(Ja3,Ow3,JO3,N3), dim = 4,
                 dimnames = list(W =  wDimnames))

Jprob <-  array(c(1,0,0,1,1,0,0,1), dim = c(2,4),dimnames = list(J = c("1","0"),
                                                                 W = wDimnames))

Oprob <-  array(c(0,1,1,0,1,0,0,1), dim = c(2,4),dimnames = list(O = c("1","0"), W = wDimnames))

japThreeCPT1 <-list(W=wProb1,J=Jprob,O=Oprob)
japThreeCPT2 <-list(W=wProb2,J=Jprob,O=Oprob)
japThreeCPT3 <-list(W=wProb3,J=Jprob,O=Oprob)

japThreeBN1 <- custom.fit(japThreeDAG,japThreeCPT1)
japThreeBN2 <- custom.fit(japThreeDAG,japThreeCPT2)
japThreeBN3 <- custom.fit(japThreeDAG,japThreeCPT3)





#Japanese swords with two nodes
#Define DAG

# J: Murderer is Japanese
# O: Muderer owns a a Samurai sword
JapDAG <- model2network("[J][O|J]")
#graphviz.plot(JapDAG)

#Define CPTS

# Scenario 1:
Jprior1 <- .000105+.0000009
JProb1 <-  priorCPT("J",prob1 = Jprior1)
OProb1 <-  singleCPT(eNode = "O", hNode = "J", 
                    probEifHS1 = .0000009/Jprior1,
                    probEifHS2 = .000105/(1-Jprior1))
Jap1CPT <-list(J=JProb1, O=OProb1)

# Scenario 2:
JProb2 <-  priorCPT("J",prob1 = .1)
OProb2 <-  singleCPT(eNode = "O", hNode = "J", 
                    probEifHS1 = .09/.1, 
                    probEifHS2 = .01/.9)
Jap2CPT <-list(J=JProb2, O=OProb2)


# Scenario 3:
JProb3 <-  priorCPT("J",prob1 = 10/12)
OProb3 <-  singleCPT(eNode = "O", hNode = "J", 
                    probEifHS1 = 9/10, 
                    probEifHS2 = 1/2)
Jap3CPT <-list(J=JProb3, O=OProb3)


#BNs
# Scenario 1:
Jap1BN <- custom.fit(JapDAG,Jap1CPT)
#have a look 
#graphviz.chart(Jap1BN,type="barprob")

# Scenario 2:
Jap2BN <- custom.fit(JapDAG,Jap2CPT)
#have a look 
#graphviz.chart(Jap2BN,type="barprob")

# Scenario 3
Jap3BN <- custom.fit(JapDAG,Jap3CPT)
#graphviz.chart(Jap3BN,type="barprob")





# test joint distribution:
# 
# Jap1JN <- compile(as.grain(Jap1BN))
# Jap1Joints <- querygrain(Jap1JN, nodes=c("J","O"), type="joint")
# Jap1Joints
# 
# Jap2JN <- compile(as.grain(Jap2BN))
# Jap2Joints <- querygrain(Jap2JN, nodes=c("J","O"), type="joint")
# Jap2Joints
