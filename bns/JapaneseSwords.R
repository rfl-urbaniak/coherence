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
