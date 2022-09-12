library(bnlearn)
library(gRain)
###Just two nodes

robbersTwoDAG <- model2network("[MIsP][MIsR|MIsP]")

MIsPProb <- priorCPT("MIsP",prob1 = .8)

MIsRProb <- singleCPT(eNode = "MIsR",hNode = "MIsP", probEifHS1 = 6/8, probEifHS2 = 1)  

robbersTwoCPT <-list(MIsR=MIsRProb,MIsP=MIsPProb)

robbersTwoBN <- custom.fit(robbersTwoDAG,robbersTwoCPT)

JN <- compile(as.grain(robbersTwoBN))



#Sgraphviz.plot(robbersTwoDAG)

###  With non-trivial probs


#odel2network("[B][P|B][G|B:P]")
robbersUncDAG <- model2network("[Murder][MIsP|Murder][MIsR|Murder:MIsP]")

#graphviz.plot(robbersUncDAG)

#graphviz.plot(BirdDAG3, sub = "Penguins with B=1, G =1",  highlight = list(nodes = c("Murder","MIsR"), col = "skyblue", fill= "skyblue"))

#Define CPTS
MurderProb <-  priorCPT("Murder",prob1 = 1)

MIsPProb <- singleCPT(eNode = "MIsP",hNode = "Murder", probEifHS1 = .8, probEifHS2 = 0)  

MIsRProb <- doubleCPT(eNode = "MIsR",h1Node = "Murder",h2Node = "MIsP",probEifH1S1H2S1 =  6/8, 
                      probEifH1S1H2S2 = 1, probEifH1S2H2S1 = 0, probEifH1S2H2S2 = 0)

#MIsRProb
#MurderProb
#GProb

#BirdCPT
robbersUncCPT <-list(Murder=MurderProb,MIsR=MIsRProb,MIsP=MIsPProb)

#BirdBN
robbersUncBN <- custom.fit(robbersUncDAG,robbersUncCPT)



JN <- compile(as.grain(robbersUncBN))















#----------------------------------
## NOW WITH THREE STATES OF THE ROOT

robbersDAG <- model2network("[WhoMurdered][MIsP|WhoMurdered][MIsR|WhoMurdered]")

robbersDAGsimplified <- model2network("[W][P|W][R|W]")

#graphviz.plot(robbersDAG)

#Define CPTS

whoMurderedDimnames <- c("OnlyP","OnlyR","Both")

whoMurderedProb <-  array(c(0.2,0.2,0.6), dim = 3,
                           dimnames = list(WhoMurdered =  whoMurderedDimnames))


MIsPProb <-  array(c(1,0,0,1,1,0), dim = c(2,3),dimnames = list(MIsP = c("1","0"), WhoMurdered = whoMurderedDimnames))


#MIsPProb

MIsRProb <-  array(c(0,1,1,0,1,0), dim = c(2,3),dimnames = list(MIsR = c("1","0"), WhoMurdered = whoMurderedDimnames))

robbersCPT <-list(WhoMurdered=whoMurderedProb,MIsP=MIsPProb,MIsR=MIsRProb)

#robbersCPT
#PickpocketsAndRobberers3CPT 

robbersBN <- custom.fit(robbersDAG,robbersCPT)


#graphviz.plot(robbersDAG)

#graphviz.chart(RobbersBN,type="barprob")


#robbersBN


#subnetworks

robbersJN <- compile(as.grain(robbersBN))

RobbersJNP1 <- setEvidence(robbersJN, node = "MIsP", state =   "1")


RobbersJNP0 <- setEvidence(robbersJN,"MIsP", "0")

RobbersJNR1 <- setEvidence(robbersJN,"MIsR",  "1")
RobbersJNR0 <- setEvidence(robbersJN,"MIsR", "0")

#querygrain(RobbersJNP1, "MIsR")
#querygrain(RobbersJNP0, "MIsR")



robbersNarrDAG <- model2network("[MIsP][MIsR|MIsP]")
#graphviz.plot(robbersNarrDAG)

MisPNarrProb <- priorCPT(node = "MIsP", prob1 = 0.8)

MisRNarrProb <- singleCPT("MIsR", "MIsP", probEifHS1 = 0.75, probEifHS2 = 1)


robbersNarrCPT <- list(MIsP = MisPNarrProb, MIsR = MisRNarrProb)

robbersNarrBN <- custom.fit(robbersNarrDAG,robbersNarrCPT)




# 
# 
# #_______________________________  BACK TO THE MORE COMPLICATED ONE
# 
# robbersLargeDAG <- model2network("[W][P|W][R|W]")
# 
# #Illustrate DAG
# #graphviz.plot(robbersLargeDAG)
# 
# #Define CPTS
# WProb <- array(rep(0.1,10), dim = 10, dimnames = list(W =  1:10))
# 
# #WProb
# 
# PProb <- array(c(1,0,1,0,1,0,1,0,1,0,1,0,0,1,0,1,1,0,1,0), dim = c(2,10),
#                   dimnames = list(P = c("1","0"), W = 1:10))
# 
# #PProb
# 
# RProb <- array(c(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,0,1,0,1), dim = c(2,10),
#                   dimnames = list(R = c("1","0"), W = 1:10))
# 
# #PProb
# #RProb
# 
# 
# #PickpocketsAndRobberersCPT
# robbersLargeCPT <-list(W=WProb,P=PProb,R=RProb)
# 
# #robbersLargeCPT
# 
# #PickpocketsAndRobberersBN
# robbersLargeBN <- custom.fit(robbersLargeDAG,robbersLargeCPT)
# 
# 
# #robbersLargeBN
# 
# 
# BN <- robbersLargeBN
# #robbersTable2 <- CoherencesTable2(robbersLargeBN,
# #                                   scenariosList = list(c("P","R"),c("P","R"),c("P","R")),
# #                                   statesList   = list(c("1","1"),c("1","0"),c("0","1")),
# #                                   exampleName = "Robbers"
# # )
# 
# 
# #visualise if you want
# #graphviz.chart(robbersLargeBN,type="barprob")
# 
# ## Problem: on Fitelson and some others, Coherence of MIsP=Yes, MIsR=Yes was negative or low. Intuitively we want this to be high. Let's see.
# 
# #------------------------------------
# #conditionalize on Pickpocket
# # PickpocketsAndRobberersJN <- compile(as.grain(PickpocketsAndRobberersBN))
# # 
# # MIsPMisRPrior <- querygrain(PickpocketsAndRobberersJN,nodes=c("MIsP","MIsR"),type="joint")
# # #MIsPMisRPrior
# # MIsPMisRPrior[1,1]
# # 
# # PickpocketJN <- setEvidence(PickpocketsAndRobberersJN,nodes="MIsP",states="Yes")
# # 
# # MIsRIfMIsP <- querygrain(PickpocketJN,nodes=c("MIsP","MIsR"),type="joint")
# # 
# # #MIsRIfMIsP
# # MIsRIfMIsP[1]
# # #-----------------------------------
# # 
# # #conditionalize on robberer
# # RobbererJN <- setEvidence(PickpocketsAndRobberersJN,nodes="MIsR",states="Yes")
# # MIsPIfMIsR <- querygrain(RobbererJN,nodes=c("MIsP","MIsR"),type="joint")
# # # MIsPIfMIsR
# # MIsPIfMIsR[1]
# # #-----------------------------------
# # 
# # #calculate coherence of MIsR=MIsP=Yes
# # 
# # mean(Z(MIsRIfMIsP[1],MIsPMisRPrior[1,1]),
# #      Z(MIsPIfMIsR[1],MIsPMisRPrior[1,1]))
# # 
# 
# 
# 
# 
# PickpocketsAndRobberersDAG <- model2network("[WhoMurdered][MIsP|WhoMurdered][MIsR|WhoMurdered]")
# 
# #Illustrate DAG
# #graphviz.plot(PickpocketsAndRobberersDAG)
# 
# #Define CPTS
# WhoMurderedProb <- array(rep(0.1,10), dim = 10, dimnames = list(WhoMurdered =  1:10))
# 
# MIsPProb <- array(c(1,0,1,0,1,0,1,0,1,0,1,0,0,1,0,1,1,0,1,0), dim = c(2,10),
#                   dimnames = list(MIsP = c("Yes","No"), WhoMurdered = 1:10))
# 
# MIsRProb <- array(c(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,0,1,0,1), dim = c(2,10),
#                   dimnames = list(MIsR = c("Yes","No"), WhoMurdered = 1:10))
# 
# WhoMurderedProb
# MIsPProb
# MIsRProb
# 
# 
# #PickpocketsAndRobberersCPT
# PickpocketsAndRobberersCPT <-list(WhoMurdered=WhoMurderedProb,MIsP=MIsPProb,MIsR=MIsRProb)
# 
# #PickpocketsAndRobberersBN
# PickpocketsAndRobberersBN <- custom.fit(PickpocketsAndRobberersDAG,PickpocketsAndRobberersCPT)
# 
# #visualise if you want
# #graphviz.chart(PickpocketsAndRobberersBN,type="barprob")
# 
# ## Problem: on Fitelson and some others, Coherence of MIsP=Yes, MIsR=Yes was negative or low. Intuitively we want this to be high. Let's see.
# 
# #------------------------------------
# #conditionalize on Pickpocket
# PickpocketsAndRobberersJN <- compile(as.grain(PickpocketsAndRobberersBN))
# 
# MIsPMisRPrior <- querygrain(PickpocketsAndRobberersJN,nodes=c("MIsP","MIsR"),type="joint")
# #MIsPMisRPrior
# MIsPMisRPrior[1,1]
# 
# PickpocketJN <- setEvidence(PickpocketsAndRobberersJN,nodes="MIsP",states="Yes")
# 
# MIsRIfMIsP <- querygrain(PickpocketJN,nodes=c("MIsP","MIsR"),type="joint")
# 
# #MIsRIfMIsP
# MIsRIfMIsP[1]
# #-----------------------------------
# 
# #conditionalize on robberer
# RobbererJN <- setEvidence(PickpocketsAndRobberersJN,nodes="MIsR",states="Yes")
# MIsPIfMIsR <- querygrain(RobbererJN,nodes=c("MIsP","MIsR"),type="joint")
# # MIsPIfMIsR
# MIsPIfMIsR[1]
# #-----------------------------------
# 
# #calculate coherence of MIsR=MIsP=Yes
# 
# mean(Z(MIsRIfMIsP[1],MIsPMisRPrior[1,1]),
#      Z(MIsPIfMIsR[1],MIsPMisRPrior[1,1]))
# 
# 
# 
