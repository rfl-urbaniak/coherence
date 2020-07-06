
###_______________________________
#THIS IS MOST LIKELY OUTDATED


#Define DAG
PickpocketsAndRobberersDAG <- model2network("[WhoMurdered][MIsP|WhoMurdered][MIsR|WhoMurdered]")

#Illustrate DAG
#graphviz.plot(PickpocketsAndRobberersDAG)

#Define CPTS
WhoMurderedProb <- array(rep(0.1,10), dim = 10, dimnames = list(WhoMurdered =  1:10))

MIsPProb <- array(c(1,0,1,0,1,0,1,0,1,0,1,0,0,1,0,1,1,0,1,0), dim = c(2,10),
                  dimnames = list(MIsP = c("Yes","No"), WhoMurdered = 1:10))

MIsRProb <- array(c(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,0,1,0,1), dim = c(2,10),
                  dimnames = list(MIsR = c("Yes","No"), WhoMurdered = 1:10))

WhoMurderedProb
MIsPProb
MIsRProb


#PickpocketsAndRobberersCPT
PickpocketsAndRobberersCPT <-list(WhoMurdered=WhoMurderedProb,MIsP=MIsPProb,MIsR=MIsRProb)

#PickpocketsAndRobberersBN
PickpocketsAndRobberersBN <- custom.fit(PickpocketsAndRobberersDAG,PickpocketsAndRobberersCPT)

#visualise if you want
#graphviz.chart(PickpocketsAndRobberersBN,type="barprob")

## Problem: on Fitelson and some others, Coherence of MIsP=Yes, MIsR=Yes was negative or low. Intuitively we want this to be high. Let's see.

#------------------------------------
#conditionalize on Pickpocket
PickpocketsAndRobberersJN <- compile(as.grain(PickpocketsAndRobberersBN))

MIsPMisRPrior <- querygrain(PickpocketsAndRobberersJN,nodes=c("MIsP","MIsR"),type="joint")
#MIsPMisRPrior
MIsPMisRPrior[1,1]

PickpocketJN <- setEvidence(PickpocketsAndRobberersJN,nodes="MIsP",states="Yes")

MIsRIfMIsP <- querygrain(PickpocketJN,nodes=c("MIsP","MIsR"),type="joint")

#MIsRIfMIsP
MIsRIfMIsP[1]
#-----------------------------------

#conditionalize on robberer
RobbererJN <- setEvidence(PickpocketsAndRobberersJN,nodes="MIsR",states="Yes")
MIsPIfMIsR <- querygrain(RobbererJN,nodes=c("MIsP","MIsR"),type="joint")
# MIsPIfMIsR
MIsPIfMIsR[1]
#-----------------------------------

#calculate coherence of MIsR=MIsP=Yes

mean(Z(MIsRIfMIsP[1],MIsPMisRPrior[1,1]),
     Z(MIsPIfMIsR[1],MIsPMisRPrior[1,1]))


###
### Now check the new coherence of MIsP=MIsR=1

#------------------------------------
#conditionalize on Pickpocket
PickpocketsAndRobberers3JN <- compile(as.grain(PickpocketsAndRobberers3BN))

MIsPMisRPrior3 <- querygrain(PickpocketsAndRobberers3JN,nodes=c("MIsP","MIsR"),type="joint")
#MIsPMisRPrior3
#MIsPMisRPrior3[1,1]

MIsPPrior3 <- querygrain(PickpocketsAndRobberers3JN,nodes=c("MIsP"))[[1]][1]
MIsRPrior3 <- querygrain(PickpocketsAndRobberers3JN,nodes=c("MIsR"))[[1]][1]

Pickpocket3JN <- setEvidence(PickpocketsAndRobberers3JN,nodes="MIsP",states="Yes")

MIsRIfMIsP3 <- querygrain(Pickpocket3JN,nodes=c("MIsP","MIsR"),type="joint")[1]

MIsRIfMIsP3
#-----------------------------------

#conditionalize on robberer
Robberer3JN <- setEvidence(PickpocketsAndRobberers3JN,nodes="MIsR",states="Yes")
MIsPIfMIsR3 <- querygrain(Robberer3JN,nodes=c("MIsP","MIsR"),type="joint")[1]
MIsPIfMIsR3
#-----------------------------------

#calculate coherence of MIsR=MIsP=Yes

mean(Z(MIsRIfMIsP3[1],MIsPMisRPrior3[1,1]),
     Z(MIsPIfMIsR3[1],MIsPMisRPrior3[1,1]))

RAE(AIfB = MIsPIfMIsR3,BIfA = MIsRIfMIsP3,BAndA = MIsPMisRPrior,A = MIsPPrior3,B = MIsRPrior3)


#coherence of MIsP and MIsR
RAF(AIfB = MIsPIfMIsR3,BIfA = MIsRIfMIsP3,A = MIsRPrior3,B=MIsPPrior3)

#coherece of NegMIsP and NegMIsR
RAF(AIfB = MIsPIfMIsR3,BIfA = MIsRIfMIsP3,A = MIsRPrior3,B=MIsPPrior3)


AnB <- 1
A <- 0.8
AAndNB <- 0.2

nBA <- 0.25
nB <- 0.2

(Z(AnB,AAndNB)+Z(nBA,AAndNB))/2

RA(AIfB = AnB,BIfA = nBA,BAndA = AAndNB)


Z(posterior = 0.98,prior=0.99)

#-----------------------------------
#NOW WITH FITELSON'S MEASURE

#conditionalize on not MIsP
PickpocketNot3JN <- setEvidence(PickpocketsAndRobberers3JN,nodes="MIsP",states="No")

MIsRIfMIsNotP3 <-
  querygrain(PickpocketNot3JN,nodes=c("MIsP","MIsR"),type="joint")

MIsRIfMIsNotP3
MIsRIfMIsNotP3[1]

#Conditionalize on not MIsR
RobbererNot3JN <- setEvidence(PickpocketsAndRobberers3JN,nodes="MIsR",states="No")

MIsPIfMIsNotR3 <-
  querygrain(RobbererNot3JN,nodes=c("MIsP","MIsR"),type="joint")

MIsPIfMIsNotR3
MIsPIfMIsNotR3[1]


#AIfB,AIfNotB,BIfA,BIfNotA
Fitelson(MIsRIfMIsP3[1],MIsRIfMIsNotP3[1],MIsPIfMIsR3[1],MIsPIfMIsNotR3[1])

# Now calculate Roche

Roche(MIsRIfMIsP3[1],MIsPIfMIsR3[1])


# Now Olsson; first we need joint and disjunction

MIsPMisRPrior3[1]

#conditionalize on Pickpocket
MIsP3 <- querygrain(PickpocketsAndRobberers3JN,nodes=c("MIsP"))[[1]][1]
MIsP3

MIsR3 <- querygrain(PickpocketsAndRobberers3JN,nodes=c("MIsR"))[[1]][1]
MIsR3

MIsPOrMIsR <- MIsP3 + MIsR3 -   MIsPMisRPrior3[1]
MIsPOrMIsR

Olsson(MIsPMisRPrior3[1],MIsPOrMIsR)


# Now Shogenji

Shogenji(MIsPMisRPrior3[1],MIsP3,MIsR3)


# Now Douven
#AifB,BifA,A,B

Douven(AIfB = MIsPIfMIsR3[1],BIfA = MIsRIfMIsP3[1],A = MIsP3,B = MIsR3)
#notice this is negative

round(RobbereresCoherences(OnlyP = 0.2,OnlyR = 0.2 ,Both = 0.6),3)


#not sure what lesson we should draw from these experiments. :)
