source("measures/CoherenceMeasures2Var.R")

RobbereresCoherences <- function(OnlyP,OnlyR,Both){
 
  #Create DAG
  PickpocketsAndRobberers3DAG <- model2network("[WhoMurdered][MIsP|WhoMurdered][MIsR|WhoMurdered]")
  
  #Create Root CPT
  WhoMurderedDimnames <- c("OnlyP","OnlyR","Both")
  
  WhoMurderedProb3 <-  array(c(OnlyP,OnlyR,Both), dim = 3,
                             dimnames = list(WhoMurdered =  WhoMurderedDimnames))
  
  #Other CPTs are fixed
  MIsPProb3 <-  array(c(1,0,0,1,1,0), dim = c(2,3),dimnames = list(MIsP = c("Yes","No"),
                                                                   WhoMurdered = WhoMurderedDimnames))
  MIsRProb3 <-  array(c(0,1,1,0,1,0), dim = c(2,3),dimnames = list(MIsR = c("Yes","No"),
                                                                   WhoMurdered = WhoMurderedDimnames))
  
  #Gather as a CPT
  PickpocketsAndRobberers3CPT <-list(WhoMurdered=WhoMurderedProb3,MIsP=MIsPProb3,MIsR=MIsRProb3)
  
  #Build BN
  PickpocketsAndRobberers3BN <- custom.fit(PickpocketsAndRobberers3DAG,PickpocketsAndRobberers3CPT)
  #graphviz.chart(PickpocketsAndRobberers3BN,type="barprob")
  
  #Now calculate all relevant probabilities
  #------------------------------------
  #Calculate the joint prior
  PickpocketsAndRobberers3JN <- compile(as.grain(PickpocketsAndRobberers3BN))
  MIsPMisRPrior3 <- querygrain(PickpocketsAndRobberers3JN,nodes=c("MIsP","MIsR"),type="joint")[1,1]
  
  #Calculate separate priors
  MIsP3 <- querygrain(PickpocketsAndRobberers3JN,nodes=c("MIsP"))[[1]][1]
  MIsR3 <- querygrain(PickpocketsAndRobberers3JN,nodes=c("MIsR"))[[1]][1]

  #Calculate disjunction
  MIsPOrMIsR <- MIsP3 + MIsR3 - MIsPMisRPrior3
  
  #conditionalize on Pickpocket
  Pickpocket3JN <- setEvidence(PickpocketsAndRobberers3JN,nodes="MIsP",states="Yes")
  MIsRIfMIsP3 <- querygrain(Pickpocket3JN,nodes=c("MIsP","MIsR"),type="joint")[1]
  
  #conditionalize on robberer
  Robberer3JN <- setEvidence(PickpocketsAndRobberers3JN,nodes="MIsR",states="Yes")
  MIsPIfMIsR3 <-  querygrain(Robberer3JN,nodes=c("MIsP","MIsR"),type="joint")[1]

  #conditionalize on not MIsP
  PickpocketNot3JN <- setEvidence(PickpocketsAndRobberers3JN,nodes="MIsP",states="No")
  MIsRIfMIsNotP3 <- querygrain(PickpocketNot3JN,nodes=c("MIsP","MIsR"),type="joint")[1]

  #Conditionalize on not MIsR
  RobbererNot3JN <- setEvidence(PickpocketsAndRobberers3JN,nodes="MIsR",states="No")
  MIsPIfMIsNotR3 <- querygrain(RobbererNot3JN,nodes=c("MIsP","MIsR"),type="joint")[1]
  
  #---------------
  #Calculate coherences
  Ra <-  RA(MIsRIfMIsP3,MIsPIfMIsR3,MIsPMisRPrior3)
  Raf <- RAF(MIsRIfMIsP3,MIsPIfMIsR3, MIsP3,MIsR3)
  Fi <- Fitelson(MIsRIfMIsP3,MIsRIfMIsNotP3,MIsPIfMIsR3,MIsPIfMIsNotR3)
  Ro <- Roche(MIsRIfMIsP3,MIsPIfMIsR3)
  Ol <- Olsson(MIsPMisRPrior3,MIsPOrMIsR)
  Sh <- Shogenji(MIsPMisRPrior3,MIsP3,MIsR3)
  Do <- Douven(AIfB = MIsPIfMIsR3,BIfA = MIsRIfMIsP3,A = MIsP3,B = MIsR3)
  
  Coherences <- c(Ra,Raf,Fi,Ro,Ol,Sh,Do)
  names(Coherences) <- c("RA","RAF","Fitelson","Roche","Olsson","Shogenji","DM")
  return(Coherences)
}
