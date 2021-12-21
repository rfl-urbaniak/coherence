source("measures/CoherenceMeasures2Var.R")

RobbereresCoherencesGeneral <- function(OnlyP,OnlyR,Both,None){
  
  #Create DAG
  PickpocketsAndRobberers4DAG <- model2network("[WhoMurdered][MIsP|WhoMurdered][MIsR|WhoMurdered]")
  
  #Create Root CPT
  WhoMurderedDimnames <- c("OnlyP","OnlyR","Both","None")
  
  WhoMurderedProb4 <-  array(c(OnlyP,OnlyR,Both,None), dim = 4,
                             dimnames = list(WhoMurdered =  WhoMurderedDimnames))
  
  #Other CPTs are fixed
  MIsPProb4 <-  array(c(1,0,0,1,1,0), dim = c(2,4),dimnames = list(MIsP = c("Yes","No"),
                                                                   WhoMurdered = WhoMurderedDimnames))
  MIsRProb4 <-  array(c(0,1,1,0,1,0), dim = c(2,4),dimnames = list(MIsR = c("Yes","No"),
                                                                   WhoMurdered = WhoMurderedDimnames))
  
  #Gather as a CPT
  PickpocketsAndRobberers4CPT <-list(WhoMurdered=WhoMurderedProb4,MIsP=MIsPProb4,MIsR=MIsRProb4)
  
  #Build BN
  PickpocketsAndRobberers4BN <- custom.fit(PickpocketsAndRobberers4DAG,PickpocketsAndRobberers4CPT)
  #graphviz.chart(PickpocketsAndRobberers3BN,type="barprob")
  
  #Now calculate all relevant probabilities
  #------------------------------------
  #Calculate the joint prior
  PickpocketsAndRobberers4JN <- compile(as.grain(PickpocketsAndRobberers4BN))
  MIsPMisRPrior4 <- querygrain(PickpocketsAndRobberers4JN,nodes=c("MIsP","MIsR"),type="joint")[1,1]
  
  #Calculate separate priors
  MIsP4 <- querygrain(PickpocketsAndRobberers4JN,nodes=c("MIsP"))[[1]][1]
  MIsR4 <- querygrain(PickpocketsAndRobberers4JN,nodes=c("MIsR"))[[1]][1]
  
  #Calculate disjunction
  MIsPOrMIsR <- MIsP4 + MIsR4 - MIsPMisRPrior4
  
  #conditionalize on Pickpocket
  Pickpocket4JN <- setEvidence(PickpocketsAndRobberers4JN,nodes="MIsP",states="Yes")
  MIsRIfMIsP4 <- querygrain(Pickpocket4JN,nodes=c("MIsP","MIsR"),type="joint")[1]
  
  #conditionalize on robberer
  Robberer4JN <- setEvidence(PickpocketsAndRobberers4JN,nodes="MIsR",states="Yes")
  MIsPIfMIsR4 <-  querygrain(Robberer4JN,nodes=c("MIsP","MIsR"),type="joint")[1]
  
  #conditionalize on not MIsP
  PickpocketNot4JN <- setEvidence(PickpocketsAndRobberers4JN,nodes="MIsP",states="No")
  MIsRIfMIsNotP4 <- querygrain(PickpocketNot4JN,nodes=c("MIsP","MIsR"),type="joint")[1]
  
  #Conditionalize on not MIsR
  RobbererNot4JN <- setEvidence(PickpocketsAndRobberers4JN,nodes="MIsR",states="No")
  MIsPIfMIsNotR4 <- querygrain(RobbererNot4JN,nodes=c("MIsP","MIsR"),type="joint")[1]
  
  #---------------
  #Calculate coherences
  Ra <-  RA(MIsRIfMIsP4,MIsPIfMIsR4,MIsPMisRPrior4)
  Raf <- RAF(MIsRIfMIsP4,MIsPIfMIsR4, MIsP4,MIsR4)
  Fi <- Fitelson(MIsRIfMIsP4,MIsRIfMIsNotP4,MIsPIfMIsR4,MIsPIfMIsNotR4)
  Ro <- Roche(MIsRIfMIsP4,MIsPIfMIsR4)
  Ol <- Olsson(MIsPMisRPrior4,MIsPOrMIsR)
  Sh <- Shogenji(MIsPMisRPrior4,MIsP4,MIsR4)
  Do <- Douven(AIfB = MIsPIfMIsR4,BIfA = MIsRIfMIsP4,A = MIsP4,B = MIsR4)
  
  Coherences <- c(Ra,Raf,Fi,Ro,Ol,Sh,Do)
  names(Coherences) <- c("RA","RAF","Fitelson","Roche","Olsson","Shogenji","DM")
  return(Coherences)
}
