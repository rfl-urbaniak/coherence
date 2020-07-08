#source("measures//CoherenceMeasures2Var.R")
#source("measures//Fitelson.R")
#source("measures//RA.R")
#source("bns//Depth.R")

#_____________________________
X1 <- c("T123","T124","T134")
X2 <- c("T123","T145","T167")

depthTable <- CoherencesTable(DepthBN,
                              scenariosList = list(X1,X2),
                              statesList   = list(c("1","1","1"),c("1","1","1")),
                              exampleName = "Depth"
)
depthTable



depthTableLaTeX <- tableLaTeX(depthTable)
#depthTableLaTeX


X1sameX2 <- depthTable[1,] == depthTable[2,] 
#X1sameX2

depthResults <- as.data.frame(rbind(X1sameX2))


rownames(depthResults) <- c("Depth: X$_1=$X$_2$")
#depthResults

depthResultsLaTeX <- tableLaTeX(depthResults)
#depthResultsLaTeX

#RAcoherenceForBNs(BN = DepthBN,narrationNodes = X1,states = c("1","1","1"))
#coherence 0.2380952

#RAcoherenceForBNs(BN = DepthBN,narrationNodes = X2,states = c("1","1","1"))

#coherence 0.2380952

#notice support levels don't really change


#_______________________________
#Fitelson coherence

#no logical relationships btw nodes within the narrations

#DepthFitelsonX1 <- FitelsonCoherenceForBNs(BN =  DepthBN, narrationNodes = X1,
  #                                         states = c("1","1","1"))
#DepthFitelsonX1

#coherence 0.3817308

#DepthFitelsonX2 <- FitelsonCoherenceForBNs(BN =  DepthBN, narrationNodes = X2,
 #                                          states = c("1","1","1"))

#DepthFitelsonX2

#coherence  a bit lower: 0.3434343


