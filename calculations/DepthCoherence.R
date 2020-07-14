X1 <- c("T123","T124","T134")
X2 <- c("T123","T145","T167")

BN <- DepthBN
#graphviz.plot(BN)
depthTable <- CoherencesTable(DepthBN,
                              scenariosList = list(X1,X2),
                              statesList   = list(c("1","1","1"),c("1","1","1")),
                              exampleName = "Depth"
)

#depthTable




# 
# sc <- structuredCoherence(DepthBN,X1,rep("1",3))
# 
# sc2 <- structuredCoherence(DepthBN,X2,rep("1",3))
# 
# sc
# 
# dragOut <- function(sc){
#   ZweightedAnteJoint <- list()
#   for(i in 1:length(sc$`Full calculations`)){
#     ZweightedAnteJoint <- append(ZweightedAnteJoint,sc$`Full calculations`[[i]]$`Options & calculations`$ZweightedAnte)
#   }
#   unlist(ZweightedAnteJoint)
# }
# 
# 
# 
# 
# do <- dragOut(sc)
# 
# do2 <- dragOut(sc2)
# 
# do2
# 
# structuredScore(do)
# 
# structuredScore(do2)


depthTableLaTeX <- tableLaTeX(depthTable)
#depthTableLaTeX


X1greaterX2<- depthTable[1,] > depthTable[2,] 
X1greaterX2

depthResults <- as.data.frame(rbind(X1sameX2))


rownames(depthResults) <- c("Depth: X$_1=$X$_2$")
depthResults

depthResultsLaTeX <- tableLaTeX(depthResults)
#depthResultsLaTeX


#OUTDATED

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


