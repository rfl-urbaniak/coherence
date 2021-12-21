#The intuition is that M, G, W, I should be less coherent than M, T, G, W, I 
# 
Dunnit <- c("M","G","W","I")
DunnitTwin<- c("M","Tw","G","W","I")
# #

DunnitNoTwinTable <- CoherencesTable(DunnitNoTwinBN,
  scenariosList = list(Dunnit),
  statesList   = list(c("1","1","1","1")),
  exampleName = "Dunnit"
)
  
  

DunnitTwinTable <- CoherencesTable(DunnitBN,
  scenariosList = list(DunnitTwin),
  statesList   = list(c("1","1","1","1","1")),
  exampleName = "Dunnit"
)

DunnitTableSeparate <- rbind(DunnitNoTwinTable,DunnitTwinTable)

# 
# 
# DunnitTableSeparate
# 
 save(DunnitTableSeparate,file="calculations/RdataObjects/DunnitTableSeparate.Rda")
# 

# 
# load("calculations/RdataObjects/DunnitTable3.Rda")
# 
# DunnitTable3LaTeX <- tableLaTeX(DunnitTable3)
# 
# 
 DunnitLessTwin <- DunnitTableSeparate[1,] < DunnitTableSeparate[2,] 
# 
DunnitResultsSeparate <- as.data.frame(DunnitLessTwin)
# 
rownames(DunnitResultsSeparate) <- c("Dunnit: Dunnit$<$Twin")
# 
# #DunnitResults2
# 
# 
# 
 save(DunnitResultsSeparate,file="calculations/RdataObjects/DunnitResultsSeparate.Rda")

 
load("calculations/RdataObjects/DunnitResults3.Rda")


#DunnitResults3

DunnitResults3LaTeX <- tableLaTeX(DunnitTable3)







#OUTDATED


# 
# 
# # DunnitTable <- CoherencesTable(DunnitBN,
# #                               scenariosList = list(Dunnit,DunnitTwin),
# #                               statesList   = list(c("1","1","1","1"),c("1","1","1","1","1")),
# #                               exampleName = "Dunnit"
# # )
# # 
# # 
# # save(DunnitTable,file="calculations/RdataObjects/DunnitTable.Rda")
# 
# load("calculations/RdataObjects/DunnitTable.Rda")
# 
# DunnitTable
# 
# #structuredCoherence(DunnitBN,Dunnit,rep("1",4))
# #structuredCoherence2(DunnitBN,Dunnit,rep("1",4))
# 
# #structuredCoherence(DunnitBN,DunnitTwin,rep("1",5))
# #structuredCoherence2(DunnitBN,DunnitTwin,rep("1",5))
# 
# DunnitTableLaTeX <- tableLaTeX(DunnitTable)
# #DUnnitTableLaTeX
# 
# DunnitLessTwin <- DunnitTable[1,] < DunnitTable[2,] 
# 
# DunnitResults <- as.data.frame(DunnitLessTwin)
# 
# rownames(DunnitResults) <- c("Dunnit: Dunnit$<$Twin")
# 
# 
# #save(DunnitResults,file="calculations/RdataObjects/DunnitResults.Rda")
# 
# load("calculations/RdataObjects/DunnitResults.Rda")
# 
# 
# #DunnitResults
# 
# DunnitResultsLaTeX <- tableLaTeX(DunnitTable)
# 
# 




#OUTDATED
# 
# 
# 
# #___________________
# #RAcoherence
# 
# 
# DunnitRA <- RAcoherenceForBNs(BN = DunnitBN, narrationNodes = c("M","G","W","I"),states = c("1","1","1","1"))
# 
# #notice RA coherence of 0.004181402
# 
# 
# DunnitTwinRA <- RAcoherenceForBNs(BN = DunnitBN, narrationNodes = c("M","T","G","W","I"),states = c("1","1","1","1","1"))
# 
# #interestingly, the coherence goes down: 0.002574031
# 
# #perhaps this is because being a twin is unlikely and doesn't support the scenario; think about the z values
# 
# #___________________
# #Fitelson coherence
# 
# #We assume the disjoint combinations of nodes are logically independent
# 
# 
# DunnitFitelson <- FitelsonCoherenceForBNs(BN =  DunnitBN, narrationNodes = c("M","G","W","I"),
#                                           states = rep("1",4))
# 
# #DunnitFitelson
# 
# #Fitelson coherence of 0.1689506
# 
# #Now with the twin
# # watch out, my computation took almost 100 seconds
# 
# DunnitTwinFitelson <- FitelsonCoherenceForBNs(BN =  DunnitBN, narrationNodes = c("M","T","G","W","I"),  states = rep("1",5))
# 
# #DunnitTwinFitelson
# 
# 
# #interestingly: 0.3852718
# 
# #_______________
# # Roche for Dunnit
# 
# DunnitRoche <- RocheCoherenceForBNs(BN =  DunnitBN, narrationNodes = c("M","G","W","I"), states = rep("1",4))
# 
# #DunnitRoche
# 
# #Roche coherence of 0.168863, very much like Fitelson
# 
# #Now with the twin
# # watch out, my computation took almost 100 seconds
# 
# #this takes time!
# DunnitTwinRoche <- RocheCoherenceForBNs(BN =  DunnitBN, narrationNodes = c("M","T","G","W","I"),  states = rep("1",5))
# 
# 
# #DunnitTwinRoche
# #note the drop in coherence
# 
# #_______________
# #Shogenji for Dunnit
# 
# 
# DunnitShogenji <- ShogenjiCoherenceForBNs(
#   BN =  DunnitBN, narrationNodes = c("M","G","W","I"),  states = rep("1",4)
# )
# #without twin: 4.29
# 
# 
# DunnitTwinShogenji <- ShogenjiCoherenceForBNs(
#   BN =  DunnitBN, narrationNodes = c("M","T","G","W","I"),  states = rep("1",5)
# )
# #with twin: 73.83
# 
# ##______________
# #Olsson for Dunnit
# 
# DunnitOlsson <- OlssonCoherenceForBNs(
#   BN =  DunnitBN, narrationNodes = c("M","G","W","I"),  states = rep("1",4)
# )
# 
# DunnitTwinOlsson <- OlssonCoherenceForBNs(
#   BN =  DunnitBN, narrationNodes = c("M","T","G","W","I"),  states = rep("1",5)
# )
# 
# #_________________
# #Douven & Meijs 
# 
# DunnitDouvenMeijs <- DouvenMeijsCoherenceForBNs(
#   BN =  DunnitBN, narrationNodes = c("M","G","W","I"),  states = rep("1",4)
# )
# 
# 
# DunnitTwinDouvenMeijs <- DouvenMeijsCoherenceForBNs(
#   BN =  DunnitBN, narrationNodes = c("M","T","G","W","I"),  states = rep("1",5)
# )
# 
# #___________________
# 
# #Tables
# 
# DunnitTRA <- c(DunnitRA[[3]],DunnitTwinRA[[3]])
# 
# rownamesDunnit <- c("Dunnit","DunnitTwin")
# 
# DunnitTShogenji <- c(DunnitShogenji[[3]],DunnitTwinShogenji[[3]])
# 
# DunnitTOlsson <- c(DunnitOlsson[[3]],DunnitTwinOlsson[[3]])
# 
# DunnitTDouvenMeijs <- c(DunnitDouvenMeijs[[3]],DunnitTwinDouvenMeijs[[3]])
# 
# DunnitTRoche <- c(DunnitRoche[[3]],DunnitTwinRoche[[3]])
# 
# DunnitTFitelson <- c(DunnitFitelson[[3]],DunnitTwinFitelson[[3]])
# 
# DunnitTable <- data.frame(
#   Fitelson = DunnitTFitelson,
#   "DouvenMeijs" = DunnitTDouvenMeijs,
#   Olsson = DunnitTOlsson,
#   RA = DunnitTRA,
#   Roche = DunnitTRoche,
#   Shogenji = DunnitTShogenji)
# 
# 
# rownames(DunnitTable) <- rownamesDunnit
# 
# 
# DunnitTable
# 
# 
# DunnitLessTwin <- DunnitTable[1,] < DunnitTable[2,] 
# 
# DunnitResults <- as.data.frame(DunnitLessTwin)
# 
# 
# rownames(DunnitResults) <- c("Dunnit: Dunnit$<$Twin")
# 
# 
# 
# #DunnitResults
# 
