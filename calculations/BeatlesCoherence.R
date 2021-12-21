 B <- c("J","P","G","R","D")
BN <- BeatlesBN
# 
BeatlesTable3 <- CoherencesTable(BeatlesBN,
                                        scenariosList = list(B),
                                        statesList   = list(c("1","1","1","1","1")),
                                        exampleName = "Beatles"
)

BeatlesTable3

#save(BeatlesTable3,file="calculations/RdataObjects/BeatlesTable3.Rda")

#save(BeatlesTable3,file="calculations/RdataObjects/BeatlesTable3.Rda")


#load("calculations/RdataObjects/BeatlesTable3.Rda")


#BeatlesTable2LaTeX <- tableLaTeX(BeatlesTable2)

#BeatlesTable3LaTeX <- tableLaTeX(BeatlesTable3)

# 
# BeatlesTable2
# 
# 
 minima <-  c(0, 0 ,0, 0, -1, -1, 0, -1, -1, -1, -1, -1)
 neutralPoints <- c(NA, NA ,1, 1, 0, 0, 0.5, 0, 0, 0, 0, 0)
# # 
# # # 
  BeatlesMinimal <- BeatlesTable3[1,] == minima
 BeatlesBelowNeutral <- BeatlesTable3[1,] < neutralPoints 
# # BeatlesIncoherent
# # BeatlesMinimal
# # 
# # # 
# # # 
 BeatlesResults3 <- as.data.frame(rbind(BeatlesBelowNeutral, BeatlesMinimal))
# # # 
# # # 
 rownames(BeatlesResults3) <- c("Beatles: below neutral", "Beatles: minimal")
# # 
BeatlesResults3
 save(BeatlesResults3,file="calculations/RdataObjects/BeatlesResults3.Rda")




#load("calculations/RdataObjects/BeatlesResults3.Rda")



BeatlesResults2LaTeX <- tableLaTeX(BeatlesResults2)
#BeatlesResultsLaTeX


#OUTDATED 
# 
# 
# 
# 
# # #graphviz.plot(BN)
# # BeatlesTable <- CoherencesTable(BeatlesBN,
# #                                         scenariosList = list(B),
# #                                         statesList   = list(c("1","1","1","1","1")),
# #                                         exampleName = "Beatles"
# # )
# # save(BeatlesTable,file="calculations/RdataObjects/BeatlesTable.Rda")
# 
# #load("calculations/RdataObjects/BeatlesTable.Rda")
# 
# 
# #BeatlesTable
# 
# 
# 
# #structuredCoherence(BeatlesBN, B, rep("1",5) )
# #structuredCoherence2(BeatlesBN, B, rep("1",5) )
# 
# #B2 <- c("J","P","R","D")
# #structuredCoherence(BeatlesBN, B2, rep("1",5) )
# #B3 <- c("J","P","R","D")
# #structuredCoherence(BeatlesBN, B3, c("0","0","1","1"))
# 
# 
# 
# # 
# # neutralPoints <- c(NA, NA ,1, 1, 0, 0, 0.5, 0, 0, 0, 0, 0)
# # #rbind(neutralPoints,names(BeatlesTable))
# # 
# # BeatlesIncoherent <- BeatlesTable[1,] < neutralPoints 
# # #BeatlesIncoherent
# # 
# # 
# # BeatlesResults <- as.data.frame(rbind(BeatlesIncoherent))
# # 
# # 
# # rownames(BeatlesResults) <- c("Beatles: incoherent")
# # 
# # BeatlesResults
# # save(BeatlesResults,file="calculations/RdataObjects/BeatlesResults.Rda")
# 
# 
# load("calculations/RdataObjects/BeatlesResults.Rda")
# 
# 
# 
# BeatlesResultsLaTeX <- tableLaTeX(BeatlesResults)
# #BeatlesResultsLaTeX
