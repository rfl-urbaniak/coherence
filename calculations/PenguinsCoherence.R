BGP <- c("B","G","P")
BG <- c("B","G")
BP <- c("B","P")

#Coherences
penguinsTableBGP <- CoherencesTable(BirdBNbgp, scenariosList = list(BGP), statesList   = list(c("1","1","1")),exampleName = "Penguins")

penguinsTableBG <- CoherencesTable(BirdBNbg, scenariosList = list(BG), statesList   = list(c("1","1")),exampleName = "Penguins")

penguinsTableBP <- CoherencesTable(BirdBNbp, scenariosList = list(BP), statesList   = list(c("1","1")),exampleName = "Penguins")

penguinsTable <- rbind(penguinsTableBGP,penguinsTableBG,penguinsTableBP)


penguinsTable

save(penguinsTable,file="calculations/RdataObjects/penguinsTablePaper.Rda")


# Desiderata

load("calculations/RdataObjects/penguinsTablePaper.Rda")

penguinsTable

BGlessBGP <- penguinsTable[1,] > penguinsTable[2,] 
#BPatleastBGP <- round(penguinsTable[3,],4) >= round(penguinsTable[1,],4)
BPapproxBGP <- abs(penguinsTable[3,] - penguinsTable[1,]) <= 0.05

penguinsResults <- as.data.frame(rbind(BGlessBGP,BPapproxBGP))
rownames(penguinsResults) <- c("Penguins: BG$<$BGP","Penguins: BP$\\approx$ BGP")

penguinsResults

save(penguinsResults,file="calculations/RdataObjects/penguinsResultsPaper.Rda")

# 
# 
# # 
# # penguinsResults3
# # # 
# # # 
# # # 
# # save(penguinsResults3,file="calculations/RdataObjects/penguinsResults3.Rda")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # #let's assume no logical conenction for Fitelson, then we can apply the general format
# # # 
# #  BGP <- c("B","G","P")
# #  BG <- c("B","G")
# #  BP <- c("B","P")
# # # # 
# # # 
# # 
# # # # 
# # # # 
# # # #BN <- BirdBN
# # # # 
# # # # 
# #  BN <- BirdBN3
# # # 
# # penguinsTable3 <- CoherencesTable(BirdBN3,
# #                                 scenariosList = list(BGP,BG,BP),
# #                                 statesList   = list(c("1","1","1"),c("1","1"), c("1","1")),
# #                                 exampleName = "Penguins"
# # )
# # #  
# # #  
# # # penguinsTable2
# # 
# # #  
# # save(penguinsTable3,file="calculations/RdataObjects/penguinsTable3.Rda")
# 
# #load("calculations/RdataObjects/penguinsTable3.Rda")
# 
# #penguinsTable3
# 
# 
# 
# 
# 
# 
# 
# 
# rownamesPenguins <- c("BGP","BG","BP")
# 
# 
# 
#  # 
# # penguinsTable <- CoherencesTable(BirdBN3,
# #                                scenariosList = list(BGP,BG,BP),
# #                                statesList   = list(c("1","1","1"),c("1","1"), c("1","1")),
# #                                exampleName = "Penguins"
# # )
# # 
# # 
# 
# # rownames(penguinsTable) <- rownamesPenguins
# # 
# # 
# # 
# # save(penguinsTable,file="calculations/RdataObjects/penguinsTable.Rda")
# 
# #load("calculations/RdataObjects/penguinsTable2.Rda")
# 
# penguinsTable2LaTeX <- tableLaTeX(penguinsTable2)
# 
# 
# # # 
# # # 
# # BGlessBGP <- penguinsTable3[1,] > penguinsTable3[2,] 
# # BPatleastBGP <- penguinsTable3[3,] >= penguinsTable3[1,]
# # BPapproxBGP <- abs(penguinsTable3[3,] - penguinsTable3[1,]) <= 0.05
# # 
# # 
# # # # 
# #  penguinsResults3 <- as.data.frame(rbind(BGlessBGP,#BPatleastBGP,
# #                                          BPapproxBGP))
# # 
# # penguinsResults3
# #  # # 
# # # # 
# # rownames(penguinsResults3) <- c("Penguins: BG$<$BGP",#"Penguins: BP$\\geq$ BGP",
# #                                 "Penguins: BP$\\approx$ BGP")
# # 
# # penguinsResults3
# # # 
# # # 
# # # 
# # save(penguinsResults3,file="calculations/RdataObjects/penguinsResults3.Rda")
# 
# #load("calculations/RdataObjects/penguinsResults3.Rda")
# 
# 
# 
# #penguinsResults3
# 
# 
# penguinsResults3LaTeX <- tableLaTeX(penguinsResults3)
# 
# 
# 
# 
# 
# 
# #OUTDATED
# 
# # 
# # 
# # 
# # #structuredCoherence(BirdBN3,BGP, rep("1",3))
# # #structuredCoherence2(BirdBN3,BGP, rep("1",3))
# # 
# # 
# # #structuredCoherence2(BirdBN3,BG, rep("1",2))
# # 
# # #structuredCoherence2(BirdBN3,BP, rep("1",2))
# # 
# # 
# # 
# # #BN <- BirdBN3
# # 
# # #graphviz.plot(BN)
# # 
# # 
# # #structuredCoherence(BirdBN3,BGP, rep("1",3))
# # 
# # #structuredCoherence(BirdBN3,BG, rep("1",2))
# # 
# # #structuredCoherence(BirdBN3,BP,rep("1",2))
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # penguinsTableLaTeX <- tableLaTeX(penguinsTable)
# # 
# # 
# # 
# # 
# # 
# # # 
# # # 
# # # BGlessBGP <- penguinsTable[1,] > penguinsTable[2,] 
# # # BPatleastBGP <- penguinsTable[3,] >= penguinsTable[1,]
# # # BPapproxBGP <- abs(penguinsTable[3,] - penguinsTable[1,]) <= 0.05
# # # 
# # # penguinsResults <- as.data.frame(rbind(BGlessBGP,BPatleastBGP,BPapproxBGP))
# # # 
# # # 
# # # rownames(penguinsResults) <- c("Penguins: BG$<$BGP","Penguins: BP$\\geq$ BGP","Penguins: BP$\\approx$ BGP")
# # # 
# # # 
# # # 
# # # save(penguinsResults,file="calculations/RdataObjects/penguinsResults.Rda")
# # 
# # load("calculations/RdataObjects/penguinsResults.Rda")
# # 
# # 
# # 
# # #penguinsResults
# # 
# # 
# # penguinsResultsLaTeX <- tableLaTeX(penguinsResults)
# # 
# # 
# # 
# # 
# # #structuredCoherence(BirdBN, BGP, c("1","1","1"))
# # 
# # #structuredCoherence(BirdBN, BG, c("1","1"))
# # 
# # #structuredCoherence(BirdBN, BP, c("1","1"))
# # 
# # 
# # # 
# # # ##Outdated
# # # 
# # # 
# # # 
# # # #_____________________
# # # ####RA for Penguins
# # # 
# # # 
# # # #Now let's see what happens to coherences
# # # BGPRA <- RAcoherenceForBNs(BN = BirdBN, narrationNodes = c("B","G","P"),states = c("1","1","1"))
# # # 
# # # #BGPRA
# # # 
# # # #RAcoherenceForBNs(BN = BirdBN, narrationNodes = c("B","G","P"),states = c("1","0","1"))
# # # 
# # # BGRA <- RAcoherenceForBNs(BN = BirdBN, narrationNodes = c("B","G"),states = c("1","1"))
# # # 
# # # ## Notice: the jump from grounded bird to grounded penguin bird is from 0.0101 to 0.34
# # # 
# # # #BGRA
# # # 
# # # BPRA <- RAcoherenceForBNs(BN = BirdBN, narrationNodes = c("B","P"),states = c("1","1"))
# # # 
# # # ## Notice the move from bird penguin to grounded penguin bird is from .505 to 0.34
# # # ## This is also a case in which adding a consequence of one of the nodes, which is unlikely given another node leads to a drop in coherence.
# # # 
# # # #_______________
# # # 
# # # ### Fitelson for Penguins
# # # 
# # # #__________________________________________
# # # #Let's test this on penguins
# # # 
# # # 
# # # BN <- BirdBN #rename for testing
# # # #graphviz.plot(BN)
# # # #graphviz.chart(BN,type="barprob")
# # # 
# # # #check BGP
# # # #narrationNodes <- c("B","G","P")
# # # #states <- c("1","1","1")
# # # 
# # # 
# # # #inspect the pairs (think both left to right and right to left)
# # # #write the logical states by hand
# # # #combinations <- DisjointPairs(narrationNodes)
# # # #combinations
# # # # 
# # # BirdStatusLR <- c("Ind","Ent","Ent","Ent","Ent","Ent")
# # # BirdStatusRL <- c("Ind","Ind","Ind","Ind","Ind","Ind")
# # # 
# # # 
# # # BGPFitelson <- FitelsonCoherenceForBNs(BN =  BirdBN, narrationNodes = c("B","G","P"),
# # #                                        states = rep("1",3), 
# # #                                        statusLR = BirdStatusLR, 
# # #                                        statusRL = BirdStatusRL 
# # # )
# # # 
# # # 
# # # 
# # # 
# # # 
# # # #Check BG, this should be low
# # # #narrationNodes <- c("B","G")
# # # #states <- c("1","1")
# # # 
# # # #combinations <- DisjointPairs(narrationNodes)
# # # #combinations
# # # # 
# # # #hey, that's easy! one row, independence both ways
# # # BirdStatusLR <- c("Ind")
# # # BirdStatusRL <- c("Ind")
# # # 
# # # BGFitelson <- FitelsonCoherenceForBNs(BN =  BirdBN, narrationNodes = c("B","G"),
# # #                                       states = rep("1",2), 
# # #                                       statusLR = BirdStatusLR, 
# # #                                       statusRL = BirdStatusRL 
# # )
# # 
# # #BGFitelson
# # 
# # 
# # #Now let's see BP, this should not be much lower than BGP
# # 
# # #narrationNodes <- c("B","P")
# # #states <- c("1","1")
# # 
# # #combinations <- DisjointPairs(narrationNodes)
# # #combinations
# # # 
# # #one row, P entails B, Ind in the other direction
# # BirdStatusLR <- c("Ent")
# # BirdStatusRL <- c("Ind")
# # 
# # BPFitelson <- FitelsonCoherenceForBNs(BN =  BirdBN, narrationNodes = c("B","P"),
# #                                       states = rep("1",2), 
# #                                       statusLR = BirdStatusLR, 
# #                                       statusRL = BirdStatusRL 
# # )
# # 
# # 
# # #________________________
# # #Roche for Penguins
# # #Shogenji for  Penguins
# # BPRoche <- RocheCoherenceForBNs(BN,narrationNodes =  c("B","P"), states = c("1","1"))
# # #BP coherence .255
# # 
# # 
# # 
# # 
# # BGPRoche <- RocheCoherenceForBNs(BN,narrationNodes =  c("B","P","G"), states = c("1","1","1"))
# # #BGPRoche
# # #note: adding information that grounded doesn't change the coherence: .255
# # 
# # 
# # BGRoche <- ShogenjiCoherenceForBNs(BN,narrationNodes =  c("B","G"), states = c("1","1"))
# # #BGRoche
# # #BG has much lower coherence than BP (0.04), so the jump obtained by adding P and obtaining .255 is quite high.
# # 
# # 
# # 
# # #____________________
# # #Shogenji for  Penguins
# # BPShogenji <- ShogenjiCoherenceForBNs(BN,narrationNodes =  c("B","P"), states = c("1","1"))
# # #BP coherence 2
# # 
# # BGPShogenji <- ShogenjiCoherenceForBNs(BN,narrationNodes =  c("B","P","G"), states = c("1","1","1"))
# # #note: adding information that grounded increases coherence to 4
# # 
# # BGShogenji <- ShogenjiCoherenceForBNs(BN,narrationNodes =  c("B","G"), states = c("1","1"))
# # #BG has much lower coherence than BP (0.04), so the jump obtained by adding P is quite high.
# # 
# # #______________________
# # #Olsson coherence for Penguins
# # 
# # BPOlsson <- OlssonCoherenceForBNs(BN,narrationNodes =  c("B","P"), states = c("1","1"))
# # #BP coherence 0.02
# # 
# # BGPOlsson <- OlssonCoherenceForBNs(BN,narrationNodes =  c("B","P","G"), states = c("1","1","1"))
# # #note: adding information that grounded decreases coherence: 0.0101
# # 
# # BGOlsson <- OlssonCoherenceForBNs(BN,narrationNodes =  c("B","G"), states = c("1","1"))
# # #BG has 0.0101, doesn't differ from BGP!
# # 
# # 
# # #______________________
# # #Douven & Meijs
# # 
# # BGPDouvenMeijs <- DouvenMeijsCoherenceForBNs(BN =  BirdBN, narrationNodes = c("B","G","P"), states = rep("1",3))
# # 
# # 
# # BGDouvenMeijs <- DouvenMeijsCoherenceForBNs(BN =  BirdBN, narrationNodes = c("B","G"), states = rep("1",2))
# # 
# # 
# # BPDouvenMeijs <- DouvenMeijsCoherenceForBNs(BN =  BirdBN, narrationNodes = c("B", "P"), states = rep("1",2))
# # 
# # #__________________________
# # #JOINT RESULTS TABLES
# # 
# # rownamesPenguins <- c("BGP","BG","BP")
# # 
# # penguinsRA <- c(BGPRA[[3]],BGRA[[3]],BPRA[[3]])
# # 
# # penguinsShogenji <- c(BGPShogenji[[3]],BGShogenji[[3]],BPShogenji[[3]])
# # 
# # penguinsOlsson <- c(BGPOlsson[[3]],BGOlsson[[3]],BPOlsson[[3]])
# # 
# # penguinsDouvenMeijs <- c(BGPDouvenMeijs[[3]],BGDouvenMeijs[[3]],BPDouvenMeijs[[3]])
# # 
# # penguinsRoche <- c(BGPRoche[[3]],BGRoche[[3]],BPRoche[[3]])
# # 
# # penguinsFitelson <- c(BGPFitelson[[3]],BGFitelson[[3]],BPFitelson[[3]])
# # 
# # 
# # 
# # 
# # 
# # 
# # penguinsTable <- data.frame(
# #                             Fitelson = penguinsFitelson,
# #                             "DouvenMeijs" = penguinsDouvenMeijs,
# #                             Olsson = penguinsOlsson,
# #                             RA = penguinsRA,
# #                             Roche = penguinsRoche,
# #                             Shogenji = penguinsShogenji)
# #                                         
# # 
# # rownames(penguinsTable) <- rownamesPenguins
# # 
# # 
# # 
# # #penguinsTable
# # 
# # BGlessBGP <- penguinsTable[1,] > penguinsTable[2,] 
# # BPatleastBGP <- penguinsTable[3,] >= penguinsTable[1,]
# # 
# # penguinsResults <- as.data.frame(rbind(BGlessBGP,BPatleastBGP))
# # 
# # 
# # rownames(penguinsResults) <- c("Penguins: BG$<$BGP", "Penguins: BP$\\geq$ BGP")
# # 
# # #penguinsResults
# 
