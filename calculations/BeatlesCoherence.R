B <- c("J","P","G","R","D")


BN <- BeatlesBN
#graphviz.plot(BN)
BeatlesTable <- CoherencesTable(BeatlesBN,
                                        scenariosList = list(B),
                                        statesList   = list(c("1","1","1","1","1")),
                                        exampleName = "Beatles"
)

#BeatlesTable


#structuredCoherence(BeatlesBN, B, rep("1",5) )


#B2 <- c("J","P","R","D")
#structuredCoherence(BeatlesBN, B2, rep("1",5) )



#B3 <- c("J","P","R","D")
#structuredCoherence(BeatlesBN, B3, c("0","0","1","1"))




BeatlesTableLaTeX <- tableLaTeX(BeatlesTable)


neutralPoints <- c(NA, NA ,1, 1, 0, 0, 0.5, 0, 0, 0, 0, 0)
#rbind(neutralPoints,names(BeatlesTable))

BeatlesIncoherent <- BeatlesTable[1,] < neutralPoints 
#BeatlesIncoherent


BeatlesResults <- as.data.frame(rbind(BeatlesIncoherent))


rownames(BeatlesResults) <- c("Beatles: incoherent")

#BeatlesResults


BeatlesResultsLaTeX <- tableLaTeX(BeatlesResults)
#BeatlesResultsLaTeX
