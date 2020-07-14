B <- c("J","P","G","R","D")


BN <- BeatlesBN
BeatlesTable <- CoherencesTable(BeatlesBN,
                                        scenariosList = list(B),
                                        statesList   = list(c("1","1","1","1","1")),
                                        exampleName = "Beatles"
)

#BeatlesTable


#structuredCoherence(BeatlesBN, B, rep("1",5) )



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
