B <- c("J","P","G","R","D")


BeatlesTable <- CoherencesTable(BeatlesBN,
                                        scenariosList = list(B),
                                        statesList   = list(c("1","1","1","1","1")),
                                        exampleName = "Beatles"
)

#BeatlesTable


BeatlesTable <- rbind(BeatlesTable)


BeatlesTable

BeatlesTableLaTeX <- tableLaTeX(BeatlesTable)


neutralPoints <- c(0,0 ,NA, .5, .5, 1)

BeatlesIncoherent <- BeatlesTable[1,] < neutralPoints 
#BeatlesIncoherent


BeatlesResults <- as.data.frame(rbind(BeatlesIncoherent))

#BeatlesResults

rownames(BeatlesResults) <- c("Beatles: incoherent")

BeatlesResultsLaTeX <- tableLaTeX(BeatlesResults)
#BeatlesResultsLaTeX
