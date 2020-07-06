A <- c("W1","W2")
B <- c("W3","W4")
C <- c("W4","W5")


witnessTable <- CoherencesTable(WitnessBN,
                              scenariosList = list(A,B,C),
                              statesList   = list(c("1","1"),c("1","1"),c("1","1")),
                              exampleName = "Witness"
)

witnessTable


witnessTableLaTeX <- tableLaTeX(witnessTable)


W1W2greaterW3W4 <- witnessTable[1,] > witnessTable[2,] 
#W1W2greaterW3W4

W4W5greaterW3W4 <- witnessTable[3,] > witnessTable[2,] 
#W4W5greaterW3W4


witnessResults <- as.data.frame(rbind(W1W2greaterW3W4,W4W5greaterW3W4))


rownames(witnessResults) <- c("Witness: W$_1$W$_2>$W$_3$W$_4$","Witness: W$_4$W$_5>$W$_3$W$_4$")
#witnessResults

witnessResultsLaTeX <- tableLaTeX(witnessResults)
#witnessResultsLaTeX


