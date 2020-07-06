J <- c("J","O")


JapaneseSwordsTable1 <- CoherencesTable(Jap1BN,
                                scenariosList = list(J),
                                statesList   = list(c("1","1")),
                                exampleName = "Japanese Swords 1"
)


JapaneseSwordsTable2 <- CoherencesTable(Jap2BN,
                                        scenariosList = list(J),
                                        statesList   = list(c("1","1")),
                                        exampleName = "Japanese Swords 2"
)


JapaneseSwordsTable3 <- CoherencesTable(Jap3BN,
                                        scenariosList = list(J),
                                        statesList   = list(c("1","1")),
                                        exampleName = "Japanese Swords 3"
)



JapaneseSwordsTable <- rbind(JapaneseSwordsTable1,JapaneseSwordsTable2,JapaneseSwordsTable3)


JapaneseSwordsTable


JapaneseSwordsTableLaTeX <- tableLaTeX(JapaneseSwordsTable)


JO2greaterJO1 <- JapaneseSwordsTable[2,] > JapaneseSwordsTable[1,] 
#JO2greaterJO1

JO2greaterJO3 <- JapaneseSwordsTable[2,] > JapaneseSwordsTable[3,] 
#JO2greaterJO1


JapaneseSwordsResults <- as.data.frame(rbind(JO2greaterJO1,JO2greaterJO3))


rownames(JapaneseSwordsResults) <- c("Swords: JO2$>$JO1","Swords: JO2$>$JO3")
#JapaneseSwordsResults

JapaneseSwordsResultsLaTeX <- tableLaTeX(JapaneseSwordsResults)
#JapaneseSwordsResultsLaTeX

