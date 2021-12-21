#with three nodes  

JapNarr <- c("J","O")


japThreeTable1 <- CoherencesTable(japThreeBN1,
                                        scenariosList = list(JapNarr),
                                        statesList   = list(c("1","1")),
                                        exampleName = "Japanese Swords, Three nodes 1"
)

japThreeTable1

japThreeTable2 <- CoherencesTable(japThreeBN2,
                                  scenariosList = list(JapNarr),
                                  statesList   = list(c("1","1")),
                                  exampleName = "Japanese Swords, Three nodes 2"
)

japThreeTable2

japThreeTable3 <- CoherencesTable(japThreeBN3,
                                  scenariosList = list(JapNarr),
                                  statesList   = list(c("1","1")),
                                  exampleName = "Japanese Swords, Three nodes 3"
)


japThreeTable3









#______

J <- c("J","O")
# #
# #
  BN <- Jap1BN
  JapaneseSwordsTableA <- CoherencesTable(Jap1BN,
                                  scenariosList = list(J),
                                  statesList   = list(c("1","1")),
                                 exampleName = "Japanese Swords 1"
 )
# #
# #
  BN <- Jap2BN
 JapaneseSwordsTableB <- CoherencesTable(Jap2BN,
                                         scenariosList = list(J),
                                         statesList   = list(c("1","1")),
                                         exampleName = "Japanese Swords 2"
 )
# #
   BN <- Jap3BN
  JapaneseSwordsTableC <- CoherencesTable(Jap3BN,
                                         scenariosList = list(J),
                                         statesList   = list(c("1","1")),
                                         exampleName = "Japanese Swords 3"
 )
# #
# #  #
# #  #
JapaneseSwordsSeparateTable <- rbind(JapaneseSwordsTableA,JapaneseSwordsTableB,JapaneseSwordsTableC)
# #  #
# #  #
save(JapaneseSwordsSeparateTable,file="calculations/RdataObjects/JapaneseSwordsSeparateTable.Rda")


#load("calculations/RdataObjects/JapaneseSwordsTable3.Rda")
 

#JapaneseSwordsTable3LaTeX <- tableLaTeX(JapaneseSwordsTable3)

# # # 
 JO2greaterJO1 <- JapaneseSwordsSeparateTable[2,] > JapaneseSwordsSeparateTable[1,] 
# # #JO2greaterJO1
# # # 
 JO2greaterJO3 <- JapaneseSwordsSeparateTable[2,] > JapaneseSwordsSeparateTable[3,] 
# # #JO2greaterJO1
# # # 
# # # 
 JapaneseSwordsSeparateResults <- as.data.frame(rbind(JO2greaterJO1,JO2greaterJO3))
# # # 
# # # 
 rownames(JapaneseSwordsSeparateResults) <- c("Swords: JO2$>$JO1","Swords: JO2$>$JO3")
# # # 
# # 
# 
# JapaneseSwordsSeparateResults
# 
 save(JapaneseSwordsSeparateResults,file="calculations/RdataObjects/JapaneseSwordsSeparateResults.Rda")

load("calculations/RdataObjects/JapaneseSwordsResults3.Rda")



JapaneseSwordsResults2LaTeX <- tableLaTeX(JapaneseSwordsResults2)
#JapaneseSwordsResultsLaTeX



 


# OUTDATED
# 
# 
# # JapaneseSwordsTable1 <- CoherencesTable(Jap1BN,
# #                                 scenariosList = list(J),
# #                                 statesList   = list(c("1","1")),
# #                                 exampleName = "Japanese Swords 1"
# # )
# # 
# # 
# # BN <- Jap2BN
# # JapaneseSwordsTable2 <- CoherencesTable(Jap2BN,
# #                                         scenariosList = list(J),
# #                                         statesList   = list(c("1","1")),
# #                                         exampleName = "Japanese Swords 2"
# # )
# # 
# # BN <- Jap3BN
# # JapaneseSwordsTable3 <- CoherencesTable(Jap3BN,
# #                                         scenariosList = list(J),
# #                                         statesList   = list(c("1","1")),
# #                                         exampleName = "Japanese Swords 3"
# # )
# # 
# # 
# # 
# # JapaneseSwordsTable <- rbind(JapaneseSwordsTable1,JapaneseSwordsTable2,JapaneseSwordsTable3)
# # 
# # 
# # save(JapaneseSwordsTable,file="calculations/RdataObjects/JapaneseSwordsTable.Rda")
# 
# load("calculations/RdataObjects/JapaneseSwordsTable.Rda")
# 
# 
# #JapaneseSwordsTable
# 
# 
# JapaneseSwordsTableLaTeX <- tableLaTeX(JapaneseSwordsTable)
# 
# # 
# # JO2greaterJO1 <- JapaneseSwordsTable[2,] > JapaneseSwordsTable[1,] 
# # #JO2greaterJO1
# # 
# # JO2greaterJO3 <- JapaneseSwordsTable[2,] > JapaneseSwordsTable[3,] 
# # #JO2greaterJO1
# # 
# # 
# # JapaneseSwordsResults <- as.data.frame(rbind(JO2greaterJO1,JO2greaterJO3))
# # 
# # 
# # rownames(JapaneseSwordsResults) <- c("Swords: JO2$>$JO1","Swords: JO2$>$JO3")
# # 
# # 
# # save(JapaneseSwordsResults,file="calculations/RdataObjects/JapaneseSwordsResults.Rda")
# 
# load("calculations/RdataObjects/JapaneseSwordsResults.Rda")
# 
# 
# 
# 
# #JapaneseSwordsResults
# 
# JapaneseSwordsResultsLaTeX <- tableLaTeX(JapaneseSwordsResults)
# #JapaneseSwordsResultsLaTeX
# 
