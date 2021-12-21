A <- c("W1","W2")
B <- c("W3","W4")
C <- c("W4","W5")
# # 



#CALCULATIONS FOR THREE-NODE NARATED BNs


structuredCoherenceUpdated(W12BN,A,c("1","1"))

W1calculations <- structuredCoherenceNarration(W12BN,A,c("1","1"))[[1]][[1]][[2]]

save(W1calculations, file = "W1calculations.Rda")



W12Table <- CoherencesTableNarr(W12BN,
                           scenariosList = list(A),
                           statesList   = list(c("1","1")),
                           exampleName = "Witness"
)



W34Table <- CoherencesTableNarr(W34BN,
                                scenariosList = list(B),
                                statesList   = list(c("1","1")),
                                exampleName = "Witness"
)

W45Table <- CoherencesTableNarr(W45BN,
                                scenariosList = list(C),
                                statesList   = list(c("1","1")),
                                exampleName = "Witness"
)



WNarrTable <- as.data.frame(rbind(W12Table, 
                                      W34Table,
                                      W45Table
))

save(WNarrTable,file = "calculations/RdataObjects/WNarrTable.Rda")






W1W2greaterW3W4 <- WNarrTable[1,] > WNarrTable[2,] 
# #W1W2greaterW3W4
# # 
W4W5greaterW3W4 <- WNarrTable[3,] > WNarrTable[2,] 
# # #W4W5greaterW3W4
# # 
# # 
WNarrResults <- as.data.frame(rbind(W1W2greaterW3W4,W4W5greaterW3W4))
# # 
# # 
rownames(WNarrResults) <- c("Witness: W$_1$W$_2>$W$_3$W$_4$","Witness: W$_4$W$_5>$W$_3$W$_4$")
# # 
# # 
# # 

save(WNarrResults,file="calculations/RdataObjects/WNarrResults.Rda")
# 
#WNarrResults

#WNarrTable














#CALCULATIONS WITH SEPARATE BNS FOR WITNESSES
W1Table <- CoherencesTable(W1BN,
                                       scenariosList = list(A),
                                       statesList   = list(c("1","1")),
                                       exampleName = "Witness"
)



W3Table <- CoherencesTable(W3BN,
                           scenariosList = list(B),
                           statesList   = list(c("1","1")),
                           exampleName = "Witness"
)



W4Table <- CoherencesTable(W4BN,
                                       scenariosList = list(C),
                                       statesList   = list(c("1","1")),
                                       exampleName = "Witness"
)



WSeparateTable <- as.data.frame(rbind(W1Table, 
                                        W3Table,
                                        W4Table
))


#WSeparateTable




save(WSeparateTable, file = "calculations/RdataObjects/WSeparateTable.Rda")





W1W2greaterW3W4 <- WSeparateTable[1,] > WSeparateTable[2,] 
# #W1W2greaterW3W4
# # 
W4W5greaterW3W4 <- WSeparateTable[3,] > WSeparateTable[2,] 
# # #W4W5greaterW3W4
# # 
# # 
WSeparateResults <- as.data.frame(rbind(W1W2greaterW3W4,W4W5greaterW3W4))
# # 
# # 
rownames(WSeparateResults) <- c("Witness: W$_1$W$_2>$W$_3$W$_4$","Witness: W$_4$W$_5>$W$_3$W$_4$")
# # 
# # 
# # 

save(WSeparateResults,file="calculations/RdataObjects/WSeparateResults.Rda")
# 
#WSeparateResults






# # #graphviz.plot(BN)
witnessTable2 <- CoherencesTable(WitnessBN,
                                scenariosList = list(A,B,C),
                                statesList   = list(c("1","1"),c("1","1"),c("1","1")),
                                exampleName = "Witness"
  )


witnessTableWhole1 <- CoherencesTable(WitnessBN1,
                                 scenariosList = list(A),
                                 statesList   = list(c("1","1")),
                                 exampleName = "Witness"
)

witnessTableWhole3 <- CoherencesTable(WitnessBN3,
                                      scenariosList = list(B),
                                      statesList   = list(c("1","1")),
                                      exampleName = "Witness"
)

witnessTableWhole4 <- CoherencesTable(WitnessBN4,
                                      scenariosList = list(C),
                                      statesList   = list(c("1","1")),
                                      exampleName = "Witness"
)



witnessWholeTable <- as.data.frame(rbind(witnessTableWhole1, 
                           witnessTableWhole3,
                           witnessTableWhole4
))


save(witnessWholeTable, file = "calculations/RdataObjects/witnessWholeTable.Rda")


W1W2greaterW3W4 <- witnessWholeTable[1,] > witnessWholeTable[2,] 
# #W1W2greaterW3W4
# # 
W4W5greaterW3W4 <- witnessWholeTable[3,] > witnessWholeTable[2,] 
# # #W4W5greaterW3W4
# # 
# # 
witnessWholeResults <- as.data.frame(rbind(W1W2greaterW3W4,W4W5greaterW3W4))
# # 
# # 
rownames(witnessWholeResults) <- c("Witness: W$_1$W$_2>$W$_3$W$_4$","Witness: W$_4$W$_5>$W$_3$W$_4$")
# # 
# # 
# # 
save(witnessWholeResults,file="calculations/RdataObjects/witnessWholeResults.Rda")
# 
#witnessWholeResults



#  
#  
#witnessTable2
#  
save(witnessTable2,file="calculations/RdataObjects/witnessTable2.Rda")
#  
load("calculations/RdataObjects/witnessTable2.Rda")
#witnessTable2
 
 
witnessTable2LaTeX <- tableLaTeX(witnessTable2)
 

 
 # 
 W1W2greaterW3W4 <- witnessTable2[1,] > witnessTable2[2,] 
 # #W1W2greaterW3W4
 # # 
 W4W5greaterW3W4 <- witnessTable2[3,] > witnessTable2[2,] 
 # # #W4W5greaterW3W4
 # # 
 # # 
 witnessResults2 <- as.data.frame(rbind(W1W2greaterW3W4,W4W5greaterW3W4))
 # # 
 # # 
 rownames(witnessResults2) <- c("Witness: W$_1$W$_2>$W$_3$W$_4$","Witness: W$_4$W$_5>$W$_3$W$_4$")
 # # 
 # # 
 # # 
 save(witnessResults2,file="calculations/RdataObjects/witnessResults2.Rda")
 # 
 # 
 load("calculations/RdataObjects/witnessResults2.Rda")
 
 
 
# witnessResults2
 
 witnessResults2LaTeX <- tableLaTeX(witnessResults2)
 #witnessResultsLaTeX
 
 #____________________________________
 ### For updated BNs
 witnessTableW1 <- CoherencesTable(WitnessBN1,
                                  scenariosList = list(A,B,C),
                                  statesList   = list(c("1","1"),c("1","1"),c("1","1")),
                                  exampleName = "Witness"
 )

#___________________________________________________________ 
##Update on complete scenarios:
 
 witnessTableWhole12 <- CoherencesTable(WitnessBN12,
                                       scenariosList = list(A),
                                       statesList   = list(c("1","1")),
                                       exampleName = "Witness"
 )
 
 witnessTableWhole34 <- CoherencesTable(WitnessBN34,
                                       scenariosList = list(B),
                                       statesList   = list(c("1","1")),
                                       exampleName = "Witness"
 )
 
 witnessTableWhole45 <- CoherencesTable(WitnessBN45,
                                       scenariosList = list(C),
                                       statesList   = list(c("1","1")),
                                       exampleName = "Witness"
 )
 
 
 
 witnessFullTable <- as.data.frame(rbind(witnessTableWhole12, 
                                          witnessTableWhole34,
                                          witnessTableWhole45
 ))
 
witnessFullTable
 
 
 save(witnessWholeTable, file = "calculations/RdataObjects/witnessWholeTable.Rda")
 
 
 
 
 
 
 
 
 
 #____calculations for subnetworks
 
 
 #W1W2
 
 A <- c("W1","W2")
 B <- c("W3","W4")
 C <- c("W4","W5")
 # # 
 # # 
BN <- W1W2BN

W1W2BNupdated <- updateBN(W1W2BN,Nodes = c("W1","W2"), States = c("1","1"))

W1W2BN
W1W2BNupdated
Z(posterior = 0.98,prior = 0.16)


 # # 
 # # #graphviz.plot(BN)
 witnessTableW1W2 <- CoherencesTable(W1W2BNupdated,
                                  scenariosList = list(A),
                                  statesList   = list(c("1","1")),
                                  exampleName = "Witness")

 
witnessTableW1W2
 
# W3W4


W3W4BNupdated <- updateBN(W3W4BN,Nodes = c("W1","W2"), States = c("1","1"))

W3W4BNupdated
 
BN <- W3W4BN
 # # 
 # # #graphviz.plot(BN)
 witnessTableW3W4 <- CoherencesTable(W3W4BNupdated,
                                     scenariosList = list(B),
                                     statesList   = list(c("1","1")),
                                     exampleName = "Witness")
 
witnessTableW3W4

 
#W4W5


W4W5BNupdated <- updateBN(W4W5BN,Nodes = c("W1","W2"), States = c("1","1"))



 
BN <- W4W5BNW4
 # # 
 # # #graphviz.plot(BN)
 witnessTableW4W5 <- CoherencesTable(W4W5BNupdated,
                                     scenariosList = list(C),
                                     statesList   = list(c("1","1")),
                                     exampleName = "Witness")
 
witnessTableW4W5
 


witnessTablePaper <- rbind(witnessTableW1W2,witnessTableW3W4,witnessTableW4W5)

witnessTablePaper 

save(witnessTablePaper,file="calculations/RdataObjects/witnessTablePaper.Rda")



# 
W1W2greaterW3W4 <- witnessTablePaper[1,] > witnessTablePaper[2,] 
# #W1W2greaterW3W4
# # 
W4W5greaterW3W4 <- witnessTablePaper[3,] > witnessTablePaper[2,] 
# # #W4W5greaterW3W4
# # 
# # 
witnessResultsPaper <- as.data.frame(rbind(W1W2greaterW3W4,W4W5greaterW3W4))
# # 
# # 
rownames(witnessResultsPaper) <- c("Witness: W$_1$W$_2>$W$_3$W$_4$","Witness: W$_4$W$_5>$W$_3$W$_4$")
# # 

witnessResultsPaper
# # 
# # 
save(witnessResultsPaper,file="calculations/RdataObjects/witnessResultsPaper.Rda")
# 
# 




 #-------------------------------------
 W1W2JN <- compile(as.grain(W1W2BN))
 
 W1W2JNW1 <- setEvidence(W1W2JN,nodes = "W1",states = "1")
 
 W1W2BNW1 <- as.bn.fit(W1W2JNW1,including.evidence = TRUE)
 
 BN <- W1W2BNW1
 # # 
 # # #graphviz.plot(BN)
 
 structuredCoherenceUpdated( BN = W1W2BNW1,narrationNodes = c("W1","W2"), states = c("1","1"))
 
 
 
 
 W3W4JN <- compile(as.grain(W3W4BN))
 
 W3W4JNW3 <- setEvidence(W3W4JN,nodes = "W3",states = "1")
 
 W3W4BNW3 <- as.bn.fit(W3W4JNW3,including.evidence = TRUE)
 

BN <- W3W4BNW3
 # # 
 # # #graphviz.plot(BN)
 
 structuredCoherenceUpdated( BN = W3W4BNW3,narrationNodes = c("W3","W4"), states = c("1","1"))
 
 
 
 W4W5JN <- compile(as.grain(W4W5BN))
 
 W4W5JNW4 <- setEvidence(W4W5JN,nodes = "W4",states = "1")
 
 W4W5BNW4 <- as.bn.fit(W4W5JNW4,including.evidence = TRUE)
 
 
 BN <- W4W5BNW4
 # # 
 # # #graphviz.plot(BN)
 
 structuredCoherenceUpdated( BN = W4W5BNW4, narrationNodes = c("W4","W5"), states = c("1","1"))
 
 
 
 
 
 
 
 
 # 
 # 
 # \block{Witnesses}{ Consider the following reports:
 #     
 #     \begin{itemize} 
 #   \item Witness no. 1: ‘‘Steve did it’’
 #   
 #   \item  Witness no. 2: ‘‘Steve did it’’
 #   
 #   \item Witness no. 3: ‘‘Steve, Martin or David did it’’
 #   
 #   \item Witness no. 4: ‘‘Steve, John or James did it’’
 #   
 #   \item Witness no. 5: ‘‘Steve, John or Peter did it’’
 #   \end{itemize}
 # }
 # 
 
 W1W2 <-  as.table(matrix(c(1,0,0,7),byrow=TRUE,ncol=2))
 rownames <- c("W1Suspect","W1Innocent")
 colnames <- c("W2Suspect","W2Innocent")
 dimnames(W1W2) <- list("W1" = rownames,"W2" = colnames)
 W1W2
 W1W2kable <- W1W2 %>% kable(format = "latex",booktabs=T,
                             caption = "W1W2",
                       linesep = "",  escape = FALSE) %>%   kable_styling(latex_options=c("striped"))
 
 
 
 W3W4 <-  as.table(matrix(c(1,2,2,3),byrow=TRUE,ncol=2))
 rownames <- c("W3Suspect","W3Innocent")
 colnames <- c("W4Suspect","W4Innocent")
 dimnames(W3W4) <- list("W3" = rownames,"W4" = colnames)
 W3W4
 W3W4kable <- W3W4 %>% kable(format = "latex",booktabs=T,
                             caption = "W3W4",
                             linesep = "",  escape = FALSE) %>%   kable_styling(latex_options=c("striped"))
 
 
 
 
 
  
 
 W4W5 <-  as.table(matrix(c(2,1,1,4),byrow=TRUE,ncol=2))
 rownames <- c("W4Suspect","W4Innocent")
 colnames <- c("W5Suspect","W5Innocent")
 dimnames(W4W5) <- list("W4" = rownames,"W5" = colnames)
 W4W5
 W4W5kable <- W4W5 %>% kable(format = "latex",booktabs=T,
                             caption = "W4W5",
                             linesep = "",  escape = FALSE) %>%   kable_styling(latex_options=c("striped"))
 
 
 
 
 
 
 # witnessTable <- CoherencesTable(WitnessBN,
#                               scenariosList = list(A,B,C),
#                               statesList   = list(c("1","1"),c("1","1"),c("1","1")),
#                               exampleName = "Witness"
# )
# 
# 
# 
# 
# save(witnessTable,file="calculations/RdataObjects/witnessTable.Rda")

#load("calculations/RdataObjects/witnessTable.Rda")





#witnessTable


# structuredCoherence(WitnessBN, A, rep("1",2))
# structuredCoherence(WitnessBN, B, rep("1",2))
# structuredCoherence(WitnessBN, C, rep("1",2))



# witnessTableLaTeX <- tableLaTeX(witnessTable)

# 
# W1W2greaterW3W4 <- witnessTable[1,] > witnessTable[2,] 
# #W1W2greaterW3W4
# 
# W4W5greaterW3W4 <- witnessTable[3,] > witnessTable[2,] 
# #W4W5greaterW3W4
# 
# 
# witnessResults <- as.data.frame(rbind(W1W2greaterW3W4,W4W5greaterW3W4))
# 
# 
# rownames(witnessResults) <- c("Witness: W$_1$W$_2>$W$_3$W$_4$","Witness: W$_4$W$_5>$W$_3$W$_4$")
# 
# 
# 
# # save(witnessResults,file="calculations/RdataObjects/witnessResults.Rda")
# 
# 
# load("calculations/RdataObjects/witnessResults.Rda")
# 
# 
# 
# 
# #witnessResults
# 
# witnessResultsLaTeX <- tableLaTeX(witnessResults)
# #witnessResultsLaTeX
# 
# 
