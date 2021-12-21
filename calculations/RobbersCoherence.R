## Restricted nodes

#BN <- robbersBN
#BN





#structuredCoherenceUpdated(robbersBN,c("MIsP","MIsR"),c("1","0"))

robbersNarrTable <- CoherencesTable(robbersNarrBN, 
                                  scenariosList = list(c("MIsP","MIsR",:),c("MIsP","MIsR"),c("MIsP","MIsR")),
                                 statesList   = list(c("1","1"),c("1","0"),c("0","1")),
                                 exampleName = "Robbers"
)


robbersNarrTable

save(robbersNarrTable, file = "calculations/RdataObjects/robbersNarrTable.Rda")

# # 
neutralPoints <- c(NA, NA ,1, 1, 0, 0, 0.5, 0, 0, 0, 0, 0)
# # 
PRgreaterPnR <- robbersNarrTable[1,] > robbersNarrTable[2,] 
# # #PRgreaterPnR
# # # 
PRgreaterNeutral <- robbersNarrTable[1,] > neutralPoints
# # # 
robbersNarrResults <- as.data.frame(rbind(PRgreaterPnR,PRgreaterNeutral))
# # # 
rownames(robbersResults3) <- c("Robbers: PR$>$P$\\neg$R","Robbers: PR$>$neutral")
# # # 
# # # 
save(robbersNarrResults,file="calculations/RdataObjects/robbersNarrResults.Rda")




robbersNarrResults




#____________________________________________________________________
BN <- robbersBN
robbersTable3 <- CoherencesTable(robbersBN,
        scenariosList = list(c("MIsP","MIsR"),c("MIsP","MIsR"),c("MIsP","MIsR")),
        statesList   = list(c("1","1"),c("1","0"),c("0","1")),
        exampleName = "Robbers"
)

robbersTable3

save(robbersTable3,file="calculations/RdataObjects/robbersTable3.Rda")

load("calculations/RdataObjects/robbersTable3.Rda")


robbersTable3LaTeX <- tableLaTeX(robbersTable3)
# 
# # 
 neutralPoints <- c(NA, NA ,1, 1, 0, 0, 0.5, 0, 0, 0, 0, 0)
# # 
 PRgreaterPnR <- robbersTable3[1,] > robbersTable3[2,] 
# # #PRgreaterPnR
# # # 
 PRgreaterNeutral <- robbersTable3[1,] > neutralPoints
# # # 
 robbersResults3 <- as.data.frame(rbind(PRgreaterPnR,PRgreaterNeutral))
# # # 
 rownames(robbersResults3) <- c("Robbers: PR$>$P$\\neg$R","Robbers: PR$>$neutral")
# # # 
# # # 
 save(robbersResults3,file="calculations/RdataObjects/robbersResults3.Rda")


load("calculations/RdataObjects/robbersResults3.Rda")





#robbersResults3

robbersResults2LaTeX <- tableLaTeX(robbersResults2)

#robbersResultsLaTeX











# 
# BN <- robbersBN
# robbersTable <- CoherencesTable(robbersBN,
#         scenariosList = list(c("MIsP","MIsR"),c("MIsP","MIsR"),c("MIsP","MIsR")),
#         statesList   = list(c("1","1"),c("1","0"),c("0","1")),
#         exampleName = "Robbers"
# )
# 
# 
# 
# save(robbersTable,file="calculations/RdataObjects/robbersTable.Rda")
# 
# load("calculations/RdataObjects/robbersTable.Rda")
# 
# 
# 
# #robbersTable
# 
# 
# #graphviz.plot(robbersBN)
# 
# #structuredCoherence(robbersBN,c("MIsP","MIsR"),c("1","0"))
# 
# 
# 
# robbersTableLaTeX <- tableLaTeX(robbersTable)
# 
# #robbersTableLaTeX
# # 
# 
# neutralPoints <- c(NA, NA ,1, 1, 0, 0, 0.5, 0, 0, 0, 0, 0)
# 
#  PRgreaterPnR <- robbersTable[1,] > robbersTable[2,] 
# # #PRgreaterPnR
# # 
# # PRgreaterNeutral <- robbersTable[1,] > neutralPoints
# # 
# # robbersResults <- as.data.frame(rbind(PRgreaterPnR,PRgreaterNeutral))
# # 
# # rownames(robbersResults) <- c("Robbers: PR$>$P$\\neg$R","Robbers: PR$>$neutral")
# # 
# # 
# # save(robbersResults,file="calculations/RdataObjects/robbersResults.Rda")
# 
# load("calculations/RdataObjects/robbersResults.Rda")
# 
# 
# 
# 
# 
# #robbersResults
# 
# robbersResultsLaTeX <- tableLaTeX(robbersResults)
# 
# #robbersResultsLaTeX
# 
