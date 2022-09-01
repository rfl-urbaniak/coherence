## Two nodes
#BN <- robbersBN
#BN

BN <- robbersBN

graphviz.plot(robbersBN)

BN

robbersBNTable <- CoherencesTableEvi(list(robbersBN,robbersBN,robbersBN), 
                    scenariosList = list(c("MIsP","MIsR"),c("MIsP","MIsR"),c("MIsP","MIsR")),
                    statesList   = list(c("1","1"),c("1","0"),c("0","1")),
                    exampleName = "Robbers")

robbersBNTable

structuredEvi(robbersBN,c("MIsP","MIsR"),c("1","1"))
CoherencesRowEvi(list(robbersBN), list(c("MIsP","MIsR")), list(c("1","1")))




length(neutralPoints)
neutralPoints <- c(NA, NA ,1, 1, 0.5, 0, 0, 0, 1, 0)
# # 
PRgreaterPnR <- robbersBNTable[1,] > robbersBNTable[2,] 
# # #PRgreaterPnR
# # # 
PRgreaterNeutral <- robbersBNTable[1,] > neutralPoints
# # # 
robbersEviResults <- as.data.frame(rbind(PRgreaterPnR,PRgreaterNeutral))
# # # 
rownames(robbersEviResults) <- c("Robbers: PR$>$P$\\neg$R","Robbers: PR$>$neutral")
# # # 

robbersEviResults
# # # 







robbersTwoTable <- CoherencesTableNarr(list(robbersTwoBN,robbersTwoBN,robbersTwoBN), 
              scenariosList = list(c("MIsP","MIsR"),c("MIsP","MIsR"),c("MIsP","MIsR")),
              statesList   = list(c("1","1"),c("1","0"),c("0","1")),
              exampleName = "Robbers")

robbersTwoTable


length(names(robbersTwoTable))
length(neutralPoints)
neutralPoints <- c(NA, NA ,1, 1, 0.5, 0, 0, 0, 1, 0)
# # 
PRgreaterPnR <- robbersTwoTable[1,] > robbersTwoTable[2,] 
# # #PRgreaterPnR
# # # 
PRgreaterNeutral <- robbersTwoTable[1,] > neutralPoints
# # # 
robbersNarrResults <- as.data.frame(rbind(PRgreaterPnR,PRgreaterNeutral))
# # # 
rownames(robbersNarrResults) <- c("Robbers: PR$>$P$\\neg$R","Robbers: PR$>$neutral")
# # # 

robbersNarrResults

#





# # 
length(names(robbersNarrTable))
length(neutralPoints)
neutralPoints <- c(NA, NA ,1, 1, 0.5, 0, 0, 0, 1, 0)
# # 
PRgreaterPnR <- robbersNarrTable[1,] > robbersNarrTable[2,] 
# # #PRgreaterPnR
# # # 
PRgreaterNeutral <- robbersNarrTable[1,] > neutralPoints
# # # 
robbersNarrResults <- as.data.frame(rbind(PRgreaterPnR,PRgreaterNeutral))
# # # 
rownames(robbersNarrResults) <- c("Robbers: PR$>$P$\\neg$R","Robbers: PR$>$neutral")
# # # 

robbersNarrResults
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

robbersTable3Narr <- CoherencesTableNarr(list(robbersBN,robbersBN,robbersBN),
                                 scenariosList = list(c("MIsP","MIsR"),c("MIsP","MIsR"),
                                                      c("MIsP","MIsR")),
                                 statesList   = list(c("1","1"),c("1","0"),c("0","1")),
                                 exampleName = "Robbers"
)


robbersTable3Narr

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
 
 robbersResults3
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
