
Dice2SixBN 
Dice2DodBN 


  BN <- Dice2SixBN 
  
  Dice2SixBN
  
  structuredCoherence(BN = Dice2SixBN, narrationNodes = c("O2","O24"), states = c("1","1"))
 
  structuredCoherenceNarration(BN = Dice2SixBN, narrationNodes = c("O2","O24"), states = c("1","1"))
  
  
  structuredCoherenceNarration(BN = Dice2DodBN, narrationNodes = c("O2","O24"), states = c("1","1"))
  
    
   
  Dice2SixTable <- CoherencesTableNarr(Dice2SixBN,
                               scenariosList = list(c("O2","O24")),
                               statesList   = list(c("1","1")),
                               exampleName = "Regular die"
 )


Dice2SixTable  


Dice2DodTable <- CoherencesTableNarr(Dice2DodBN,
                                     scenariosList = list(c("O2","O24")),
                                     statesList   = list(c("1","1")),
                                     exampleName = "Dodecahedron"
)


Dice2DodTable







RegularTable <- CoherencesTableNarr(RegularBN,
                                     scenariosList = list(c("T","TF")),
                                     statesList   = list(c("1","1")),
                                     exampleName = "Regular"
)




DodecahedronTable <- CoherencesTableNarr(DodecahedronBN,
                                    scenariosList = list(c("T","TF")),
                                    statesList   = list(c("1","1")),
                                    exampleName = "Dodecahedron"
)


RegularTable
DodecahedronTable

DiceTableNew <- rbind(RegularTable,DodecahedronTable)

DiceTableNew


save(DiceTableNew,file="calculations/RdataObjects/diceTablePaper.Rda")


save(DiceTableNew,file="calculations/RdataObjects/DiceTableNew.Rda")




RegularSameDod <- DiceTableNew[1,] == DiceTableNew[2,] 
# 
# #RegularSameDod
# 
dodecahedronResults <- as.data.frame(RegularSameDod)
# 
# #DodecahedronResults
# 
rownames(dodecahedronResults) <- c("Dodecahedron:  Regular $=$  Dodecahedron")
# 
# 
save(dodecahedronResults,file="calculations/RdataObjects/dodecahedronResultsNew.Rda")

dodecahedronResults

load("calculations/RdataObjects/dodecahedronResults.Rda")










structuredCoherenceNarration(RegularBN, c("T","TF"),c("1","1"))





# 
# dodN <- c("T","TF")
# # #graphviz.plot(BN)
# # 
#  BN <- RegularBN
#  dodecahedronTableRegular <- CoherencesTable(RegularBN,
#                               scenariosList = list(dodN),
#                               statesList   = list(c("1","1")),
#                               exampleName = "Regular die"
# )
# # 
#dodecahedronTableRegular

 
# #structuredCoherence(RegularBN, dodN, c("1","1"))
# 
# # 
# BN <- DodecahedronBN
# dodecahedronTableDodecahedron <- CoherencesTable(DodecahedronBN,
#                                      scenariosList = list(dodN),
#                                      statesList   = list(c("1","1")),
#                                      exampleName = "Dodecahedron"
# )
# # 
# # 
# #dodecahedronTableDodecahedron
# # 
# dodecahedronTable <- rbind(dodecahedronTableRegular,dodecahedronTableDodecahedron)


#save(dodecahedronTable,file="calculations/RdataObjects/dodecahedronTable.Rda")

load("calculations/RdataObjects/dodecahedronTable.Rda")

#dodecahedronTable

#structuredCoherence(DodecahedronBN, depthN, c("1","1"))


#dodecahedronTableLaTeX <- tableLaTeX(dodecahedronTable)


#RegularSameDod <- dodecahedronTable[1,] == dodecahedronTable[2,] 
# 
# #RegularSameDod
# 
#dodecahedronResults <- as.data.frame(RegularSameDod)
# 
# #DodecahedronResults
# 
#rownames(dodecahedronResults) <- c("Dodecahedron:  Regular $=$  Dodecahedron")
# 
# 
# save(dodecahedronResults,file="calculations/RdataObjects/dodecahedronResults.Rda")

load("calculations/RdataObjects/dodecahedronResults.Rda")

#dodecahedronResults

#dodecahedronResultsLaTeX <- tableLaTeX(dodecahedronResults)




#try tetrahedron


#graphviz.plot(D4BNC)

# tetN <- c("O2","TT","C","L")
# 
# 
# BN <- D4BN
# 
# tetTableL <- CoherencesTable(D4BN, scenariosList = list(tetN,tetN), statesList   = list(c("1","1","1","1"),c("1","1","1","0")),  exampleName = "Tetrahedron")

#tetTableL

#save(tetTableL,file="calculations/RdataObjects/tetTableL2.Rda")



load("calculations/RdataObjects/tetTableL2.Rda")
# 
tet <-  c("O2","TT","C")



#structuredCoherence(D4BN,tet,rep("1",3))

# 
# 
# options <- seq(0.1,.9,by=0.1)
# Lprobs <- list()
# DCPTs <- list()
# DBNs <- list()
# Dtables <- list()
# 
# length(options)
# 
# 
# 
# for(o in 1:length(options)){
# Lprobs[[o]] <- priorCPT(node = "L",state1 = "1", state2 = "0", prob1 = options[o])
# 
# DCPTs[[o]] <- list(L=Lprobs[[o]], O1 = Prob1, O2 = Prob2, O3 = Prob3, O4 = Prob4, C = Cprob, TT = TTprob)
# 
# DBNs[[o]] <- custom.fit(D4DAG,DCPTs[[o]])
# 
# Dtables[[o]]  <- CoherencesTable(DBNs[[o]], scenariosList = list(tet), statesList   = list(c("1","1","1"),c("1","1","1")),  exampleName = "Tetrahedron") 
# }
# 
# Dtable <- do.call("rbind",Dtables)

#rownames(Dtable) <- paste("Prior =",options)

#Dtable


#Lprobs


#tetTable5 <- CoherencesTable(D4BN, scenariosList = list(tet), statesList   = list(c("1","1","1"),c("1","1","1")),  exampleName = "Tetrahedron")


#tetTable5

#save(tetTable5,file="calculations/RdataObjects/tetTable5.Rda")

load("calculations/RdataObjects/tetTable5.Rda")


#tetTable1 <- CoherencesTable(D4BN.1, scenariosList = list(tet), statesList   = list(c("1","1","1"),c("1","1","1")),  exampleName = "Tetrahedron")


#tetTable1


#tetTable9 <- CoherencesTable(D4BN.1, scenariosList = list(tet), statesList   = list(c("1","1","1"),c("1","1","1")),  exampleName = "Tetrahedron")











 #<- dodecahedronTable[1,] == dodecahedronTable[2,] 
# 
# #RegularSameDod
# 
# dodecahedronResults <- as.data.frame(RegularSameDod)
# 
# #DodecahedronResults
# 
# rownames(dodecahedronResults) <- c("Dodecahedron:  Regular $=$  Dodecahedron")
# 
# 
# save(dodecahedronResults,file="calculations/RdataObjects/dodecahedronResults.Rda")


# 
# BN <- D4BN
# structuredCoherence(D4BN,narrationNodes = tetN,states = c("1","1","1"))







### OUTDATED







# 
# 
# #_______________
# #Fitelson
# 
# #narrationNodes <- c("T","TF")
# #states <- c("1","1")
# 
# 
# #inspect the pairs (think both left to right and right to left)
# #write the logical states by hand
# #combinations <- DisjointPairs(narrationNodes)
# #combinations
# # 
# DieStatusLR <- c("Ind")
# DieStatusRL <- c("Ent")
# 
# 
# RegularFitelson <- FitelsonCoherenceForBNs(BN =  RegularBN, narrationNodes = c("T","TF"),
#                                               states = c("1","1"), 
#                                               statusLR = DieStatusLR, 
#                                               statusRL = DieStatusRL 
# )
# 
# 
# #_________________
# #Fitelson for a dodecahedron
# 
# #narrationNodes <- c("T","TF")
# #states <- c("1","1")
# 
# 
# #inspect the pairs (think both left to right and right to left)
# #write the logical states by hand
# #combinations <- DisjointPairs(narrationNodes)
# #combinations
# # 
# DieStatusLR <- c("Ind")
# DieStatusRL <- c("Ent")
# 
# 
# DodecahedronFitelson <- FitelsonCoherenceForBNs(BN =  DodecahedronBN, narrationNodes = c("T","TF"),
#                                                 states = c("1","1"), 
#                                                 statusLR = DieStatusLR, 
#                                                 statusRL = DieStatusRL 
# )
# 
# #DodecahedronFitelson
# 
# 
# #_______________________
# #RA 
# 
# 
# 
# RegularRA <- RAcoherenceForBNs(BN = RegularBN,narrationNodes = c("T","TF"),states = c("1","1"))
# 
# DodecahedronRA <- RAcoherenceForBNs(BN = DodecahedronBN,narrationNodes = c("T","TF"),states = c("1","1"))
# 
# 
# #_________________
# #Roche
# 
# RegularRoche <-  RocheCoherenceForBNs(BN = RegularBN,narrationNodes = c("T","TF"),states = c("1","1"))
# 
# 
# DodecahedronRoche <-   RocheCoherenceForBNs(BN = DodecahedronBN,narrationNodes = c("T","TF"),states = c("1","1"))
# 
# #________________
# #Shogenji
# 
# RegularShogenji <- ShogenjiCoherenceForBNs(BN = RegularBN,narrationNodes = c("T","TF"),states = c("1","1"))
# 
# DodecahedronShogenji <- ShogenjiCoherenceForBNs(BN = DodecahedronBN,narrationNodes = c("T","TF"),states = c("1","1"))
# 
# #________________
# #Olsson
# 
# 
# RegularOlsson <- OlssonCoherenceForBNs(BN = RegularBN,narrationNodes = c("T","TF"),states = c("1","1"))
# 
# DodecahedronOlsson <- OlssonCoherenceForBNs(BN = DodecahedronBN,narrationNodes = c("T","TF"),states = c("1","1"))
# 
# #________________
# # Douven - Meijs
# 
# RegularDouvenMeijs <- DouvenMeijsCoherenceForBNs(BN = RegularBN,narrationNodes = c("T","TF"),states = c("1","1"))
# 
# DodecahedronDouvenMeijs <- DouvenMeijsCoherenceForBNs(BN = DodecahedronBN,narrationNodes = c("T","TF"),states = c("1","1"))
# 
# 
# #_______________
# # structured
# 
# regularStructured <- structuredCoherence(BN = RegularBN,narrationNodes = c("T","TF"),states = c("1","1"))
# 
# 
# dodecahedronStructured <- structuredCoherence(BN = DodecahedronBN,narrationNodes = c("T","TF"),states = c("1","1"))
# 
# 
# regularStructured
# 
# dodecahedronStructured
# 
# graphviz.plot(DodecahedronBN)
# 
# #__________________
# #TABLES
# 
# 
# 
# 
# 
# rownamesDodecahedron <- c("Regular","Dodecahedron")
# 
# DodecahedronTRA <- c(RegularRA[[3]],DodecahedronRA[[3]])
# 
# DodecahedronTShogenji <- c(RegularShogenji[[3]],DodecahedronShogenji[[3]])
# 
# DodecahedronTOlsson <- c(RegularOlsson[[3]],DodecahedronOlsson[[3]])
# 
# DodecahedronTDouvenMeijs <- c(RegularDouvenMeijs[[3]],DodecahedronDouvenMeijs[[3]])
# 
# DodecahedronTRoche <- c(RegularRoche[[3]],DodecahedronRoche[[3]])
# 
# DodecahedronTFitelson <- c(RegularFitelson[[3]],DodecahedronFitelson[[3]])
# 
# DodecahedronTStructuredCon <- c(regularStructured$`structured Coherence Con`,
# dodecahedronStructured$`structured Coherence Con`)
# 
# #regularStructured
# 
# DodecahedronTStructuredAnte <- c(regularStructured$`structured Coherence Ante`,
#                                 dodecahedronStructured$`structured Coherence Ante`)
# 
# 
# 
# 
# DodecahedronTable <- data.frame(
#   Fitelson = DodecahedronTFitelson,
#   "DouvenMeijs" = DodecahedronTDouvenMeijs,
#   Olsson = DodecahedronTOlsson,
#   RA = DodecahedronTRA,
#   Roche = DodecahedronTRoche,
#   Shogenji = DodecahedronTShogenji,
#   StructuredCon  = DodecahedronTStructuredCon,
#   StructuredAnte = DodecahedronTStructuredAnte)
# 
# DodecahedronTable
# 
# rownames(DodecahedronTable) <- rownamesDodecahedron
# 
# 
# 
# DodecahedronTableLaTeX <- tableLaTeX(DodecahedronTable)


