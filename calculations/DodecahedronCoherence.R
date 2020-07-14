#source("measures//CoherenceMeasures2Var.R")
#source("measures//Fitelson.R")
#source("measures//RA.R")
#source("bns//Dodecahedron.R")


dodN <- c("T","TF")

BN <- RegularBN
dodecahedronTableRegular <- CoherencesTable(RegularBN,
                              scenariosList = list(dodN),
                              statesList   = list(c("1","1")),
                              exampleName = "Regular die"
)

#dodecahedronTableRegular
#structuredCoherence(RegularBN, depthN, c("1","1"))


BN <- DodecahedronBN
dodecahedronTableDodecahedron <- CoherencesTable(DodecahedronBN,
                                     scenariosList = list(dodN),
                                     statesList   = list(c("1","1")),
                                     exampleName = "Dodecahedron"
)


#dodecahedronTableDodecahedron

dodecahedronTable <- rbind(dodecahedronTableRegular,dodecahedronTableDodecahedron)


#dodecahedronTable
#structuredCoherence(DodecahedronBN, depthN, c("1","1"))


dodecahedronTableLaTeX <- tableLaTeX(depthTable)


RegularSameDod <- dodecahedronTable[1,] == dodecahedronTable[2,] 

#RegularSameDod

DodecahedronResults <- as.data.frame(RegularSameDod)

#DodecahedronResults

rownames(DodecahedronResults) <- c("Dodecahedron:  Regular $=$  Dodecahedron")

#DodecahedronResults

DodecahedronResultsLaTeX <- tableLaTeX(DodecahedronResults)



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


