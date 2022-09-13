BGP <- c("B","G","P")
BG <- c("B","G")
BP <- c("B","P")

BN <- BirdBNbgp
penguinsTable <- CoherencesTableEvi(BN = list(BirdBNbgp,BirdBNbg,BirdBNbp),
                scenariosList = list(BGP, BG, BP),
                statesList   = list(c("1","1","1"), c("1","1"), c("1","1")), 
                exampleName = "Penguins")


TTF <- c("T", "TF")
diceTable <- CoherencesTableEvi(BN = list(Dod2RegularBN,Dod2DodecahedronBN),
                                scenariosList = list(TTF, TTF), 
                                statesList   = list(c("1", "1"), c("1", "1")),
                                exampleName = "Dice")



Dunnit <- c("M","G","W","I")
DunnitTwin<- c("M","Tw","G","W","I")
DunnitNoTwinTable <- CoherencesTableEvi(list(DunnitNoTwinBN),
                                        scenariosList = list(Dunnit),
                                        statesList   = list(c("1","1","1","1")),
                                        exampleName = "Dunnit"
)
DunnitTwinTable <- CoherencesTableEvi(list(DunnitBN),
                                      scenariosList = list(DunnitTwin),
                                      statesList   = list(c("1","1","1","1","1")),
                                      exampleName = "Dunnit"
)
DunnitTableSeparate <- rbind(DunnitNoTwinTable,DunnitTwinTable)


J <- c("J","O")
BN <- Jap1BN
JapaneseSwordsTableA <- CoherencesTableEvi(list(Jap1BN),
                                           scenariosList = list(J),
                                           statesList   = list(c("1","1")),
                                           exampleName = "Japanese Swords 1"
)
BN <- Jap2BN
JapaneseSwordsTableB <- CoherencesTableEvi(list(Jap2BN),
                                           scenariosList = list(J),
                                           statesList   = list(c("1","1")),
                                           exampleName = "Japanese Swords 2"
)
BN <- Jap3BN
JapaneseSwordsTableC <- CoherencesTableEvi(list(Jap3BN),
                                           scenariosList = list(J),
                                           statesList   = list(c("1","1")),
                                           exampleName = "Japanese Swords 3"
)
JapaneseSwordsSeparateTable <- rbind(JapaneseSwordsTableA,JapaneseSwordsTableB,JapaneseSwordsTableC)


robbersBNTable <- CoherencesTableEvi(list(robbersBN,robbersBN,robbersBN), 
                                     scenariosList = list(c("MIsP","MIsR"),c("MIsP","MIsR"),c("MIsP","MIsR")),
                                     statesList   = list(c("1","1"),c("1","0"),c("0","1")),
                                     exampleName = "Robbers")

B <- c("J","P","G","R","D")
BN <- BeatlesBN
BeatlesTable <- CoherencesTableEvi(list(BeatlesBN),
                                   scenariosList = list(B),
                                   statesList   = list(c("1","1","1","1","1")),
                                   exampleName = "Beatles"
)



A <- c("W1","W2")
B <- c("W3","W4")
C <- c("W4","W5")

W12Table <- CoherencesTableEvi(list(W12BN),
                               scenariosList = list(A),
                               statesList   = list(c("1","1")),
                               evidenceList = list(A),
                               evidenceStatesList = list(c("1","1")),
                               exampleName = "Witness"
)

W34Table <- CoherencesTableEvi(list(W34BN),
                               scenariosList = list(B),
                               statesList   = list(c("1","1")),
                               evidenceList = list(B),
                               evidenceStatesList = list(c("1","1")),
                               exampleName = "Witness"
)

W45Table <- CoherencesTableEvi(list(W45BN),
                               scenariosList = list(C),
                               statesList   = list(c("1","1")),
                               evidenceList = list(C),
                               evidenceStatesList = list(c("1","1")),
                               exampleName = "Witness"
)

WTable <- as.data.frame(rbind(W12Table, 
                              W34Table,
                              W45Table))






resultsJoint <- rbind(round(penguinsTable,3),
round(diceTable,3), 
round(DunnitTableSeparate,3),
round(JapaneseSwordsSeparateTable,3),
round(robbersBNTable,3),
round(BeatlesTable, 3),
round(WTable,3)
)





resultsJoint










#results
BGlessBGP <- penguinsTable[1,] > penguinsTable[2,]
BPbetweenBGandBGP <-  abs(penguinsTable[3,] - penguinsTable[2,]) >.1 & abs(penguinsTable[1,] - penguinsTable[3,]) >=0
penguinsResults <- as.data.frame(rbind(BGlessBGP,BPbetweenBGandBGP))
rownames(penguinsResults) <- c("Penguins: BG$<$BGP","Penguins: BG$<<$ BP$<$ BGP")

diceDesiderata <- diceTable[1,] == diceTable[2,] 
diceDesiderata <- as.data.frame(diceDesiderata)
rownames(diceDesiderata) <- c("Dodecahedron:  Regular $=$  Dodecahedron")

DunnitLessTwin <- DunnitTableSeparate[1,] < DunnitTableSeparate[2,]
DunnitResultsSeparate <- as.data.frame(DunnitLessTwin)
rownames(DunnitResultsSeparate) <- c("Dunnit: Dunnit$<$Twin")


JO2greaterJO1 <- JapaneseSwordsSeparateTable[2,] > JapaneseSwordsSeparateTable[1,]
JO2greaterJO3 <- JapaneseSwordsSeparateTable[2,] > JapaneseSwordsSeparateTable[3,]
JapaneseSwordsSeparateResults <- as.data.frame(rbind(JO2greaterJO1,JO2greaterJO3))
rownames(JapaneseSwordsSeparateResults) <- c("Swords: JO2$>$JO1","Swords: JO2$>$JO3")


neutralPoints <- c(NA, NA ,1, 1, 0.5, 0, 0, 0, 1, 0)
PRgreaterPnR <- robbersBNTable[1,] > robbersBNTable[2,] 
PRgreaterNeutral <- robbersBNTable[1,] > neutralPoints
robbersEviResults <- as.data.frame(rbind(PRgreaterPnR,PRgreaterNeutral))
rownames(robbersEviResults) <- c("Robbers: PR$>$P$\\neg$R","Robbers: PR$>$neutral")


B <- c("J","P","G","R","D")
minima <-  c(0, 0 ,0, 0, -1, -1, 0, -1, 0, -1)
neutralPoints <- c(NA, NA ,1, 1, 0, 0, 0.5, 0, 1, 0)
BeatlesMinimal <- BeatlesTable[1,] == minima
BeatlesBelowNeutral <- BeatlesTable[1,] < neutralPoints
BeatlesResults <- as.data.frame(rbind(BeatlesBelowNeutral, BeatlesMinimal))
rownames(BeatlesResults) <- c("Beatles: below neutral", "Beatles: minimal")


W1W2greaterW3W4 <- WTable[1,] > WTable[2,] 
W4W5greaterW3W4 <- WTable[3,] > WTable[2,] 
WResults <- as.data.frame(rbind(W1W2greaterW3W4,W4W5greaterW3W4))
rownames(WResults) <- c("Witness: W$_1$W$_2>$W$_3$W$_4$","Witness: W$_4$W$_5>$W$_3$W$_4$")


desiderataJoint <- rbind(penguinsResults,
diceDesiderata, 
DunnitResultsSeparate,
JapaneseSwordsSeparateResults,
robbersEviResults,
BeatlesResults,
WResults
)


desiderataJoint <- rbind(desiderataJoint,
                         paste(round(colMeans(desiderataJoint, na.rm = TRUE),2)*100, 
                               "\\%", sep = ""))

rownames(desiderataJoint)[13] <- "Success rate"

desiderataJoint

