
structuredEvi(RegularBN, c("T","TF"), c("1","1"))

structuredEvi(RegularBN, c("T","TF"), c("1","1"),
              evidenceNodes = c("T","TF"), evidenceStates = c("1","1") )

graphviz.plot(Dod2RegularBN)
structuredEvi(Dod2RegularBN,c("T","TF"),c("1","1"))
structuredEvi(Dod2DodBN,c("T","TF"),c("1","1"))
structuredEvi(Dod2ReverseBN,c("T","TF"),c("1","1"))
structuredEvi(Dod2DodReverseBN,c("T","TF"),c("1","1"))

Dod2ReverseBN




BN <- RegularBN
RegularTable <- CoherencesTableEvi(list(RegularBN),
                                     scenariosList = list(c("T","TF")),
                                     statesList   = list(c("1","1")),
                                     exampleName = "Regular"
)


DodecahedronTable <- CoherencesTableEvi(list(DodecahedronBN),
                                    scenariosList = list(c("T","TF")),
                                    statesList   = list(c("1","1")),
                                    exampleName = "Dodecahedron"
)


DiceTableNew <- rbind(RegularTable,DodecahedronTable)

DiceTableNew


RegularSameDod <- DiceTableNew[1,] == DiceTableNew[2,] 
dodecahedronResults <- as.data.frame(RegularSameDod)
rownames(dodecahedronResults) <- c("Dodecahedron:  Regular $=$  Dodecahedron")


dodecahedronResults




