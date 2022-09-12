
Dod2RegularBN 


Dod2DodecahedronBN


TTF <- c("T", "TF")

Dod2DAG <- model2network("[TF|T][T]")
T2Prob <- priorCPT(node = "T", prob1 = 1/6)
TF2Prob <- singleCPT(eNode = "TF",hNode = "T", probEifHS1 = 1, probEifHS2 = 1/5)
Dod2RegularCPT <-  list(T=T2Prob,TF=TF2Prob)
Dod2RegularBN <- custom.fit(Dod2DAG,Dod2RegularCPT)

T2dodProb <- priorCPT(node = "T", prob1 = 1/12)
TF2dodProb <- singleCPT(eNode = "TF",hNode = "T", probEifHS1 = 1, probEifHS2 = 1/11)
Dod2DodecahedronCPT <-  list(T=T2dodProb,TF=TF2dodProb)
Dod2DodecahedronBN <- custom.fit(Dod2DAG,Dod2DodecahedronCPT)

diceTable <- CoherencesTableEvi(BN = list(Dod2RegularBN,Dod2DodecahedronBN),
              scenariosList = list(TTF, TTF), 
              statesList   = list(c("1", "1"), c("1", "1")),
              exampleName = "Dice")


diceTable





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




