#source("measures//CoherenceMeasures2Var.R")
#source("measures//Fitelson.R")
#source("measures//RA.R")
#source("bns//Dodecahedron.R")



#_______________
#Fitelson

#narrationNodes <- c("T","TF")
#states <- c("1","1")


#inspect the pairs (think both left to right and right to left)
#write the logical states by hand
#combinations <- DisjointPairs(narrationNodes)
#combinations
# 
DieStatusLR <- c("Ind")
DieStatusRL <- c("Ent")


RegularFitelson <- FitelsonCoherenceForBNs(BN =  RegularBN, narrationNodes = c("T","TF"),
                                              states = c("1","1"), 
                                              statusLR = DieStatusLR, 
                                              statusRL = DieStatusRL 
)


#_________________
#Fitelson for a dodecahedron

#narrationNodes <- c("T","TF")
#states <- c("1","1")


#inspect the pairs (think both left to right and right to left)
#write the logical states by hand
#combinations <- DisjointPairs(narrationNodes)
#combinations
# 
DieStatusLR <- c("Ind")
DieStatusRL <- c("Ent")


DodecahedronFitelson <- FitelsonCoherenceForBNs(BN =  DodecahedronBN, narrationNodes = c("T","TF"),
                                                states = c("1","1"), 
                                                statusLR = DieStatusLR, 
                                                statusRL = DieStatusRL 
)

#DodecahedronFitelson


#_______________________
#RA 



RegularRA <- RAcoherenceForBNs(BN = RegularBN,narrationNodes = c("T","TF"),states = c("1","1"))

DodecahedronRA <- RAcoherenceForBNs(BN = DodecahedronBN,narrationNodes = c("T","TF"),states = c("1","1"))


#_________________
#Roche

RegularRoche <-  RocheCoherenceForBNs(BN = RegularBN,narrationNodes = c("T","TF"),states = c("1","1"))


DodecahedronRoche <-   RocheCoherenceForBNs(BN = DodecahedronBN,narrationNodes = c("T","TF"),states = c("1","1"))

#________________
#Shogenji

RegularShogenji <- ShogenjiCoherenceForBNs(BN = RegularBN,narrationNodes = c("T","TF"),states = c("1","1"))

DodecahedronShogenji <- ShogenjiCoherenceForBNs(BN = DodecahedronBN,narrationNodes = c("T","TF"),states = c("1","1"))

#________________
#Olsson


RegularOlsson <- OlssonCoherenceForBNs(BN = RegularBN,narrationNodes = c("T","TF"),states = c("1","1"))

DodecahedronOlsson <- OlssonCoherenceForBNs(BN = DodecahedronBN,narrationNodes = c("T","TF"),states = c("1","1"))

#________________
# Douven - Meijs

RegularDouvenMeijs <- DouvenMeijsCoherenceForBNs(BN = RegularBN,narrationNodes = c("T","TF"),states = c("1","1"))

DodecahedronDouvenMeijs <- DouvenMeijsCoherenceForBNs(BN = DodecahedronBN,narrationNodes = c("T","TF"),states = c("1","1"))

#__________________
#TABLES


rownamesDodecahedron <- c("Regular","Dodecahedron")

DodecahedronTRA <- c(RegularRA[[3]],DodecahedronRA[[3]])

DodecahedronTShogenji <- c(RegularShogenji[[3]],DodecahedronShogenji[[3]])

DodecahedronTOlsson <- c(RegularOlsson[[3]],DodecahedronOlsson[[3]])

DodecahedronTDouvenMeijs <- c(RegularDouvenMeijs[[3]],DodecahedronDouvenMeijs[[3]])

DodecahedronTRoche <- c(RegularRoche[[3]],DodecahedronRoche[[3]])

DodecahedronTFitelson <- c(RegularFitelson[[3]],DodecahedronFitelson[[3]])






DodecahedronTable <- data.frame(
  Fitelson = DodecahedronTFitelson,
  "DouvenMeijs" = DodecahedronTDouvenMeijs,
  Olsson = DodecahedronTOlsson,
  RA = DodecahedronTRA,
  Roche = DodecahedronTRoche,
  Shogenji = DodecahedronTShogenji)


rownames(DodecahedronTable) <- rownamesDodecahedron



#DodecahedronTable

RegularSameDodecahedron <- DodecahedronTable[1,] == DodecahedronTable[2,] 

#RegularSameDodecahedron

DodecahedronResults <- as.data.frame(rbind(RegularSameDodecahedron))


rownames(DodecahedronResults) <- c("Dodecahedron: Regular$=$Dodecahedron")

#DodecahedronResults



