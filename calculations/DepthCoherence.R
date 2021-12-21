X1 <- c("T123","T124","T134")
X2 <- c("T123","T145","T167")
# # #
BN <- DepthBN
#graphviz.plot(BN)


##Separate three-node BNs

graphviz.plot(DX1BN)

graphviz.plot(DX2BN)

depthTableNarrX1 <- CoherencesTable(DX1BN,
                               scenariosList = list(X1),
                               statesList   = list(c("1","1","1")),
                               exampleName = "Depth"
)


depthTableNarrX2 <- CoherencesTable(DX2BN,
                                        scenariosList = list(X2),
                                        statesList   = list(c("1","1","1")),
                                        exampleName = "Depth"
)


depthTableNarr <- rbind(depthTableNarrX1,depthTableNarrX2)




depthTableNarr

save(depthTableNarr, file  = "calculations/RdataObjects/depthTableNarr.Rda")



#  
X1greaterX2<- depthTableNarr[1,] > depthTableNarr[2,]
# X1greaterX2
# 
depthResultsNarr <- as.data.frame(rbind(X1greaterX2))
# 
# 
rownames(depthResultsNarr) <- c("Depth: X$_1>$X$_2$")
# 
# 
# 
save(depthResultsNarr,file="calculations/RdataObjects/depthResultsNarr.Rda")









structuredCoherence(BN = DepthBN,narrationNodes = X1, states = c("1","1","1"))

# # 
depthTable2 <- CoherencesTable(DepthBN,
                              scenariosList = list(X1,X2),
                              statesList   = list(c("1","1","1"),c("1","1","1")),
                              exampleName = "Depth"
)
# 
#depthTable2
# # 
# # 
save(depthTable2,file="calculations/RdataObjects/depthTable2.Rda")


load("calculations/RdataObjects/depthTable2.Rda")


#depthTable2


depthTable2LaTeX <- tableLaTeX(depthTable2)





library(vcd)


A <- as.table(matrix(c(2,1,1,4),byrow=TRUE,ncol=2))
rownames <- c("aSuspect","aInnocent")
colnames <- c("bSuspect","bInnocent")
dimnames(A) <- list("aWitness" = rownames,"bWitness" = colnames)
A


B <- as.table(matrix(c(1,2,2,3),byrow=TRUE,ncol=2))
rownames <- c("aSuspect","aInnocent")
colnames <- c("bSuspect","bInnocent")
dimnames(B) <- list("aWitness" = rownames,"bWitness" = colnames)
B




Kappa(A)
Kappa(B)


agreementplot(A)
agreementplot(B)


#  
 X1greaterX2<- depthTable2[1,] > depthTable2[2,]
# X1greaterX2
# 
 depthResults2 <- as.data.frame(rbind(X1greaterX2))
# 
# 
 rownames(depthResults2) <- c("Depth: X$_1>$X$_2$")
# 
# 
# 
 save(depthResults2,file="calculations/RdataObjects/depthResults2.Rda")

load("calculations/RdataObjects/depthResults2.Rda")


#depthResults2

depthResults2LaTeX <- tableLaTeX(depthResults2)
#depthResultsLaTeX



options <- expand.grid(0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1)

nrow(options)

x1 <- options[(options$Var1 == 1 | options$Var2 == 1 | options$Var3 == 1) & 
                (options$Var1 == 1 | options$Var2 == 1 | options$Var4 == 1) & 
                (options$Var1 == 1 | options$Var3 == 1 | options$Var4 == 1)
              ,]


nrow(x1)




w2 <- options[,]

w3 <- options[,]


nrow(w1)
nrow(w2)
nrow(w3)


library(dplyr)

w2 <- 

testimonies






library(vcd)


A <- as.table(matrix(c(2,1,1,4),byrow=TRUE,ncol=2))
rownames <- c("aSuspect","aInnocent")
colnames <- c("bSuspect","bInnocent")
dimnames(A) <- list("aWitness" = rownames,"bWitness" = colnames)
A


B <- as.table(matrix(c(1,2,2,3),byrow=TRUE,ncol=2))
rownames <- c("aSuspect","aInnocent")
colnames <- c("bSuspect","bInnocent")
dimnames(B) <- list("aWitness" = rownames,"bWitness" = colnames)
B




Kappa(A)
Kappa(B)


agreementplot(A)
agreementplot(B)







## now for witness perspective
witnessA1 <- CoherencesTable(T123BN,
                                 scenariosList = list(c("T123","T124")),
                                 statesList   = list(c("1","1")),
                                 exampleName = "WitnessA"
)


witnessA1


witnessB1 <- CoherencesTable(T123BN2,
                             scenariosList = list(c("T123","T145")),
                             statesList   = list(c("1","1")),
                             exampleName = "WitnessB"
)


depthPerspectiveTable <- rbind(witnessA1,witnessB1)

depthPerspectiveTable

rownames(depthPerspectiveTable) <- c("DepthA: T123T124 11", "DepthB: T123T145 11")

save(depthPerspectiveTable,file = "calculations/RdataObjects/depthPerspectiveTable.Rda")

load("calculations/RdataObjects/depthPerspectiveTable.Rda")


# # 
X1greaterX2<- depthPerspectiveTable[1,] > depthPerspectiveTable[2,] 
# # X1greaterX2
# # \
 depthPerspectiveResults <- as.data.frame(rbind(X1greaterX2))
# # 
# # 
rownames(depthPerspectiveResults) <- c("Depth: X$_1>$X$_2$")
# # 
# 
# 
save(depthPerspectiveResults,file="calculations/RdataObjects/depthPerspectiveResults.Rda")
# 

#depthPerspectiveResults





# 
#depthTable <- CoherencesTable(DepthBN,
#                               scenariosList = list(X1,X2),
#                               statesList   = list(c("1","1","1"),c("1","1","1")),
#                               exampleName = "Depth"
# )
# 
# 
# save(depthTable,file="calculations/RdataObjects/depthTable.Rda")

#load("calculations/RdataObjects/depthTable.Rda")

#depthTable



# # 
#X1greaterX2<- depthTable[1,] > depthTable[2,] 
# # X1greaterX2
# # 
# # depthResults <- as.data.frame(rbind(X1greaterX2))
# # 
# # 
# # rownames(depthResults) <- c("Depth: X$_1=$X$_2$")
# # 
# 
# 
# #save(depthResults,file="calculations/RdataObjects/depthResults.Rda")
# 
# load("calculations/RdataObjects/depthResults.Rda")
# 
# 
# #depthResults
# 
# depthResultsLaTeX <- tableLaTeX(depthResults)
# #depthResultsLaTeX







# 
# sc <- structuredCoherence(DepthBN,X1,rep("1",3))
# 
# sc2 <- structuredCoherence(DepthBN,X2,rep("1",3))
# 
# sc
# 
# dragOut <- function(sc){
#   ZweightedAnteJoint <- list()
#   for(i in 1:length(sc$`Full calculations`)){
#     ZweightedAnteJoint <- append(ZweightedAnteJoint,sc$`Full calculations`[[i]]$`Options & calculations`$ZweightedAnte)
#   }
#   unlist(ZweightedAnteJoint)
# }
# 
# 
# 
# 
# do <- dragOut(sc)
# 
# do2 <- dragOut(sc2)
# 
# do2
# 
# structuredScore(do)
# 
# structuredScore(do2)


# 



