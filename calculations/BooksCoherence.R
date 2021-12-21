 AR <- c("A","R")
 BN <- BooksBN
# # 
 booksTable3 <- CoherencesTable(BooksBN,
                                 scenariosList = list(AR,AR,AR,AR),
                                 statesList   = list(c("1","1"),c("1","0"),c("0","1"),c("0","0")),
                                 exampleName = "Books"
 )
# # 
# # 
# # 
booksTable3
# 
# 
save(booksTable3,file="calculations/RdataObjects/booksTable3.Rda")

#load("calculations/RdataObjects/booksTable3.Rda")
 
#  
#booksTable3LaTeX <- tableLaTeX(booksTable3)
# 
# 
# 
# 
# #booksTable3
# #
 ARgreaterAnR2 <- booksTable3[1,] > booksTable3[2,]
# # #ARgreaterAnR
# #
 ARgreaternAR2 <- booksTable3[1,] > booksTable3[3,]
# # #ARgreaternAR
# #
# #
 nAnRgreaterAnR2 <- booksTable3[4,] > booksTable3[2,]
# # #ARgreaterAnR
# #
# nAnRgreaternAR2 <- booksTable3[4,] > booksTable3[3,]
# # #nAnRgreaternAR
# #
# #
 booksResults3 <- as.data.frame(rbind(ARgreaterAnR2,ARgreaternAR2,nAnRgreaterAnR2,nAnRgreaternAR2))
# #
# #
# #
 rownames(booksResults3) <- c("Books: AR$>$A$\\neg$R",
                             "Books: AR$>\\neg$AR",
                             "Books: $\\neg$A$\\neg$R$>$A$\\neg$R",
                             "Books: $\\neg$A$\\neg$R$>\\neg$AR")
# 
 save(booksResults3,file="calculations/RdataObjects/booksResults3.Rda")

$load("calculations/RdataObjects/booksResults3.Rda")


#booksResults3

booksResults3LaTeX <- tableLaTeX(booksResults3)
#booksResultsLaTeX








#OUTDATED

 
  
# booksTable <- CoherencesTable(BooksBN,
#                                 scenariosList = list(AR,AR,AR,AR),
#                                 statesList   = list(c("1","1"),c("1","0"),c("0","1"),c("0","0")),
#                                 exampleName = "Books"
# )
# 
# 
# save(booksTable,file="calculations/RdataObjects/booksTable.Rda")

#load("calculations/RdataObjects/booksTable.Rda")









# booksTableLaTeX <- tableLaTeX(booksTable)
# 
# #booksTable
# 
# #ARgreaterAnR <- booksTable[1,] > booksTable[2,] 
# #ARgreaterAnR 
# 
# #ARgreaternAR <- booksTable[1,] > booksTable[3,] 
# #ARgreaternAR
# 
# 
# #nAnRgreaterAnR <- booksTable[4,] > booksTable[2,] 
# #ARgreaterAnR 
# 
# #nAnRgreaternAR <- booksTable[4,] > booksTable[3,] 
# #nAnRgreaternAR
# 
# 
# #booksResults <- as.data.frame(rbind(ARgreaterAnR,ARgreaternAR,nAnRgreaterAnR,nAnRgreaternAR))
# 
# 
# # 
# # rownames(booksResults) <- c("Books: AR$>$A$\\neg$R",
# #                             "Books: AR$>\\neg$AR",
# #                             "Books: $\\neg$A$\\neg$R$>$A$\\neg$R",
# #                             "Books: $\\neg$A$\\neg$R$>\\neg$AR")
# 
# #save(booksResults,file="calculations/RdataObjects/booksResults.Rda")
# 
# load("calculations/RdataObjects/booksResults.Rda")
# 
# 
# #booksResults
# 
# booksResultsLaTeX <- tableLaTeX(booksResults)
# #booksResultsLaTeX
# 
# 
# 
