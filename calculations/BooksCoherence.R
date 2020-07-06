AR <- c("A","R")



booksTable <- CoherencesTable(BooksBN,
                                scenariosList = list(AR,AR,AR,AR),
                                statesList   = list(c("1","1"),c("1","0"),c("0","1"),c("0","0")),
                                exampleName = "Books"
)

booksTable


booksTableLaTeX <- tableLaTeX(booksTable)

booksTable

ARgreaterAnR <- booksTable[1,] > booksTable[2,] 
#ARgreaterAnR 

ARgreaternAR <- booksTable[1,] > booksTable[3,] 

nAnRgreaterAnR <- booksTable[4,] > booksTable[2,] 
#ARgreaterAnR 

nAnRgreaternAR <- booksTable[4,] > booksTable[3,] 



booksResults <- as.data.frame(rbind(ARgreaterAnR,ARgreaternAR,nAnRgreaterAnR,nAnRgreaternAR))


rownames(booksResults) <- c("Books: AR$>$A$\\neg$R",
                            "Books: AR$>\\neg$AR",
                            "Books: $\\neg$A$\\neg$R$>$A$\\neg$R",
                            "Books: $\\neg$A$\\neg$R$>\\neg$AR")

booksResults

booksResultsLaTeX <- tableLaTeX(booksResults)
#booksResultsLaTeX



