
AllCoherences <- rbind(BeatlesTable,booksTable,depthTable,dodecahedronTable,
                       DunnitTable,JapaneseSwordsTable,penguinsTable,robbersTable,witnessTable
)


#AllCoherences



AllCoherencesLaTeX <- tableLaTeX(AllCoherences)


AllCoherencesLaTeX 



AllResults <- rbind(BeatlesResults,booksResults,depthResults,DodecahedronResults,DunnitResults,
                    JapaneseSwordsResults,penguinsResults,robbersResults,witnessResults)

#AllResults



AllResultsLaTeX <- tableLaTeX(AllResults)

AllResultsLaTeX


#success rate 
#colMeans(AllResults, na.rm = TRUE, dims = 1)

