
AllCoherences <- rbind(BeatlesTable,booksTable,depthTable,DodecahedronTable,
                       DunnitTable,JapaneseSwordsTable,penguinsTable,robbersTable,witnessTable
)


#AllCoherences

AllCoherencesLaTeX <- tableLaTeX(AllCoherences)


AllResults <- rbind(BeatlesResults,booksResults,depthResults,DodecahedronResults,DunnitResults,JapaneseSwordsResults,penguinsResults,robbersResults,witnessResults)

AllResultsLaTeX <- tableLaTeX(AllResults)

AllResultsLaTeX


#success rate 
success <- colSums(AllResults,na.rm=TRUE)/nrow(AllResults)
#success
