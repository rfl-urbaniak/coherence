
AllCoherences <- rbind(BeatlesTable,booksTable,depthTable,dodecahedronTable,
                       DunnitTable,JapaneseSwordsTable,penguinsTable,robbersTable,witnessTable
)


#AllCoherences

AllCoherencesRounded <- AllCoherences %>%  mutate_if(is.numeric, round, digits = 3)


AllCoherencesLaTeX <- tableLaTeX(AllCoherences)
AllCoherencesRoundedLaTeX <- tableLaTeX(AllCoherencesRounded)



#AllCoherencesRoundedLaTeX 



AllResults <- rbind(BeatlesResults,booksResults,depthResults,DodecahedronResults,DunnitResults,
                    JapaneseSwordsResults,penguinsResults,robbersResults,witnessResults)

#AllResults



AllResultsLaTeX <- tableLaTeX(AllResults)

#AllResultsLaTeX


#success rate 
#colMeans(AllResults, na.rm = TRUE, dims = 1)

