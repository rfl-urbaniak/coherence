
load("calculations/RdataObjects/BeatlesTable2.Rda")
load("calculations/RdataObjects/booksTable2.Rda")
load("calculations/RdataObjects/depthTable2.Rda")
load("calculations/RdataObjects/dodecahedronTable2.Rda")
load("calculations/RdataObjects/DunnitTable2.Rda")
load("calculations/RdataObjects/JapaneseSwordsTable2.Rda")
load("calculations/RdataObjects/penguinsTable2.Rda")
load("calculations/RdataObjects/robbersTable2.Rda")
load("calculations/RdataObjects/witnessTable2.Rda")




load("calculations/RdataObjects/BeatlesResults2.Rda")
load("calculations/RdataObjects/booksResults2.Rda")
load("calculations/RdataObjects/depthResults2.Rda")
#load("calculations/RdataObjects/dodecahedronResults.Rda")
load("calculations/RdataObjects/DunnitResults2.Rda")
load("calculations/RdataObjects/JapaneseSwordsResults2.Rda")
load("calculations/RdataObjects/penguinsResults2.Rda")
load("calculations/RdataObjects/robbersResults2.Rda")
load("calculations/RdataObjects/witnessResults2.Rda")






load("calculations/RdataObjects/BeatlesTable3.Rda")
load("calculations/RdataObjects/booksTable3.Rda")
load("calculations/RdataObjects/depthTable3.Rda")
load("calculations/RdataObjects/dodecahedronTable3.Rda")
load("calculations/RdataObjects/DunnitTable3.Rda")
load("calculations/RdataObjects/JapaneseSwordsTable3.Rda")
load("calculations/RdataObjects/penguinsTable3.Rda")
load("calculations/RdataObjects/robbersTable3.Rda")
load("calculations/RdataObjects/witnessTable3.Rda")




load("calculations/RdataObjects/BeatlesResults3.Rda")
load("calculations/RdataObjects/booksResults3.Rda")
load("calculations/RdataObjects/depthResults3.Rda")
#load("calculations/RdataObjects/dodecahedronResults.Rda")
load("calculations/RdataObjects/DunnitResults3.Rda")
load("calculations/RdataObjects/JapaneseSwordsResults3.Rda")
load("calculations/RdataObjects/penguinsResults3.Rda")
load("calculations/RdataObjects/robbersResults3.Rda")
load("calculations/RdataObjects/witnessResults3.Rda")






# AllCoherences <- rbind(BeatlesTable2,booksTable2#,DunnitTable2,JapaneseSwordsTable2,penguinsTable2,robbersTable2
#                        #depthTable2
#                        #dodecahedronTable,
#                        #witnessTable2
#                        )
# 
# AllCoherencesRounded <- AllCoherences %>%  mutate_if(is.numeric, round, digits = 3)
# rownames(AllCoherencesRounded) <- rownames(AllCoherences)
# 


CoherencesFinal <- rbind(BeatlesTable3,booksTable3, DunnitTable3,JapaneseSwordsTable3,penguinsTable3,robbersTable3) 
CoherencesFinalR <- CoherencesFinal %>% mutate_if(is.numeric, round, digits = 3)
rownames(CoherencesFinalR) <- rownames(CoherencesFinal)
CoherencesInitialR <- CoherencesFinalR[,-9]

save(CoherencesFinal,file="calculations/RdataObjects/CoherencesFinal.Rda")
save(CoherencesFinalR,file="calculations/RdataObjects/CoherencesFinalR.Rda")
save(CoherencesInitialR,file="calculations/RdataObjects/CoherencesInitialR.Rda")



#CoherencesFinal
#CoherencesFinalR


ResultsFinal <- rbind(BeatlesResults3,booksResults3,DunnitResults3,JapaneseSwordsResults3,penguinsResults3,robbersResults3)
ResultsInitial <- ResultsFinal[,-9]
#ResultsInitial
save(ResultsFinal,file="calculations/RdataObjects/ResultsFinal.Rda")
save(ResultsInitial,file="calculations/RdataObjects/ResultsInitial.Rda")




#AllCoherencesRounded

#AllCoherencesLaTeX <- tableLaTeX(AllCoherences)
# AllCoherencesRoundedLaTeX <- tableLaTeX(AllCoherencesRounded)
# 
# 
# 
# AllCoherencesRoundedLaTeX 



AllResults <- rbind(BeatlesResults2,booksResults2,DunnitResults2,JapaneseSwordsResults2,penguinsResults2,robbersResults2,
                    depthResults2,#dodecahedronResults, 
                    witnessResults2)

AllResults



AllResultsLaTeX <- tableLaTeX(AllResults)

AllResultsLaTeX


#success rate 
colMeans(AllResults, na.rm = TRUE, dims = 1)

