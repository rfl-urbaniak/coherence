#source("measures//CoherenceMeasures2Var.R")
#source("measures//Fitelson.R")
#source("measures//RA.R")
#source("bns//Robbers.R")

#source("..//measures//Fitelson.R")

robbersTable <- CoherencesTable(robbersBN,
        scenariosList = list(c("MIsP","MIsR"),c("MIsP","MIsR"),c("MIsP","MIsR")),
        statesList   = list(c("1","1"),c("1","0"),c("0","1")),
        exampleName = "Robbers"
)

#robbersTable

robbersTableLaTeX <- tableLaTeX(robbersTable)

robbersTableLaTeX

#d,f, o, ra, r,
neutralPoints <- c(0,0 ,NA, 0.5, .5, 1)

PRgreaterPnR <- robbersTable[1,] > robbersTable[2,] 
#PRgreaterPnR

PRgreaterNeutral <- robbersTable[1,] > neutralPoints

robbersResults <- as.data.frame(rbind(PRgreaterPnR,PRgreaterNeutral))

rownames(robbersResults) <- c("Robbers: PR$>$P$\\neg$R","Robbers: PR$>$neutral")

robbersResults

robbersResultsLaTeX <- tableLaTeX(robbersResults)

#robbersResultsLaTeX

