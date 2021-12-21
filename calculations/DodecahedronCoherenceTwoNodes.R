Dod2DAG <- model2network("[T][TF|T]")



#graphviz.plot(Dod2DAG)


TprobReg2 <- array(c(1/6,5/6), dim = 2, dimnames = list(T =  c("1","0")))


TFprobReg2 <- singleCPT(eNode = "TF",hNode = "T", probEifHS1 = 1, probEifHS2 = 1/5) 



TprobDod2 <- array(c(1/12,11/12), dim = 2, dimnames = list(T =  c("1","0")))

TFprobDod2 <- singleCPT(eNode = "TF",hNode = "T", probEifHS1 = 1, probEifHS2 = 1/11) 


Dod2RegCPT <- list(T=TprobReg2,TF=TFprobReg2)

Dod2DodCPT <- list(T=TprobDod2,TF=TFprobDod2)



Dod2RegBN <-  custom.fit(Dod2DAG,Dod2RegCPT)
Dod2DodBN <-  custom.fit(Dod2DAG,Dod2DodCPT)

CoherencesTable(Dod2RegBN,
                    scenariosList = list(c("T","TF")),
                    statesList   = list(c("1","1")),
                    exampleName = "Regular"
)


CoherencesTable(Dod2DodBN,
                scenariosList = list(c("T","TF")),
                statesList   = list(c("1","1")),
                exampleName = "Dodecahedron"
)


