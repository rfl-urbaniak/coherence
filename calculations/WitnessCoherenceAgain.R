#define D and its prob
DStates <- c('Steve', 'Martin', 'David', 'John', 'James', 'Peter')
dimnames <- list(DStates)
names(dimnames) <- 'D' 
DProb <-  array(c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6),
                dim = 6, dimnames = dimnames)


#function for defining witness nodes
witnessCPT <- function(eNode, probEifHS1, probEifHS2,probEifHS3, 
                       probEifHS4,probEifHS5, probEifHS6){
  eStates <- c(1,0)
  dimnames <- list(eStates,DStates)
  names(dimnames) <- c(eNode,'D')
  array(
    c(
      probEifHS1, as.numeric(1-probEifHS1),
      probEifHS2, as.numeric(1-probEifHS2),
      probEifHS3, as.numeric(1-probEifHS3),
      probEifHS4, as.numeric(1-probEifHS4),
      probEifHS5, as.numeric(1-probEifHS5),
      probEifHS6, as.numeric(1-probEifHS6)
    ), 
    dim = c(2,6),
    dimnames = dimnames
  )
}


W1Prob <- witnessCPT('W1', .8, .05, .05, .05, .05, .05)
W2Prob <- witnessCPT('W2', .8, .05, .05, .05, .05, .05)
W3Prob <- witnessCPT('W3', .8, .8, .8, .05, .05, .05)
W4Prob <- witnessCPT('W4', .8, .05, .05, .8, .8, .05)
W5Prob <- witnessCPT('W5', .8, .05, .05, .8, .05, .8)

### Define separate BNs


W12DAG <- model2network("[D][W1|D][W2|D]")
W12CPT <-list(D=DProb, W1=W1Prob, W2=W2Prob)
W12BN <- custom.fit(W12DAG,W12CPT)

# W12BN
# 
# structuredCoherenceNarration(W12BN,c("W1","W2"),c("1","1"))
# 
# structuredCoherenceNarration(W12BN,A,c("1","1"))[[1]][[1]][[2]]
# 
# structuredCoherenceUpdated(W12BN,c("W1","W2"),c("1","1")) 
# 


W34DAG <- model2network("[D][W3|D][W4|D]")
W34CPT <-list(D=DProb, W3=W3Prob, W4=W4Prob)
W34BN <- custom.fit(W34DAG,W34CPT)




W45DAG <- model2network("[D][W4|D][W5|D]")
W45CPT <-list(D=DProb, W4=W4Prob, W5=W5Prob)
W45BN <- custom.fit(W45DAG,W45CPT)

#####


A <- c("W1","W2")
B <- c("W3","W4")
C <- c("W4","W5")

W12Table <- CoherencesTableNarr(W12BN,
                                scenariosList = list(A),
                                statesList   = list(c("1","1")),
                                exampleName = "Witness"
)

#W12Table


W34Table <- CoherencesTableNarr(W34BN,
                                scenariosList = list(B),
                                statesList   = list(c("1","1")),
                                exampleName = "Witness"
)


#W34Table

W45Table <- CoherencesTableNarr(W45BN,
                                scenariosList = list(C),
                                statesList   = list(c("1","1")),
                                exampleName = "Witness"
)


WTable <- as.data.frame(rbind(W12Table, 
                                  W34Table,
                                  W45Table
))


WTable

W1W2greaterW3W4 <- WTable[1,] > WTable[2,] 
W4W5greaterW3W4 <- WTable[3,] > WTable[2,] 
WResults <- as.data.frame(rbind(W1W2greaterW3W4,W4W5greaterW3W4))
rownames(WResults) <- c("Witness: W$_1$W$_2>$W$_3$W$_4$","Witness: W$_4$W$_5>$W$_3$W$_4$")

WResults



BN <- W34BN
structuredNoSD(W34BN,c("W3","W4","D"),c("1","1","Steve"))
structuredNoSD(W34BN,c("W3","W4","D"),c("1","1","Martin"))
structuredNoSD(W34BN,c("W3","W4","D"),c("1","1","Peter"))


BN <- W45BN
structuredNoSD(W45BN,c("W4","W5","D"),c("1","1","Steve"))
structuredNoSD(W45BN,c("W4","W5","D"),c("1","1","John"))
structuredNoSD(W45BN,c("W4","W5","D"),c("1","1","Peter"))
structuredNoSD(W45BN,c("W4","W5","D"),c("1","1","David"))


