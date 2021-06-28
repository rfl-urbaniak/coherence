#Define DAG

# W1 - Witness no. 1: ‘‘Steve did it’’
# W2 - Witness no. 2: ‘‘Steve did it’’
# W3 - Witness no. 3: ‘‘Steve, Martin or David did it’’
# W4 - Witness no. 4: ‘‘Steve, John or James did it’’
# W5 - Wittness no. 5: ‘‘Steve, John or Peter did it’’
# D - who did it

WitnessDAG <- model2network("[D][W1|D][W2|D][W3|D][W4|D][W5|D]")
#graphviz.plot(WitnessDAG)

#Define CPTS

DStates <- c('Steve', 'Martin', 'David', 'John', 'James', 'Peter')
dimnames <- list(DStates)
names(dimnames) <- 'D' 
DProb <-  array(c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6),
                dim = 6, dimnames = dimnames)
# DProb

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
# The probability of  a witness saying they saw him do it - around .8
# (although see a discussion of eyewitnesses in our LPR paper);
# if he is innocent, say 0.5%
# probability of a disjunctive claim (e.g. ‘‘Steve, Martin or David did it’’) 
# if one ot them did it - not sure, around .8 as well?
W1Prob <- witnessCPT('W1', .8, .05, .05, .05, .05, .05)
W2Prob <- witnessCPT('W2', .8, .05, .05, .05, .05, .05)
W3Prob <- witnessCPT('W3', .8, .8, .8, .05, .05, .05)
W4Prob <- witnessCPT('W4', .8, .05, .05, .8, .8, .05)
W5Prob <- witnessCPT('W5', .8, .05, .05, .8, .05, .8)

#WitnessCPT
WitnessCPT <-list(D=DProb, W1=W1Prob, W2=W2Prob, W3=W3Prob, W4=W4Prob, W5=W5Prob)

#WitnessBN
WitnessBN <- custom.fit(WitnessDAG,WitnessCPT)


#___________________________RESTRICTED BNs


## Three-node scenario BNS


#graphviz.plot(W12DAG)

W12DAG <- model2network("[D][W1|D][W2|D]")
W12CPT <-list(D=DProb, W1=W1Prob, W2=W2Prob)
W12BN <- custom.fit(W12DAG,W12CPT)


W34DAG <- model2network("[D][W3|D][W4|D]")
W34CPT <-list(D=DProb, W3=W3Prob, W4=W4Prob)
W34BN <- custom.fit(W34DAG,W34CPT)

W45DAG <- model2network("[D][W4|D][W5|D]")
W45CPT <-list(D=DProb, W4=W4Prob, W5=W5Prob)
W45BN <- custom.fit(W45DAG,W45CPT)

#W12BN




#graphviz.plot(W12DAG)



# The probability of  a witness saying they saw him do it - around .8
# (although see a discussion of eyewitnesses in our LPR paper);
# if he is innocent, say 0.5%
# probability of a disjunctive claim (e.g. ‘‘Steve, Martin or David did it’’) 
# if one ot them did it - not sure, around .8 as well?
#W1Prob <- witnessCPT('W1', .8, .05, .05, .05, .05, .05)
#W2Prob <- witnessCPT('W2', .8, .05, .05, .05, .05, .05)
#W3Prob <- witnessCPT('W3', .8, .8, .8, .05, .05, .05)
#W4Prob <- witnessCPT('W4', .8, .05, .05, .8, .8, .05)
#W5Prob <- witnessCPT('W5', .8, .05, .05, .8, .05, .8)

#WitnessCPT

#WitnessBN
























##Build restricted BNs with no extra nodes


W1DAG <- model2network("[W1][W2|W1]")
W3DAG <- model2network("[W3][W4|W3]")
W4DAG <- model2network("[W4][W5|W4]")


W1prior <- priorCPT(node = "W1", prob1 = 0.175)
W3prior <- priorCPT(node = "W3", prob1 = 0.425)
W4prior <- priorCPT(node = "W4", prob1 = 0.425)


W2ifW1 <- singleCPT(eNode = "W2",hNode = "W1", probEifHS1 = 0.621, probEifHS2 = 0.378)
W4ifW3 <- singleCPT(eNode = "W4",hNode = "W3", probEifHS1 = 0.314, probEifHS2 = 0.685)
W5ifW4 <- singleCPT(eNode = "W5",hNode = "W4", probEifHS1 = 0.535, probEifHS2 = 0.464)


W1Cpt <- list(W1=W1prior, W2=W2ifW1)
W3Cpt <- list(W3=W3prior, W4=W4ifW3)
W4Cpt <- list(W4=W4prior, W5=W5ifW4)


W1BN <- custom.fit(W1DAG,W1Cpt)
W3BN <- custom.fit(W3DAG,W3Cpt)
W4BN <- custom.fit(W4DAG,W4Cpt)



















#experiment with arrow strength





#______________________  SEPARATE BNs for the scenarios

#W1W2
W1W2DAG <- model2network("[D][W1|D][W2|D]")
#graphviz.plot(W1W2DAG)

#Define CPTS

DStates <- c('Steve', 'Martin', 'David', 'John', 'James', 'Peter')
dimnames <- list(DStates)
names(dimnames) <- 'D' 
DProb <-  array(c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6),
                dim = 6, dimnames = dimnames)
# DProb

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

W1ProbW1W2 <- witnessCPT('W1', .8, .05, .05, .05, .05, .05)
W2ProbW1W2 <- witnessCPT('W2', .8, .05, .05, .05, .05, .05)

#WitnessCPT
W1W2CPT <-list(D=DProb, W1=W1ProbW1W2, W2=W2ProbW1W2)

#WitnessBN
W1W2BN <- custom.fit(W1W2DAG,W1W2CPT)

#________________________________________________________

#W3W4

W3W4DAG <- model2network("[D][W3|D][W4|D]")
#graphviz.plot(WitnessDAG)

#Define CPTS

DStates <- c('Steve', 'Martin', 'David', 'John', 'James', 'Peter')
dimnames <- list(DStates)
names(dimnames) <- 'D' 
DProb <-  array(c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6),
                dim = 6, dimnames = dimnames)
# DProb

2/5

W3ProbW3W4 <- witnessCPT('W3', .8, .8, .8, 2/5, 2/5, 2/5)
W4ProbW3W4 <- witnessCPT('W4', .8, .2/5, 2/5, .8, .8, .2/5)

#WitnessCPT
W3W4CPT <-list(D=DProb,  W3=W3ProbW3W4, W4=W4ProbW3W4)

#WitnessBN
W3W4BN <- custom.fit(W3W4DAG,W3W4CPT)
#___________________________________________________________________________

#W4W5

W4W5DAG <- model2network("[D][W4|D][W5|D]")
#graphviz.plot(WitnessDAG)

#Define CPTS

DStates <- c('Steve', 'Martin', 'David', 'John', 'James', 'Peter')
dimnames <- list(DStates)
names(dimnames) <- 'D' 
DProb <-  array(c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6),
                dim = 6, dimnames = dimnames)
# DProb

W4ProbW4W5 <- witnessCPT('W4', .8, 2/5, 2/5, .8, .8, 2/5)
W5ProbW4W5 <- witnessCPT('W5', .8, 2/5, 2/5, .8, 2/5, .8)

W4ProbW4W5

#WitnessCPT
W4W5CPT <-list(D=DProb,  W4=W4ProbW4W5, W5=W5ProbW4W5)

#WitnessBN
W4W5BN <- custom.fit(W4W5DAG,W4W5CPT)
































#______________________ Updates to W1 and to W3

WitnessJN <- compile(as.grain(WitnessBN))

WitnessJN1 <- setEvidence(WitnessJN, nodes = "W1", states = "1")

WitnessBN1 <- as.bn.fit(WitnessJN1,including.evidence = TRUE)







#have a look 
#graphviz.chart(WitnessBN,type="barprob")

#explore judge and one witness


#W8DAG <- model2network("[C][W|C]")
#graphviz.plot(W8DAG)


