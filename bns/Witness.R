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

#have a look 
#graphviz.chart(WitnessBN,type="barprob")
