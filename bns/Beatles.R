#Define DAG

# D: exactly 1 is dead
# J: J is alive
# P: P is alive
# G: G is alive
# R: R is alive
BeatlesDAG <- model2network("[J][P][G][R][D|J:P:G:R]")
#graphviz.plot(BeatlesDAG)

#Define CPTS
GProb <-  priorCPT("G",prob1 = .5)
JProb <-  priorCPT("J",prob1 = .5)
PProb <-  priorCPT("P",prob1 = .5)
RProb <-  priorCPT("R",prob1 = .5)

dimnames <- list(c("1","0"),c("1","0"),c("1","0"),c("1","0"),c("1","0"))
names(dimnames) <- c("D","G","J","P","R")
DProb <- array(
                c(
                  0,1,1,0,  1,0,0,1,
                  1,0,0,1,  0,1,0,1,
                  1,0,0,1,  0,1,0,1,
                  0,1,0,1,  0,1,0,1
                ),
                dim = c(2,2,2,2,2), dimnames = dimnames
               )
DProb

#BeatlesCPT
BeatlesCPT <-list(G=GProb, J=JProb, P=PProb, R=RProb, D=DProb)

#BeatlesBN
BeatlesBN <- custom.fit(BeatlesDAG,BeatlesCPT)

#BeatlesBN

#have a look 
#graphviz.chart(BeatlesBN,type="barprob")
