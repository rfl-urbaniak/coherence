#JUST TWO NODES


Dod2DAG <- model2network("[TF|T][T]")

graphviz.plot(Dod2DAG)
T2Prob <- priorCPT(node = "T", prob1 = 1/6)
T2Prob
TF2Prob <- singleCPT(eNode = "TF",hNode = "T", probEifHS1 = 1, probEifHS2 = 1/5)
TF2Prob

Dod2RegularCPT <-  list(T=T2Prob,TF=TF2Prob)
Dod2RegularBN <- custom.fit(Dod2DAG,Dod2RegularCPT)

Dod2RegularJN <- compile(as.grain(Dod2RegularBN))
querygrain(Dod2RegularJN, nodes = "TF")



T2dodProb <- priorCPT(node = "T", prob1 = 1/12)
TF2dodProb <- singleCPT(eNode = "TF",hNode = "T", probEifHS1 = 1, probEifHS2 = 1/11)

Dod2DodCPT <-  list(T=T2dodProb,TF=TF2dodProb)
Dod2DodBN <- custom.fit(Dod2DAG,Dod2DodCPT)








#The scenario:You’re either tossing a regular die, or a dodecahedron,X is the result.  Consider the coherence of:{X= 2,(X= 2∨X= 4)}. 
#The desideratum: coherence should not change, whether it's a fair die or a dodecahedron

DodDAG <- model2network("[O][T|O][TF|O]")

#graphviz.plot(DodDAG)

#REGULAR DIE
#Outcome 
Oprob <- array(rep(1/6,6), dim = 6, dimnames = list(O =  1:6))


#The result is a Two
Tprob <- array(c(0,1,1,0,0,1,
             0,1,0,1,0,1), dim = c(2,6),
                  dimnames = list(T = c("1","0"), O = 1:6))


#The result is either a Two or a Four
TFprob <- array(c(0,1,1,0,0,1,
             1,0,0,1,0,1), dim = c(2,6),
           dimnames = list(TF = c("1","0"), O = 1:6))


#build BN
RegularCPT <- list(O=Oprob,T=Tprob,TF=TFprob)
RegularCPT

RegularBN <- custom.fit(DodDAG,RegularCPT)

#graphviz.chart(RegularBN,type="barprob")

#JN <- compile(as.grain(RegularBN))

#querygrain(JN,nodes = c("TF","O"), type = "joint")

#Dodecahedron
DOprob <- array(rep(1/12,12), dim = 12, dimnames = list(O =  1:12))

#DOprob

DTprob <- array(c(0,1,1,0,0,1,
                 0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1), dim = c(2,12),
               dimnames = list(T = c("1","0"), O = 1:12))
#DTprob

DTFprob <- array(c(0,1,1,0,0,1,
                  1,0,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1), dim = c(2,12),
                dimnames = list(TF = c("1","0"), O = 1:12))

#DTFprob

DodecahedronCPT <- list(O=DOprob,T=DTprob,TF=DTFprob)

#DodecahedronCPT

DodecahedronBN <- custom.fit(DodDAG,DodecahedronCPT)

#graphviz.chart(DodecahedronBN,type="barprob")


#____ let's put them in one network using 

#same idea, but either draw out of three or out of four


#L: lower number of option (3 vs 4), TT: two or three

D4DAG <- model2network("[L][O1|L][O2|L][O3|L][O4|L][TT|O1:O2:O3:O4][C|O1:O2:O3:O4]")

LprobD4 <- priorCPT(node = "L",state1 = "1", state2 = "0", prob1 = .5)
Prob1 <- singleCPT(eNode = "O1", hNode = "L", probEifHS1 = 1/3, probEifHS2 = 1/4)
Prob2 <- singleCPT(eNode = "O2", hNode = "L", probEifHS1 = 1/3, probEifHS2 = 1/4)
Prob3 <- singleCPT(eNode = "O3", hNode = "L", probEifHS1 = 1/3, probEifHS2 = 1/4)
Prob4 <- singleCPT(eNode = "O4", hNode = "L", probEifHS1 = 1/3, probEifHS2 = 1/4)

#constraint node
Cprob <- array(c(0,  1,  0, 1, 
                  0,  1,  0,  1,  
                  0, 1, 0, 1,
                  0, 1, 1, 0,
                  0, 1, 0, 1, 
                  0, 1, 1, 0,
                  0, 1, 1, 0,
                  1, 0, 0, 1),
               dim = c(2,2,2,2,2), dimnames = list (C = c("1","0"),O1 = c("1","0"), O2 = c("1","0"), O3 = c("1","0"), O4 = c("1","0")))

#Cprob

#two or three
TTprob <- array(c(1,  0,  1, 0,  
                  1,  0,  1,  0,  
                  1, 0, 1, 0,  
                  0, 1, 0, 1, 
                  1, 0, 1, 0, 
                  1, 0, 1, 0, 
                  1, 0, 1, 0,
                  0, 1, 0, 1), 
                dim = c(2,2,2,2,2), dimnames = list(TT = c("1","0"),O1 = c("1","0"), O2 = c("1","0"), O3 = c("1","0"), O4 = c("1","0")))
#TTprob

D4CPT <- list(L=LprobD4, O1 = Prob1, O2 = Prob2, O3 = Prob3, O4 = Prob4, C = Cprob, TT = TTprob)
D4BN <- custom.fit(D4DAG,D4CPT)



##with priors .1 and .9
LprobD4.1 <- priorCPT(node = "L",state1 = "1", state2 = "0", prob1 = .1)
LprobD4.9 <- priorCPT(node = "L",state1 = "1", state2 = "0", prob1 = .9)
D4CPT.1 <- list(L=LprobD4.1, O1 = Prob1, O2 = Prob2, O3 = Prob3, O4 = Prob4, C = Cprob, TT = TTprob)
D4BN.1 <- custom.fit(D4DAG,D4CPT.1)
D4CPT.9 <- list(L=LprobD4.9, O1 = Prob1, O2 = Prob2, O3 = Prob3, O4 = Prob4, C = Cprob, TT = TTprob)
D4BN.9 <- custom.fit(D4DAG,D4CPT.9)




### Simple two-node network

Dice2DAG <- model2network("[O24][O2|O24]")
#graphviz.plot(Dice2DAG)


O24probSix <- priorCPT("O24",prob = 2/6)
O24probDod <- priorCPT("O24",prob = 2/12)

O2prob <- singleCPT("O2","O24", probEifHS1 = 1/2, probEifHS2 = 0)


Dice2SixCPT <- list(O2 = O2prob, O24 = O24probSix)
Dice2DodCPT <- list(O2 = O2prob, O24 = O24probDod)



Dice2SixBN <- custom.fit(Dice2DAG,Dice2SixCPT)
Dice2DodBN <- custom.fit(Dice2DAG,Dice2DodCPT)











