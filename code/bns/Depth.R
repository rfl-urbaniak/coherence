DepthDAG <- model2network("[C][T123|C][T124|C][T134|C][T145|C][T167|C]")

#graphviz.plot(DepthDAG)

Cprob <- array(rep(1/8,8), dim = 8, dimnames = list(C =  1:8))
#Cprob


T123prob <- array(c(1,0,1,0,1,0,0,1,0,1,0,1,0,1,0,1), dim = c(2,8),dimnames = list(T123 = c("1","0"), C = 1:8))

#T123prob

T124prob <- array(c(1,0,1,0,0,1,1,0,0,1,0,1,0,1,0,1), dim = c(2,8),dimnames = list(T124 = c("1","0"), C = 1:8))

#T124prob

T134prob <- array(c(1,0,0,1,1,0,1,0,0,1,0,1,0,1,0,1), dim = c(2,8),dimnames = list(T134 = c("1","0"), C = 1:8))

#T134prob

T145prob <- array(c(1,0,0,1,0,1,1,0,1,0,0,1,0,1,0,1), dim = c(2,8),dimnames = list(T145 = c("1","0"), C = 1:8))

#T145prob


T167prob <- array(c(1,0,0,1,0,1,0,1,0,1,1,0,1,0,0,1), dim = c(2,8),dimnames = list(T167 = c("1","0"), C = 1:8))

#T167prob



DepthCPT <- list(C=Cprob,T123=T123prob,
                 T124=T124prob,
                 T134=T134prob,
                 T145=T145prob,
                 T167=T167prob)


DepthBN <- custom.fit(DepthDAG,DepthCPT)

#DepthBN


## Narrated three BNs

DX1DAG <- model2network("[C][T123|C][T124|C][T134|C]")
#graphviz.plot(DX1DAG)
DX1CPT <-list(C=Cprob, T123=T123prob, T124=T124prob, T134= T134prob)
DX1BN <- custom.fit(DX1DAG,DX1CPT)
#DX1BN


DX2DAG <- model2network("[C][T123|C][T145|C][T167|C]")
#graphviz.plot(DX2DAG)
DX2CPT <-list(C=Cprob, T123=T123prob, T145=T145prob, T167= T167prob)
DX2BN <- custom.fit(DX2DAG,DX2CPT)
#DX2BN




## Witness' perspective


T123DAG <- model2network("[T123][T124|T123]")
#graphviz.plot(T123DAG)

T123Apr <- priorCPT("T123",prob1 = .98)

T124Apr <- singleCPT(eNode = "T124", hNode = "T123", probEifHS1 = 2/3, probEifHS2 = 1/5)

T123CPT <- list(T123 = T123Apr, T124 = T124Apr)

T123BN <- custom.fit(T123DAG,T123CPT)


T124Apr


T123BN

#__________________________________

T123DAG2 <- model2network("[T123][T145|T123]")
#graphviz.plot(T123DAG)

T123Bpr <- priorCPT("T123",prob1 = .98)

T145Bpr <- singleCPT(eNode = "T145", hNode = "T123", probEifHS1 = 1/3, probEifHS2 = 2/5)


T123CPT2 <- list(T123 = T123Bpr, T145 = T145Bpr)

T123BN2 <- custom.fit(T123DAG2,T123CPT2)


T124Apr




T123BN
