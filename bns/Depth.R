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

graphviz.plot(DepthBN)

JN <- compile(as.grain(DepthBN))

JNevc <- 


#graphviz.chart(DepthBN,type="barprob")