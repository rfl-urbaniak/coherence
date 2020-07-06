#The scenario:You’re either tossing a regular die, or a dodecahedron,Xisthe result.  Consider the coherence of:{X= 2,(X= 2∨X= 4)}. 
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
RegularBN <- custom.fit(DodDAG,RegularCPT)

#graphviz.chart(RegularBN,type="barprob")

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
