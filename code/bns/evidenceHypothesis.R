ehDAG <- model2network("[H][E|H]")
graphviz.plot(ehDAG)

HProb <-  priorCPT("H",prob1 = .4)
EProb <- singleCPT(eNode = "E",hNode = "H", probEifHS1 = .6, probEifHS2 = .2)  

ehCPT <-list(H=HProb,E=EProb)


ehBN <- custom.fit(ehDAG,ehCPT)


ehBN

graphviz.plot(ehBN)


pEraw <- .6 * .4 + .2 * .6
pEraw
LREraw <- .6/.2
LREraw

structuredLR(ehBN,"E","1")


library(rethinking)
library(pracma)

x <- seq(0,500,.01)
y <- erf(x)



logistic(1)

plot(x,y)



ggplot()+geom_line(aes(x = x, y = y))+geom_line()

new <- ifelse (x <= 1, x -1, NA)

new






