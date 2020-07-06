#----------- ok, let's try this DAG
#Define DAG
BirdDAG <- model2network("[G][P|G:B][B|G]")

#graphviz.plot(BirdDAG)


#Define CPTS
GProb <-  priorCPT("G",prob1 = .5)
#GProb  
#btw, am I right thinking that they take the probability of being a bird .5? That's so unrealistic!

BProb <- singleCPT(eNode = "B",hNode = "G", probEifHS1 = .02, probEifHS2 = .98)  
#BProb
  
#why TF the probability of being a penguin is 1 if you're G and B? What the hell did you do to chickens?? 
# ate them. Not my Venn diagram:P
PProb <- doubleCPT(eNode = "P",h1Node = "B",h2Node = "G",probEifH1S1H2S1 =  1, 
                probEifH1S1H2S2 = 0, probEifH1S2H2S1 = 0, probEifH1S2H2S2 = 0)
#PProb


#BirdCPT
BirdCPT <-list(B=BProb,G=GProb,P=PProb)

#BirdBN
BirdBN <- custom.fit(BirdDAG,BirdCPT)

#have a look 
#graphviz.chart(BirdBN,type="barprob")

## Stuff below used to play around and test
## Now calculate relevant probabilities
#compile as grain and compute joint probs
#BirdJN <- compile(as.grain(BirdBN))
#this generates joint probabilities, they seem alright  in comparison to Deuven & Meijs2007
#BNjoints <- querygrain(BirdJN,nodes=c("G","B","P"),type="joint")
#BNjoints
#testJN <- setEvidence(BirdJN , nodes = c("G"), states  = c("1"))
#querygrain(testJN, nodes = c("P","B"), type = "joint")
# example of referring to particular joint probabilities
#BNjoints[G="0",B="1",P="0"]
#Let's see if this gets the conditional probabilities right
# grounded  if penguin should be 1.
#Penguin <- setEvidence(BirdJN,nodes = "P",states = "1")
#querygrain(Penguin,nodes = "G")
#seems fine
#now bird if penguin. Should be 1 too.
#querygrain(Penguin,nodes = "B")
#seems fine too
#now set evidence to bird
#Bird <- setEvidence(BirdJN,nodes = "B",states = "1")
#penguin if bird should be 0.02
#querygrain(Bird,nodes = "P")
#fine
#grounded if bird should be 0.02 too 
#querygrain(Bird,nodes = "G")
#ok. So the network seems to work.

