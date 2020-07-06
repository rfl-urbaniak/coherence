#I: incriminating witness evidence
#M: motive
#W: another witness seeing someone looking like Dunnit somewhere else at the time
#G: Dunnit being guilty
#T: Dunnit has an identical twin seen somewhere else (same location as in W) at the time
DunnitDAG <- model2network("[M][T][I|G][G|M][W|G:T]")

#graphviz.plot(DunnitDAG)

#say the prior prob of him having a motive  non-negligible, but not very high
Mprob <- priorCPT(node = "M", prob1 = .4)

#Mprob

# The probability of Dunnit having a twin is say close to real frequency, 3.4%
Tprob <- priorCPT(node = "T", prob1 = .034)

#Tprob


#The probability of guilt given motive, non-negligible, but rather low; most people with a motive don't commit a murder; say 5%; the probability of murdering without a motive should be even lower, say .5\%

Gprob <- singleCPT(eNode = "G",hNode = "M", probEifHS1 = .05, probEifHS2 = .005) 

#Gprob


#The probability of incriminating evidence in the form of a witness saying they saw him do it. Say around .8
# (although see a discussion of eyewitnesses in our LPR paper); pretty low, however, if he is innocent, say 0.5%

Iprob <- singleCPT(eNode = "I",hNode = "G", probEifHS1 = .8, probEifHS2 = .005)

#Iprob

#The probability of a witness saying someone who looks like Dunnit somewhere else at that time:
#- no twin, guilty <- very low, say .5%
#- twin, guilty <- higher, because he could've seen the twin, say  20%
#- twin, not guilty <- even higher, because he could've seen Dunnit or the twin, say 40%
#- no twin, not guilty <- say like the probability of seeing the twin, 20%


Wprob <- doubleCPT(eNode = "W",h1Node = "T",h2Node = "G",probEifH1S1H2S1 =  .2, 
                   probEifH1S1H2S2 = .4, probEifH1S2H2S1 = 0.005, probEifH1S2H2S2 = .2)

#Wprob
  
#DunnitCPT
DunnitCPT <-list(M=Mprob,T=Tprob,G=Gprob, I = Iprob, W = Wprob)

#DunnitCPT

#DunnitBN
DunnitBN <- custom.fit(DunnitDAG,DunnitCPT)

#have a look 
#graphviz.chart(DunnitBN,type="barprob")
