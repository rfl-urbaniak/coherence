#----------------------------------
## NOW WITH THREE STATES OF THE ROOT

robbersDAG <- model2network("[WhoMurdered][MIsP|WhoMurdered][MIsR|WhoMurdered]")

#Define CPTS

whoMurderedDimnames <- c("OnlyP","OnlyR","Both")

whoMurderedProb <-  array(c(0.2,0.2,0.6), dim = 3,
                           dimnames = list(WhoMurdered =  whoMurderedDimnames))


MIsPProb <-  array(c(1,0,0,1,1,0), dim = c(2,3),dimnames = list(MIsP = c("1","0"), WhoMurdered = whoMurderedDimnames))


MIsRProb <-  array(c(0,1,1,0,1,0), dim = c(2,3),dimnames = list(MIsR = c("1","0"), WhoMurdered = whoMurderedDimnames))




robbersCPT <-list(WhoMurdered=whoMurderedProb,MIsP=MIsPProb,MIsR=MIsRProb)

#PickpocketsAndRobberers3CPT

robbersBN <- custom.fit(robbersDAG,robbersCPT)


#graphviz.plot(robbersDAG)

#graphviz.chart(RobbersBN,type="barprob")
