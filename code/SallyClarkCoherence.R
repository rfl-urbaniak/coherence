# #installation of packages, if needed
# install.packages("https://www.bnlearn.com/releases/bnlearn_latest.tar.gz", 
#                  repos = NULL, type = "source")
# install.packages("BiocManager")
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install()
# BiocManager::install(c("graph", "Rgraphviz"))
# BiocManager::install(c("graph", "RBGL", "Rgraphviz"))
# install.packages("https://www.bnlearn.com/releases/bnlearn_latest.tar.gz", 
#                  repos = NULL, type = "source")
# install.packages("gRbase", dependencies=TRUE); 
# install.packages("gRain", dependencies=TRUE); 
# install.packages("gRim", dependencies=TRUE)

library(bnlearn)
library(Rgraphviz)
library(gRain)
library(kableExtra)
library(ggplot2)
library(ggthemes)
library(gridExtra)


#check you have utils and measures as sufolders of your working directory
getwd()

#load scripts
source("utils//CombinationsBN.R")
source("utils//CptCreate.R")
source("utils//LogicAndBNs.R")
source("utils//kableCPTs.R")
source("measures//structuredCoherence.R")
source("utils//CoherenceTables.R")
source("measures//Fitelson.R")
source("measures//DouvenMeijs.R")
source("measures//generalizedOlsson.R")
source("measures//Olsson.R")
source("measures//generalizedShogenji.R")
source("measures//Shogenji.R")
source("measures//Roche.R")
#source("measures//RA.R")

#define Z function
Z <- function(posterior,prior){
  d <- posterior - prior
  ifelse(prior == posterior, 0, ifelse(posterior > prior, d/(1-prior), d/prior))
}




#define the structure of the Sally Clark BN
SallyClarkDAG <- model2network("[Abruising|Amurder][Adisease|Amurder][Bbruising|Bmurder][Bdisease|Bmurder][Amurder][Bmurder|Amurder]")

#SallyClarkFullDAG <- model2network("[Abruising|Acause][Adisease|Acause][Bbruising|Bcause][Bdisease|Bcause][Acause][Bcause|Acause][NoMurdered|Acause:Bcause][Guilty|NoMurdered]")

#plot 
png(file="../images/SC.png", 
    units="in", 
    width=6, 
    height=6, 
    pointsize = 1,
    res=400)
graphviz.plot(SallyClarkDAG)
dev.off()

graphviz.plot(SallyClarkFullDAG)


#CPTs as used in Fenton & al.
AmurderProb <-prior.CPT("Amurder","0","1",0.921659)
AbruisingProb <- single.CPT("Abruising","Amurder","1","0","0","1",0.01,0.05)
AdiseaseProb <- single.CPT("Adisease","Amurder","1","0","0","1",0.05,0.001)
BbruisingProb <- single.CPT("Bbruising","Bmurder","1","0","0","1",0.01,0.05)
BdiseaseProb <- single.CPT("Bdisease","Bmurder","1","0","0","1",0.05,0.001)
BmurderProb <- single.CPT("Bmurder","Amurder","0","1","0","1",0.9993604,1-0.9998538)



#E goes first; order: last variable through levels, second last, then first
#NoMurderedProb <- array(c(0, 0, 1, 0, 1, 0, 0,1,0,1,0,0), dim = c(3, 2, 2),dimnames = list(NoMurdered = c("both","one","none"),Bmurder = c("SIDS","Murder"), Amurder = c("SIDS","Murder")))

#this one is definitional
#GuiltyProb <-  array(c( 1,0, 1,0, 0,1), dim = c(2,3),dimnames = list(Guilty = c("Yes","No"), NoMurdered = c("both","one","none")))

# Put CPTs together
SallyClarkCPT <- list(Amurder=AmurderProb,Adisease = AdiseaseProb,
                      Bmurder = BmurderProb,Bdisease=BdiseaseProb,
                      Abruising = AbruisingProb,Bbruising = BbruisingProb)
                      #NoMurdered = NoMurderedProb,Guilty=GuiltyProb)

# join with the DAG to get a BN
SallyClarkBN <- custom.fit(SallyClarkDAG,SallyClarkCPT)


png(file="../images/SCBN.png", 
    units="in", 
    width=6, 
    height=6, 
    pointsize = 1,
    res=400)
graphviz.chart(SallyClarkBN,type="barprob", scale = c(0.7,1.3), main = "Priors in the Sally Clark case")
dev.off()




SCnodes <- c("Amurder","Bmurder")

SCstates <- c("00", "11","01","10")



BN <- SallyClarkBN


structuredNoSD(SallyClarkBN,SCnodes,c("0","0"))

structuredNoSD(SallyClarkBN,SCnodes,c("1","1"))

structuredNoSD(SallyClarkBN,SCnodes,c("0","1"))

structuredNoSD(SallyClarkBN,SCnodes,c("1","0"))


SCstructuredNoSD <- round(c(structuredNoSD(SallyClarkBN,SCnodes,c("0","0"))$structuredNoSD,
                    structuredNoSD(SallyClarkBN,SCnodes,c("1","1"))$structuredNoSD,
                    structuredNoSD(SallyClarkBN,SCnodes,c("0","1"))$structuredNoSD,
                    structuredNoSD(SallyClarkBN,SCnodes,c("1","0"))$structuredNoSD),4)


  
  
#--------------------

SCFitelson <-round(c(FitelsonCoherenceForBNs(SallyClarkBN,SCnodes,c("0","0"))$`Fitelson coherence`,
              FitelsonCoherenceForBNs(SallyClarkBN,SCnodes,c("1","1"))$`Fitelson coherence`,
              FitelsonCoherenceForBNs(SallyClarkBN,SCnodes,c("1","0"))$`Fitelson coherence`,
              FitelsonCoherenceForBNs(SallyClarkBN,SCnodes,c("0","1"))$`Fitelson coherence`),4)

#--------------------

SCDouvenMeijs <- round(c(DouvenMeijsCoherenceForBNs(SallyClarkBN,SCnodes,c("0","0"))$'DM coherence',
                    DouvenMeijsCoherenceForBNs(SallyClarkBN,SCnodes,c("1","1"))$'DM coherence',
                    DouvenMeijsCoherenceForBNs(SallyClarkBN,SCnodes,c("1","0"))$'DM coherence',
                    DouvenMeijsCoherenceForBNs(SallyClarkBN,SCnodes,c("0","1"))$'DM coherence'),4)


#---------------------

SCOlsson <- round(c(OlssonCoherenceForBNs(SallyClarkBN,SCnodes,c("0","0"))$`Olsson coherence`,
OlssonCoherenceForBNs(SallyClarkBN,SCnodes,c("1","1"))$`Olsson coherence`,
OlssonCoherenceForBNs(SallyClarkBN,SCnodes,c("1","0"))$`Olsson coherence`,
OlssonCoherenceForBNs(SallyClarkBN,SCnodes,c("0","1"))$`Olsson coherence`),4)


#---------------------

SCShogenji <- round(c(ShogenjiCoherenceForBNs(SallyClarkBN,SCnodes,c("0","0"))$`Shogenji coherence`,
ShogenjiCoherenceForBNs(SallyClarkBN,SCnodes,c("1","1"))$`Shogenji coherence`,
ShogenjiCoherenceForBNs(SallyClarkBN,SCnodes,c("1","0"))$`Shogenji coherence`,
ShogenjiCoherenceForBNs(SallyClarkBN,SCnodes,c("0","1"))$`Shogenji coherence`),4)


#----------------------
RocheCoherenceForBNs(SallyClarkBN,SCnodes,c("0","0"))
RocheCoherenceForBNs(SallyClarkBN,SCnodes,c("1","1"))
RocheCoherenceForBNs(SallyClarkBN,SCnodes,c("1","0"))
RocheCoherenceForBNs(SallyClarkBN,SCnodes,c("0","1"))


SCRoche <- round(c(RocheCoherenceForBNs(SallyClarkBN,SCnodes,c("0","0"))$`Roche coherence`,
RocheCoherenceForBNs(SallyClarkBN,SCnodes,c("1","1"))$`Roche coherence`,
RocheCoherenceForBNs(SallyClarkBN,SCnodes,c("1","0"))$`Roche coherence`,
RocheCoherenceForBNs(SallyClarkBN,SCnodes,c("0","1"))$`Roche coherence`),4)



#add probabilities, build table
SallyClarkJN <- compile(as.grain(SallyClarkBN))


SCprobs <- round(querygrain(SallyClarkJN, nodes = SCnodes, type = "joint"),4)

SCprobsVector <- c(SCprobs["0","0"],SCprobs["1","1"],SCprobs["1","0"],SCprobs["0","1"])

SCnoEvidence <- data.frame(SCstates,SCstructuredNoSD,SCFitelson,SCDouvenMeijs,SCRoche,SCShogenji,SCOlsson,SCprobsVector)

colnames(SCnoEvidence) <- c("States","Structured","Fitelson","Douven-Meijs","Roche","Shogenji","Olsson-Glass", "Probability")

saveRDS(SCnoEvidence, file = "tables/SCnoEvidence.RDS")

SCnoEvidence %>%  kable(format = "latex",booktabs=T,
                           linesep = "",  escape = FALSE, 
                           caption = "Head of the religion dataset.") %>%  
  kable_styling(latex_options=c("scale_down"))

#updating with evidence

#Stage A: bruising twice, no signs of disease


SallyClarkJNstageA <- setEvidence(SallyClarkJN, nodes = c("Abruising","Bbruising","Adisease","Bdisease"), states = c("1","1","0","0"))

SallyClarkBNstageA <- as.bn.fit(SallyClarkJNstageA, including.evidence = TRUE)

graphviz.chart(SallyClarkBNstageA,type="barprob", scale = c(0.7,1.3), main = "No disease in the Sally Clark case")


SCprobsStageA <- round(querygrain(SallyClarkJNstageA, nodes = SCnodes, type = "joint"),4)
SCprobsStageAVector <- c(SCprobsStageA["0","0"],SCprobsStageA["1","1"],SCprobsStageA["1","0"],SCprobsStageA["0","1"])




#structured for Stage A
BN <- SallyClarkBNstageA

structuredNoSD(SallyClarkBNstageA,SCnodes,c("0","0"))
structuredNoSD(SallyClarkBNstageA,SCnodes,c("1","1"))
structuredNoSD(SallyClarkBNstageA,SCnodes,c("0","1"))
structuredNoSD(SallyClarkBNstageA,SCnodes,c("1","0"))




SCstructuredStageA <- round(c(structuredNoSD(SallyClarkBNstageA,SCnodes,c("0","0"))$structuredNoSD,
                            structuredNoSD(SallyClarkBNstageA,SCnodes,c("1","1"))$structuredNoSD,
                            structuredNoSD(SallyClarkBNstageA,SCnodes,c("0","1"))$structuredNoSD,
                            structuredNoSD(SallyClarkBNstageA,SCnodes,c("1","0"))$structuredNoSD),4)

SCFitelsonStageA <-round(c(FitelsonCoherenceForBNs(SallyClarkBNstageA,
                                             SCnodes,c("0","0"))$`Fitelson coherence`,
                     FitelsonCoherenceForBNs(SallyClarkBNstageA,
                                             SCnodes,c("1","1"))$`Fitelson coherence`,
                     FitelsonCoherenceForBNs(SallyClarkBNstageA,
                                             SCnodes,c("1","0"))$`Fitelson coherence`,
                     FitelsonCoherenceForBNs(SallyClarkBNstageA,
                                             SCnodes,c("0","1"))$`Fitelson coherence`),4)


SCDouvenMeijsStageA <- round(c(DouvenMeijsCoherenceForBNs(SallyClarkBNstageA,
                                                          SCnodes,c("0","0"))$'DM coherence',
                         DouvenMeijsCoherenceForBNs(SallyClarkBNstageA,
                                                    SCnodes,c("1","1"))$'DM coherence',
                         DouvenMeijsCoherenceForBNs(SallyClarkBNstageA,
                                                    SCnodes,c("1","0"))$'DM coherence',
                         DouvenMeijsCoherenceForBNs(SallyClarkBNstageA,
                                                    SCnodes,c("0","1"))$'DM coherence'),4)


SCOlssonStageA <- round(c(OlssonCoherenceForBNs(SallyClarkBNstageA,
                                                SCnodes,c("0","0"))$`Olsson coherence`,
                    OlssonCoherenceForBNs(SallyClarkBNstageA,SCnodes,c("1","1"))$`Olsson coherence`,
                    OlssonCoherenceForBNs(SallyClarkBNstageA,SCnodes,c("1","0"))$`Olsson coherence`,
                    OlssonCoherenceForBNs(SallyClarkBNstageA,SCnodes,c("0","1"))$`Olsson coherence`),4)



SCRocheStageA <- round(c(RocheCoherenceForBNs(SallyClarkBNstageA,
                                              SCnodes,c("0","0"))$`Roche coherence`,
                   RocheCoherenceForBNs(SallyClarkBNstageA,SCnodes,c("1","1"))$`Roche coherence`,
                   RocheCoherenceForBNs(SallyClarkBNstageA,SCnodes,c("1","0"))$`Roche coherence`,
                   RocheCoherenceForBNs(SallyClarkBNstageA,SCnodes,c("0","1"))$`Roche coherence`),4)


SCShogenjiStageA <- round(c(ShogenjiCoherenceForBNs(SallyClarkBNstageA,
                                              SCnodes,c("0","0"))$`Shogenji coherence`,
                      ShogenjiCoherenceForBNs(SallyClarkBNstageA,
                                              SCnodes,c("1","1"))$`Shogenji coherence`,
                      ShogenjiCoherenceForBNs(SallyClarkBNstageA,
                                              SCnodes,c("1","0"))$`Shogenji coherence`,
                      ShogenjiCoherenceForBNs(SallyClarkBNstageA,
                                              SCnodes,c("0","1"))$`Shogenji coherence`),4)



SCstageA <- data.frame(SCstates,SCstructuredStageA,SCFitelsonStageA,SCDouvenMeijsStageA, 
                       SCRocheStageA, SCShogenjiStageA,SCOlssonStageA,SCprobsStageAVector)
                       
                       #,SCstructuredNoSD,SCFitelson,SCDouvenMeijs,SCOlsson,SCShogenji,SCRoche,SCprobsVector)

colnames(SCstageA) <- c("States","Structured","Fitelson","Douven-Meijs","Roche","Shogenji","Olsson-Glass", "Probability")

SCstageA %>%  kable(format = "latex",booktabs=T,
                   linesep = "",  escape = FALSE, 
                   caption = "Head of the religion dataset.") %>%  
  kable_styling(latex_options=c("scale_down"))





saveRDS(SCstageA, file = "tables/SCstageA.RDS")





#now suppose disease was found in son A, call this StageB

SallyClarkJNstageB <- setEvidence(SallyClarkJN, nodes = c("Abruising","Bbruising","Adisease","Bdisease"), states = c("1","1","1","0"))

SallyClarkBNstageB <- as.bn.fit(SallyClarkJNstageB, including.evidence = TRUE)

graphviz.chart(SallyClarkBNstageB,type="barprob", scale = c(0.7,1.3), main = "Disease in son A in the Sally Clark case")

SCprobsStageB <- round(querygrain(SallyClarkJNstageB, nodes = SCnodes, type = "joint"),4)
SCprobsStageBVector <- c(SCprobsStageB["0","0"],SCprobsStageB["1","1"],SCprobsStageB["1","0"],SCprobsStageB["0","1"])


BN <- SallyClarkBNstageB

structuredNoSD(SallyClarkBNstageB,SCnodes,c("0","0"))
structuredNoSD(SallyClarkBNstageB,SCnodes,c("1","1"))
structuredNoSD(SallyClarkBNstageB,SCnodes,c("1","0"))
structuredNoSD(SallyClarkBNstageB,SCnodes,c("0","1"))


SCstructuredStageB <- round(c(structuredNoSD(SallyClarkBNstageB,SCnodes,c("0","0"))$structuredNoSD,
                              structuredNoSD(SallyClarkBNstageB,SCnodes,c("1","1"))$structuredNoSD,
                              structuredNoSD(SallyClarkBNstageB,SCnodes,c("0","1"))$structuredNoSD,
                              structuredNoSD(SallyClarkBNstageB,SCnodes,c("1","0"))$structuredNoSD),4)

SCFitelsonStageB <-round(c(FitelsonCoherenceForBNs(SallyClarkBNstageB,
                                                   SCnodes,c("0","0"))$`Fitelson coherence`,
                           FitelsonCoherenceForBNs(SallyClarkBNstageB,
                                                   SCnodes,c("1","1"))$`Fitelson coherence`,
                           FitelsonCoherenceForBNs(SallyClarkBNstageB,
                                                   SCnodes,c("1","0"))$`Fitelson coherence`,
                           FitelsonCoherenceForBNs(SallyClarkBNstageB,
                                                   SCnodes,c("0","1"))$`Fitelson coherence`),4)


SCDouvenMeijsStageB <- round(c(DouvenMeijsCoherenceForBNs(SallyClarkBNstageB,
                                                          SCnodes,c("0","0"))$'DM coherence',
                               DouvenMeijsCoherenceForBNs(SallyClarkBNstageB,
                                                          SCnodes,c("1","1"))$'DM coherence',
                               DouvenMeijsCoherenceForBNs(SallyClarkBNstageB,
                                                          SCnodes,c("1","0"))$'DM coherence',
                               DouvenMeijsCoherenceForBNs(SallyClarkBNstageB,
                                                          SCnodes,c("0","1"))$'DM coherence'),4)


SCOlssonStageB <- round(c(OlssonCoherenceForBNs(SallyClarkBNstageB,
                                                SCnodes,c("0","0"))$`Olsson coherence`,
                          OlssonCoherenceForBNs(SallyClarkBNstageB,SCnodes,c("1","1"))$`Olsson coherence`,
                          OlssonCoherenceForBNs(SallyClarkBNstageB,SCnodes,c("1","0"))$`Olsson coherence`,
                          OlssonCoherenceForBNs(SallyClarkBNstageB,SCnodes,c("0","1"))$`Olsson coherence`),4)



SCRocheStageB <- round(c(RocheCoherenceForBNs(SallyClarkBNstageB,
                                              SCnodes,c("0","0"))$`Roche coherence`,
                         RocheCoherenceForBNs(SallyClarkBNstageB,SCnodes,c("1","1"))$`Roche coherence`,
                         RocheCoherenceForBNs(SallyClarkBNstageB,SCnodes,c("1","0"))$`Roche coherence`,
                         RocheCoherenceForBNs(SallyClarkBNstageB,SCnodes,c("0","1"))$`Roche coherence`),4)


SCShogenjiStageB <- round(c(ShogenjiCoherenceForBNs(SallyClarkBNstageB,
                                                    SCnodes,c("0","0"))$`Shogenji coherence`,
                            ShogenjiCoherenceForBNs(SallyClarkBNstageB,
                                                    SCnodes,c("1","1"))$`Shogenji coherence`,
                            ShogenjiCoherenceForBNs(SallyClarkBNstageB,
                                                    SCnodes,c("1","0"))$`Shogenji coherence`,
                            ShogenjiCoherenceForBNs(SallyClarkBNstageB,
                                                    SCnodes,c("0","1"))$`Shogenji coherence`),4)



SCstageB <- data.frame(SCstates,SCstructuredStageB,SCFitelsonStageB,SCDouvenMeijsStageB, 
                       SCRocheStageB, SCShogenjiStageB,SCOlssonStageB,SCprobsStageBVector)

#,SCstructuredNoSD,SCFitelson,SCDouvenMeijs,SCOlsson,SCShogenji,SCRoche,SCprobsVector)
colnames(SCstageB) <- c("States","Structured","Fitelson","Douven-Meijs","Roche","Shogenji","Olsson-Glass", "Probability")

SCstageB %>%  kable(format = "latex",booktabs=T,
                    linesep = "",  escape = FALSE, 
                    caption = "Head of the religion dataset.") %>%  
  kable_styling(latex_options=c("scale_down"))





saveRDS(SCstageB, file = "tables/SCstageB.RDS")




#now look at correlations with probability

SCall <- rbind(SCnoEvidence,SCstageA,SCstageB)


SCall
colnames(SCall)

#high coherence with various pr, low coherence, low pr
strPlot <- ggplot(SCall)+geom_jitter(aes(y = Probability, x = Structured), size = 1, alpha = .5,
                                     width = 0.05, height = 0.05)+
  theme_tufte()+xlab("Coherence")+labs(color = "Coherence measure")+ggtitle("Structured")

strPlot

fitPlot <- ggplot(SCall) +
  geom_jitter(aes(y = Probability, x = Fitelson), size = 1, alpha = .5,
              width = 0.05, height = 0.05) +
  theme_tufte()+xlab("Coherence")+ggtitle("Fitelson")

fitPlot

ogPlot <- ggplot(SCall) +
  geom_jitter(aes(y = Probability, x = SCall$`Olsson-Glass`), size = 1, alpha = .5,
              width = 0.05, height = 0.05)  +
  theme_tufte()+xlab("Coherence")+labs(color = "Coherence measure")+ggtitle("Olsson-Glass")

ogPlot

rochPlot <-ggplot(SCall)+ geom_jitter(aes(y = Probability, x = Roche), size = 1, alpha = .5, 
                                      width = 0.05, height = 0.05)+
  theme_tufte()+xlab("Coherence")+labs(color = "Coherence measure")+ggtitle("Roche")

rochPlot


ogPlot

#wow, coherence high, prob tends to be low!
dmPlot <- ggplot(SCall) +
  geom_jitter(aes(y = Probability, x = SCall$`Douven-Meijs`), size = 1, alpha = .5, width = .15, height = 0.05) +
  theme_tufte()+xlab("Coherence")+labs(color = "Coherence measure")+ggtitle("Douven-Meijs")


dmPlot

shPlot <- ggplot(SCall)+geom_jitter(aes(y = Probability, x = Shogenji), size = 1, alpha = .7, width = 1, height = 0.05)+
  theme_tufte()+xlab("Coherence")+labs(color = "Coherence measure")+ggtitle("Shogenji")



shPlot

png(file="../images/cohPlots.png", 
    units="in", 
    width=6, 
    height=6, 
    pointsize = 1,
    res=400)
grid.arrange(strPlot, fitPlot, ogPlot, rochPlot, dmPlot, shPlot, nrow = 3)

dev.off()









nrow(SCall)




