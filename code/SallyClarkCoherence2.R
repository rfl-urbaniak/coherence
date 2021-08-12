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

#stage 0 
scStage0DAG <- model2network("[Amurder][Bmurder|Amurder]")

graphviz.plot(scStage0DAG)

png(file="../images/scStage0DAG.png", 
    units="in", 
    width=6, 
    height=6, 
    pointsize = 1,
    res=400)
graphviz.plot(scStage0DAG)
dev.off()


#stages 1 and 2


scDAG <- model2network("[Abruising|Amurder][Adisease|Amurder][Bbruising|Bmurder][Bdisease|Bmurder][Amurder][Bmurder|Amurder]")


#plot 
png(file="../images/scFullDAG.png", 
    units="in", 
    width=6, 
    height=6, 
    pointsize = 1,
    res=400)
graphviz.plot(scDAG)
dev.off()

graphviz.plot(scDAG)


#Now define conditional probability tables

#CPTs as used in Fenton & al.
AmurderProb <-prior.CPT("Amurder","0","1",0.921659)
AbruisingProb <- single.CPT("Abruising","Amurder","1","0","0","1",0.01,0.05)
AdiseaseProb <- single.CPT("Adisease","Amurder","1","0","0","1",0.05,0.001)
BbruisingProb <- single.CPT("Bbruising","Bmurder","1","0","0","1",0.01,0.05)
BdiseaseProb <- single.CPT("Bdisease","Bmurder","1","0","0","1",0.05,0.001)
BmurderProb <- single.CPT("Bmurder","Amurder","0","1","0","1",0.9993604,1-0.9998538)

AmurderProb

BmurderProb
AbruisingProb


# Put CPTs together


scStage0CPT <-  list(Amurder=AmurderProb,
                     Bmurder = BmurderProb)


scCPT <- list(Amurder=AmurderProb,Adisease = AdiseaseProb,
                      Bmurder = BmurderProb,Bdisease=BdiseaseProb,
                      Abruising = AbruisingProb,Bbruising = BbruisingProb)
              
# join with the DAGs to get  BNs

scStage0BN <- custom.fit(scStage0DAG, scStage0CPT)

scBN <- custom.fit(scDAG,scCPT)


png(file="../images/scBN.png", 
    units="in", 
    width=6, 
    height=6, 
    pointsize = 1,
    res=400)
graphviz.chart(scBN,type="barprob", scale = c(0.7,1.3))
dev.off()

png(file="../images/scStage0BN.png", 
    units="in", 
    width=6, 
    height=6, 
    pointsize = 1,
    res=400)
graphviz.chart(scStage0BN,type="barprob", scale = c(0.7,1.3))
dev.off()



#Now coherence calculations

scStage0nodes <- c("Amurder","Bmurder")

scNodes <- c("Amurder","Bmurder", "Abruising", "Bbruising","Adisease","Bdisease")


#First, stage 0


BN <- scStage0BN

#structuredNoSD(scStage0BN,scStage0nodes,c("0","0"))
#structuredNoSD(scStage0BN,scStage0nodes,c("1","1"))
#structuredNoSD(scStage0BN,scStage0nodes,c("0","1"))
#structuredNoSD(scStage0BN,scStage0nodes,c("1","0"))

sc0structured <- round(c(structuredNoSD(scStage0BN,scStage0nodes,c("0","0"))$structuredNoSD,
                         structuredNoSD(scStage0BN,scStage0nodes,c("1","1"))$structuredNoSD,
                         structuredNoSD(scStage0BN,scStage0nodes,c("0","1"))$structuredNoSD,
                         structuredNoSD(scStage0BN,scStage0nodes,c("1","0"))$structuredNoSD),4)

sc0structured
  
  
#--------------------

sc0fitelson <- round(c(FitelsonCoherenceForBNs(scStage0BN,scStage0nodes,c("0","0"))$`Fitelson coherence`,
                  FitelsonCoherenceForBNs(scStage0BN,scStage0nodes,c("1","1"))$`Fitelson coherence`,
                  FitelsonCoherenceForBNs(scStage0BN,scStage0nodes,c("0","1"))$`Fitelson coherence`,
                  FitelsonCoherenceForBNs(scStage0BN,scStage0nodes,c("1","0"))$`Fitelson coherence`),4)



sc0fitelson


sc0DouvenMeijs <- round(c(DouvenMeijsCoherenceForBNs(scStage0BN,
                                      scStage0nodes,c("0","0"))$'DM coherence',                       DouvenMeijsCoherenceForBNs(scStage0BN,scStage0nodes,c("1","1"))$'DM coherence',
  DouvenMeijsCoherenceForBNs(scStage0BN,scStage0nodes,c("1","0"))$'DM coherence',
  DouvenMeijsCoherenceForBNs(scStage0BN,scStage0nodes,c("0","1"))$'DM coherence'),4)

sc0DouvenMeijs


sc0Olsson <- round(c(OlssonCoherenceForBNs(scStage0BN,scStage0nodes,c("0","0"))$`Olsson coherence`,
                    OlssonCoherenceForBNs(scStage0BN,scStage0nodes,c("1","1"))$`Olsson coherence`,
                    OlssonCoherenceForBNs(scStage0BN,scStage0nodes,c("1","0"))$`Olsson coherence`,
                    OlssonCoherenceForBNs(scStage0BN,scStage0nodes,c("0","1"))$`Olsson coherence`),5)


sc0Olsson


sc0Shogenji <- round(c(ShogenjiCoherenceForBNs(scStage0BN,
            scStage0nodes,c("0","0"))$`Shogenji coherence`,   
            ShogenjiCoherenceForBNs(scStage0BN,scStage0nodes,c("1","1"))$`Shogenji coherence`,
            ShogenjiCoherenceForBNs(scStage0BN,scStage0nodes,c("1","0"))$`Shogenji coherence`,
            ShogenjiCoherenceForBNs(scStage0BN,scStage0nodes,c("0","1"))$`Shogenji coherence`),4)


sc0Shogenji



sc0Roche <- round(c(RocheCoherenceForBNs(scStage0BN,scStage0nodes,c("0","0"))$`Roche coherence`,
                   RocheCoherenceForBNs(scStage0BN,scStage0nodes,c("1","1"))$`Roche coherence`,
                   RocheCoherenceForBNs(scStage0BN,scStage0nodes,c("1","0"))$`Roche coherence`,
                   RocheCoherenceForBNs(scStage0BN,scStage0nodes,c("0","1"))$`Roche coherence`),4)


sc0Roche


#add probabilities, build table
scStage0JN <- compile(as.grain(scStage0BN))
sc0probs <- round(querygrain(scStage0JN, nodes = scStage0nodes, type = "joint"),4)

sc0probsVector <- c(sc0probs["0","0"],sc0probs["1","1"],sc0probs["1","0"],sc0probs["0","1"])



sc0probsVector


SCstates <- c("00", "11","01","10")

scStage0Table <- data.frame(rep("Stage 0",4),SCstates,sc0structured,sc0fitelson,sc0DouvenMeijs,sc0Roche,sc0Shogenji,sc0Olsson,sc0probsVector, rep(NA,4))

colnames(scStage0Table) <- c("Stage", "States","Structured","Fitelson","Douven-Meijs","Roche","Shogenji","Olsson-Glass", "Priors", "Posteriors")

scStage0Table <- readRDS("tables/scStage0Table.RDS")

saveRDS(scStage0Table, file = "tables/scStage0Table.RDS")




#Stage 1: both bruising, no disease



BN <- scBN 

scJN <- compile(as.grain(scBN)) 

scJNEvi  <- setEvidence(scJN, nodes = c("Abruising","Bbruising", "Adisease", "Bdisease"),
                        states = c("1","1", "0", "0"))

scBNEvi <- as.bn.fit(scJNEvi, including.evidence = TRUE)


scBNEvi


structuredNoSD(scBNEvi, SCnodes[1:2],c("1","1"))

structuredNoSD(scBN, SCnodes[1:2],c("1","1"))

structuredNoSD(scBN, SCnodes[1:2],c("1","1"))



SCnodes <- c("Amurder","Bmurder", "Abruising", "Bbruising", "Adisease","Bdisease")


structuredNoSD(scBN, SCnodes,c("1","1","1","1","0","0"))

sc1structured <- round(c(structuredNoSD(scBN, SCnodes,c("0","0","1","1","0","0"))$structuredNoSD,
                         structuredNoSD(scBN, SCnodes,c("1","1","1","1","0","0"))$structuredNoSD,
                         structuredNoSD(scBN, SCnodes,c("0","1","1","1","0","0"))$structuredNoSD,
                         structuredNoSD(scBN, SCnodes,c("1","0","1","1","0","0"))$structuredNoSD),4)

sc1structured


#calculation of structured for a single scenario is 0.036 seconds
#calculation of fitelson for a single scenario is 999 seconds, approx 17 minutes


sc1fitelson <- round(c(FitelsonCoherenceForBNs(scBN, 
        SCnodes,c("0","0","1","1","0","0"))$`Fitelson coherence`,
        FitelsonCoherenceForBNs(scBN, SCnodes,c("1","1","1","1","0","0"))$`Fitelson coherence`,
        FitelsonCoherenceForBNs(scBN, SCnodes,c("0","1","1","1","0","0"))$`Fitelson coherence`,
        FitelsonCoherenceForBNs(scBN, SCnodes,c("1","0","1","1","0","0"))$`Fitelson coherence`),4)


sc1fitelson

saveRDS(sc1fitelson, file = "tables/sc1fitelson.RDS")


sc1DM <- round(c(DouvenMeijsCoherenceForBNs(scBN, 
              SCnodes,c("0","0","1","1","0","0"))$'DM coherence',
          DouvenMeijsCoherenceForBNs(scBN, SCnodes,c("1","1","1","1","0","0"))$'DM coherence',
          DouvenMeijsCoherenceForBNs(scBN, SCnodes,c("0","1","1","1","0","0"))$'DM coherence',
          DouvenMeijsCoherenceForBNs(scBN, SCnodes,c("1","0","1","1","0","0"))$'DM coherence'),4)

sc1DM

saveRDS(sc1DM, file = "tables/sc1DM.RDS")



sc1Olsson <- round(c(OlssonCoherenceForBNs(scBN, 
          SCnodes,c("0","0","1","1","0","0"))$`Olsson coherence`,
          OlssonCoherenceForBNs(scBN, SCnodes,c("1","1","1","1","0","0"))$`Olsson coherence`,
          OlssonCoherenceForBNs(scBN, SCnodes,c("0","1","1","1","0","0"))$`Olsson coherence`,
          OlssonCoherenceForBNs(scBN, SCnodes,c("1","0","1","1","0","0"))$`Olsson coherence`),7)

sc1Olsson

saveRDS(sc1Olsson, file = "tables/sc1Olsson.RDS")



sc1shogenji<- round(c(ShogenjiCoherenceForBNs(scBN, 
              SCnodes,c("0","0","1","1","0","0"))$`Shogenji coherence`,
        ShogenjiCoherenceForBNs(scBN, SCnodes,c("1","1","1","1","0","0"))$`Shogenji coherence`,
        ShogenjiCoherenceForBNs(scBN, SCnodes,c("0","1","1","1","0","0"))$`Shogenji coherence`,
        ShogenjiCoherenceForBNs(scBN, SCnodes,c("1","0","1","1","0","0"))$`Shogenji coherence`),4)



sc1shogenji

saveRDS(sc1shogenji, file = "tables/sc1shogenji.RDS")




sc1roche <- round(c(RocheCoherenceForBNs(scBN, 
                      SCnodes,c("0","0","1","1","0","0"))$`Roche coherence`,
          RocheCoherenceForBNs(scBN, SCnodes,c("1","1","1","1","0","0"))$`Roche coherence`,
          RocheCoherenceForBNs(scBN, SCnodes,c("0","1","1","1","0","0"))$`Roche coherence`,
          RocheCoherenceForBNs(scBN, SCnodes,c("1","0","1","1","0","0"))$`Roche coherence`),4)

sc1roche


saveRDS(sc1roche, file = "tables/sc1roche.RDS")




#Now Stage 2: disease in the older son


sc2structured <- round(c(structuredNoSD(scBN, SCnodes,c("0","0","1","1","1","0"))$structuredNoSD,
                         structuredNoSD(scBN, SCnodes,c("1","1","1","1","1","0"))$structuredNoSD,
                         structuredNoSD(scBN, SCnodes,c("0","1","1","1","1","0"))$structuredNoSD,
                         structuredNoSD(scBN, SCnodes,c("1","0","1","1","1","0"))$structuredNoSD),4)

sc2structured

saveRDS(sc2structured, file = "tables/sc2structured.RDS")


sc2fitelson <- round(c(FitelsonCoherenceForBNs(scBN, 
        SCnodes,c("0","0","1","1","0","0"))$`Fitelson coherence`,
        FitelsonCoherenceForBNs(scBN, SCnodes,c("1","1","1","1","1","0"))$`Fitelson coherence`,
        FitelsonCoherenceForBNs(scBN, SCnodes,c("0","1","1","1","1","0"))$`Fitelson coherence`,
        FitelsonCoherenceForBNs(scBN, SCnodes,c("1","0","1","1","1","0"))$`Fitelson coherence`),4)


sc2fitelson

saveRDS(sc2fitelson, file = "tables/sc2fitelson.RDS")


sc2DM <- round(c(DouvenMeijsCoherenceForBNs(scBN, 
          SCnodes,c("0","0","1","1","0","0"))$'DM coherence',
          DouvenMeijsCoherenceForBNs(scBN, SCnodes,c("1","1","1","1","1","0"))$'DM coherence',
          DouvenMeijsCoherenceForBNs(scBN, SCnodes,c("0","1","1","1","1","0"))$'DM coherence',
          DouvenMeijsCoherenceForBNs(scBN, SCnodes,c("1","0","1","1","1","0"))$'DM coherence'),4)

sc2DM

saveRDS(sc2DMn, file = "tables/sc2sc2DM.RDS")



sc2Olsson <- round(c(OlssonCoherenceForBNs(scBN, 
              SCnodes,c("0","0","1","1","1","0"))$`Olsson coherence`,
              OlssonCoherenceForBNs(scBN, SCnodes,c("1","1","1","1","1","0"))$`Olsson coherence`,
              OlssonCoherenceForBNs(scBN, SCnodes,c("0","1","1","1","1","0"))$`Olsson coherence`,
              OlssonCoherenceForBNs(scBN, SCnodes,c("1","0","1","1","1","0"))$`Olsson coherence`),9)

sc2Olsson

saveRDS(sc2Olsson, file = "tables/sc2Olsson.RDS")


sc2shogenji<- round(c(ShogenjiCoherenceForBNs(scBN, 
          SCnodes,c("0","0","1","1","1","0"))$`Shogenji coherence`,
          ShogenjiCoherenceForBNs(scBN, SCnodes,c("1","1","1","1","1","0"))$`Shogenji coherence`,
          ShogenjiCoherenceForBNs(scBN, SCnodes,c("0","1","1","1","1","0"))$`Shogenji coherence`,
          ShogenjiCoherenceForBNs(scBN, SCnodes,c("1","0","1","1","1","0"))$`Shogenji coherence`),4)

sc2shogenji


saveRDS(sc2shogenji, file = "tables/sc2shogenji.RDS")


sc2roche <- round(c(RocheCoherenceForBNs(scBN, SCnodes,c("0","0","1","1","1","0"))$`Roche coherence`,
                    RocheCoherenceForBNs(scBN, SCnodes,c("1","1","1","1","1","0"))$`Roche coherence`,
                    RocheCoherenceForBNs(scBN, SCnodes,c("0","1","1","1","1","0"))$`Roche coherence`,
                    RocheCoherenceForBNs(scBN, SCnodes,c("1","0","1","1","1","0"))$`Roche coherence`),4)

sc2roche

saveRDS(sc2roche, file = "tables/sc2roche.RDS")





#building tables
scJN <- compile(as.grain(scBN))

?querygrain

probsS1 <- querygrain(scJN, nodes = c("Amurder","Bmurder","Abruising","Bbruising","Adisease","Bdisease"),type = "joint")

probsS1

str(probsS1)


..$Abruising: chr [1:2] "1" "0"
..$ Amurder  : chr [1:2] "0" "1"
..$ Adisease : chr [1:2] "1" "0"
..$ Bmurder  : chr [1:2] "0" "1"
..$ Bbruising: chr [1:2] "1" "0"
..$ Bdisease : chr [1:2] "1" "0"


scenario1 <- probsS1[1,1,2,1,1,2]    #scenario 1 c("0","0","1","1","1","0")
scenario2 <- probsS1[1,2,2,2,1,2]    #scenario 2 c("1","1","1","1","1","0")
scenario3 <- probsS1[1,1,2,2,1,2]    #scenario 3 c("0","1","1","1","1","0")
scenario4 <- probsS1[1,2,2,1,1,2]   #scenario 4 c("1","0","1","1","1","0")


priorsStage1 <- c(scenario1,scenario2,scenario3,scenario4)



str(probsS1)

..$ Abruising: chr [1:2] "1" "0"
..$ Amurder  : chr [1:2] "0" "1"
..$ Adisease : chr [1:2] "1" "0"
..$ Bmurder  : chr [1:2] "0" "1"
..$ Bbruising: chr [1:2] "1" "0"
..$ Bdisease : chr [1:2] "1" "0"


scenario1s2 <- probsS1[1,1,1,1,1,2]    #scenario 1 c("0","0","1","1","1","0")
scenario2s2 <- probsS1[1,2,1,2,1,2]    #scenario 2 c("1","1","1","1","1","0")
scenario3s2 <- probsS1[1,1,1,2,1,2]    #scenario 3 c("0","1","1","1","1","0")
scenario4s2 <- probsS1[1,2,1,1,1,2]   #scenario 4 c("1","0","1","1","1","0")

priorsStage2 <- c(scenario1s2,scenario2s2,scenario3s2,scenario4s2)


#or, update BN first with evidence and get posteriors
scJNstage1 <- setEvidence(scJN, nodes = c("Abruising","Bbruising","Adisease","Bdisease"), states = c("1","1","0","0"))

scBNstage1 <- as.bn.fit(scJNstage1, including.evidence = TRUE)

graphviz.chart(scBNstage1,type="barprob", scale = c(0.7,1.3), main = "No disease in the Sally Clark case")


scProbsStage1 <- round(querygrain(scJNstage1, nodes = scStage0nodes, type = "joint"),5)
scProbsStage1
scPosteriorsS1 <- c(scProbsStage1["0","0"],scProbsStage1["1","1"],scProbsStage1["0","1"],scProbsStage1["1","0"])

scPosteriorsS1




scJNstage2 <- setEvidence(scJN, nodes = c("Abruising","Bbruising","Adisease","Bdisease"), states = c("1","1","1","0"))

scBNstage2 <- as.bn.fit(scJNstage2, including.evidence = TRUE)

graphviz.chart(scBNstage2,type="barprob", scale = c(0.7,1.3), main = "No disease in the Sally Clark case")


scProbsStage2 <- round(querygrain(scJNstage2, nodes = scStage0nodes, type = "joint"),6)
scProbsStage2
scPosteriorsS2 <- c(scProbsStage2["0","0"],scProbsStage2["1","1"],scProbsStage2["0","1"],scProbsStage2["1","0"])

scPosteriorsS2






scStage1Table <- data.frame(rep("Stage 1",4),SCstates,sc1structured,sc1fitelson,sc1DM,sc1roche,sc1shogenji,sc1Olsson,priorsStage1, scPosteriorsS1)

colnames(scStage1Table) <- c("Stage","States","Structured","Fitelson","Douven-Meijs","Roche","Shogenji","Olsson-Glass", "Priors", "Posteriors")

scStage1Table


scStage2Table <- data.frame(rep("Stage 2",4),SCstates,sc2structured,sc2fitelson,sc2DM,sc2roche,sc2shogenji,sc2Olsson,priorsStage2, scPosteriorsS2)

colnames(scStage2Table) <- c("Stage","States","Structured","Fitelson","Douven-Meijs","Roche","Shogenji","Olsson-Glass", "Priors", "Posteriors")

scStage2Table

scJointTable <- rbind(scStage0Table, scStage1Table, scStage2Table)

saveRDS(scJointTable, file = "tables/scJointTable.RDS")

scJointTable <- readRDS("tables/scJointTable.RDS")

pairs(scJointTable[,c(-1,-2)])


library(dplyr)
scJointTableForPrinting <- scJointNew %>% dplyr::mutate_if(is.numeric,
                                                             funs(as.character(signif(., 4)))) 

scJointNew


scJointTableForPrinting


scJointTableForPrinting %>%  kable(format = "latex",booktabs=T,
           linesep = "",  escape = FALSE, 
           caption = "Coherence scores in the Sally Clark scenarios in three stages, with priors and posteriors given evidence.") %>%  
  kable_styling(latex_options=c("scale_down"))






#OUTDATED


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



