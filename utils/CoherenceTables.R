
#first for a single scenario, then generalize to a list of scenarios
#source("..//measures//Fitelson.R")

CoherencesRow <- function (BN, narrationNodes, states,
                statusLR = rep("Ind",FitelsonSize(length(narrationNodes))),
                statusRL = rep("Ind",FitelsonSize(length(narrationNodes))),
                exampleName){
  FI <- FitelsonCoherenceForBNs(BN, narrationNodes,
                              states = states, statusLR,statusRL)[[3]]
  DM <- DouvenMeijsCoherenceForBNs(BN,narrationNodes,states)[[3]]
  OL <- OlssonCoherenceForBNs(BN,narrationNodes,states)[[3]]
  RA <- RAcoherenceForBNs(BN,narrationNodes,states)[[3]]
  RO <- RocheCoherenceForBNs(BN,narrationNodes,states)[[3]]
  SH <- ShogenjiCoherenceForBNs(BN,narrationNodes,states)[[3]]
  StructuredCon <- structuredCoherence(BN,narrationNodes,states)$`structured Coherence Con`
  StructuredAnte <- structuredCoherence(BN,narrationNodes,states)$`structured Coherence Ante`
  
  
  row <- data.frame( "DouvenMeijs" = DM,
                     Fitelson = FI,
             Olsson = OL,
             RA = RA,
             Roche = RO,
             Shogenji = SH,
             StructuredCon = StructuredCon,
             StructuredAnte = StructuredAnte)

  rownames(row) <- paste(paste(exampleName, ":", sep = ""), paste(narrationNodes, sep = "", collapse=""), paste(states, sep = "", collapse=""), sep = " ")

return(row)
}
  


#___________test on penguins


#BirdStatusLR <- c("Ind","Ent","Ent","Ent","Ent","Ent")
#BirdStatusRL <- c("Ind","Ind","Ind","Ind","Ind","Ind")

#R1 <- CoherencesRow(BN = BirdBN, narrationNodes = c("B","G","P"),
#             states = c("1", "1", "1"), statusLR = BirdStatusLR,
#            statusRL = BirdStatusRL, exampleName = "Penguins")
#                states = c("1", "1", "1"), exampleName = "Penguins")


#R1Ind
#note R1Ind is different from R1!!


#R2 <- CoherencesRow(BN = BirdBN, narrationNodes = c("B","G"),
 #             states = c("1", "1"),exampleName = "Penguins" )


#BirdStatusLR <- c("Ent")
#BirdStatusRL <- c("Ind")

#R3Ind <- CoherencesRow(BN = BirdBN, narrationNodes = c("B","P"),
#              states = c("1", "1"), exampleName = "Penguins")

#R3 <- CoherencesRow(BN = BirdBN, narrationNodes = c("B", "P"),
#                    states = c("1", "1"), statusLR = BirdStatusLR,
#                    statusRL = BirdStatusRL, exampleName = "Penguins")


#rbind(R1,R2,R3)



#watch out! this works with INDEPENDENT nodes!! )
CoherencesTable <- function(BN,scenariosList,
             statesList,exampleName){
rows <- list()
for(s in 1:length(scenariosList)){
rows[[s]] <-  CoherencesRow(BN,scenariosList[[s]],statesList[[s]], 
                                exampleName = exampleName)
  }
table <- do.call("rbind", rows)
return(table)
}



#gsub("[A-Z]", "1", scenariosList[[s]])
# rows[[s]] <-  ifelse((statesList == gsub("[A-Z]", "1", scenariosList)),
#                      CoherencesRow(BN,scenariosList[[s]], eval(parse(text = statesList[[s]])),  exampleName = exampleName),

# onesFromScenarioList <- function(scenarioList){
# str_split(string = gsub("[A-Z]", "1", scenariosList), pattern = ",")
# }
# 
# 
# scenariosList <-  list(c("B","G", "P"),c("B","G"),c("B","P"))
# statesList <- onesFromScenarioList(scenariosList)
# 
# as.vector(str_split(statesList[[1]], pattern = ",")))

#example of use
# CoherencesTable(BirdBN, 
#                 scenariosList =  list(c("B","G", "P"),c("B","G"),c("B","P")),
#                 statesList = list(c("1","1", "1"),c("1","1"),c("1","1")),
#                 exampleName = "Penguins")
# 
# 
# CoherencesTable(BirdBN, 
#                 scenariosList =  list(c("B","G", "P"),c("B","G"),c("B","P")),
#                 statesList = list(c("1","1","0"),c("1","0"), c("0","1")),
#                 exampleName = "Penguins")
# 










