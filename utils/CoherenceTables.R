
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
  OLg <- OlssonGeneralizedCoherence(BN, narrationNodes, states)[[1]]
  SH <- ShogenjiCoherenceForBNs(BN,narrationNodes,states)[[3]]
  SHg <- ShogenjiGeneralizedCoherence(BN, narrationNodes, states)[[1]]
  RO <- RocheCoherenceForBNs(BN,narrationNodes,states)[[3]]
  RA <- RAcoherenceForBNs(BN,narrationNodes,states)[[3]]
  
  StructuredCon <- structuredCoherence(BN,narrationNodes,states)$`structured Coherence Con`
  StructuredAnte <- structuredCoherence(BN,narrationNodes,states)$`structured Coherence Ante`
  StructuredConS <- structuredCoherence(BN,narrationNodes,states)$`structured Coherence Con scaled`
  StructuredAnteS <- structuredCoherence(BN,narrationNodes,states)$`structured Coherence Ante scaled`
    
  row <- data.frame( 
             Ol = OL,
             OlG = OLg,
             Sh = SH,
             ShG = SHg,
             Fit = FI,
             "DouvenMeijs" = DM,
             Roche = RO,
             RA = RA,
             StrCon = StructuredCon,
             StrAnte = StructuredAnte,
             StrConS = StructuredConS,
             StrAnteS = StructuredAnteS)

rownames(row) <- paste(paste(exampleName, ":", sep = ""), paste(narrationNodes, sep = "", collapse=""), paste(states, sep = "", collapse=""), sep = " ")

return(row)
}
  


#___________test on penguins

#test <- CoherencesRow(BN = BirdBN, narrationNodes = c("B","G","P"), states = c("1", "1", "1"), exampleName = "Penguins")




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













