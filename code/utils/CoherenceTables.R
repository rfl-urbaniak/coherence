

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
  S <-   structuredL(BN,narrationNodes,states)$structuredZ
  LR <-   structuredL(BN,narrationNodes,states)$structuredLR
  L <-   structuredL(BN,narrationNodes,states)$structuredL
  #RA <- RAcoherenceForBNs(BN,narrationNodes,states)[[3]]
  #result <- structuredNoSD(BN,narrationNodes,states)
  #Structured <- result$`structured Coherence Ante scaled`
  #StructuredSquared <- result$`structuredSquared` 
  #StructuredNoSD <- result$`structuredNoSD`
  
  
  row <- data.frame( 
    OG = OL,
    OGGen = OLg,
    Sh = SH,
    ShGen = SHg,
    DM = DM,
    R = RO,
    Fi = FI,
    SZ= S,
    SLR = LR,
    SL  = L
    )
  
  rownames(row) <- paste(paste(exampleName, ":", sep = ""), paste(narrationNodes, sep = "", collapse=""), paste(states, sep = "", collapse=""), sep = " ")
  
  return(row)
}



#watch out! this works with INDEPENDENT nodes!! )
CoherencesTable <- function(BNlist,scenariosList,
                            statesList,exampleName){
  rows <- list()
  for(s in 1:length(scenariosList)){
    rows[[s]] <-  CoherencesRow(BNlist[[s]],scenariosList[[s]],statesList[[s]], 
                                exampleName = exampleName)
  }
  table <- do.call("rbind", rows)
  return(table)
}



#statusLR = rep("Ind",FitelsonSize(length(narrationNodes)))
#statusRL = rep("Ind",FitelsonSize(length(narrationNodes)))


CoherencesRowNarr <- function (BN, narrationNodes, states,
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
  Znarr <- structuredNarr(BN, narrationNodes,states)$structuredZnarr
  LRnarr <- structuredNarr(BN, narrationNodes,states)$structuredLRnarr
  Lnarr <- structuredNarr(BN, narrationNodes,states)$structuredLnarr
  
  
  row <- data.frame( 
    OG = OL,
    OGGen = OLg,
    Sh = SH,
    ShGen = SHg,
    DM = DM,
    R = RO,
    Fi = FI,
    SZ= Znarr,
    SLR = LRnarr,
    SL  = Lnarr
  )
#  exampleName <- "W1W2"
  
  rownames(row) <- paste(paste(exampleName, ":", sep = ""), paste(narrationNodes, sep = "", collapse=""), paste(states, sep = "", collapse=""), sep = " ")

  return(row)
}



# 
# 
# CoherencesRowNarr <- function (BN, narrationNodes, states,
#                                statusLR = rep("Ind",FitelsonSize(length(narrationNodes))),
#                                statusRL = rep("Ind",FitelsonSize(length(narrationNodes))),
#                                exampleName){
#   FI <- FitelsonCoherenceForBNs(BN, narrationNodes,
#                                 states = states, statusLR,statusRL)[[3]]
#   DM <- DouvenMeijsCoherenceForBNs(BN,narrationNodes,states)[[3]]
#   OL <- OlssonCoherenceForBNs(BN,narrationNodes,states)[[3]]
#   OLg <- OlssonGeneralizedCoherence(BN, narrationNodes, states)[[1]]
#   SH <- ShogenjiCoherenceForBNs(BN,narrationNodes,states)[[3]]
#   SHg <- ShogenjiGeneralizedCoherence(BN, narrationNodes, states)[[1]]
#   RO <- RocheCoherenceForBNs(BN,narrationNodes,states)[[3]]
#   Znarr <- structuredNarr(BN, narrationNodes,states)$structuredZnarr
#   LRnarr <- structuredNarr(BN, narrationNodes,states)$structuredLRnarr
#   Lnarr <- structuredNarr(BN, narrationNodes,states)$structuredLnarr
#   
#   
#   row <- data.frame( 
#     OG = OL,
#     OGGen = OLg,
#     Sh = SH,
#     ShGen = SHg,
#     DM = DM,
#     R = RO,
#     Fi = FI,
#     SZnarr= Znarr,
#     SLRnarr = LRnarr,
#     SLnarr  = Lnarr
#   )
#   #  exampleName <- "W1W2"
#   
#   rownames(row) <- paste(paste(exampleName, ":", sep = ""), paste(narrationNodes, sep = "", collapse=""), paste(states, sep = "", collapse=""), sep = " ")
#   
#   return(row)
# }



#watch out! this works with INDEPENDENT nodes!! )
CoherencesTableNarr <- function(BNlist,scenariosList,
                            statesList,exampleName){
  rows <- list()
  for(s in 1:length(scenariosList)){
    rows[[s]] <-  CoherencesRowNarr(BNlist[[s]],scenariosList[[s]],statesList[[s]], 
                                exampleName = exampleName)
  }
  table <- do.call("rbind", rows)
  return(table)
}

# 
# 
# #watch out! this works with INDEPENDENT nodes!! )
# CoherencesTableNarr2 <- function(BNlist,scenariosList,
#                                 statesList,exampleName){
#   rows <- list()
#   for(s in 1:length(scenariosList)){
#     rows[[s]] <-  CoherencesRowNarr2(BNlist[[s]],scenariosList[[s]],statesList[[s]], 
#                                     exampleName = exampleName)
#   }
#   table <- do.call("rbind", rows)
#   return(table)
# }





# 
# 
# #first for a single scenario, then generalize to a list of scenarios
# #source("..//measures//Fitelson.R")
# 
# CoherencesRow3 <- function (BN, narrationNodes, states,
#                 statusLR = rep("Ind",FitelsonSize(length(narrationNodes))),
#                 statusRL = rep("Ind",FitelsonSize(length(narrationNodes))),
#                 exampleName){
#   FI <- FitelsonCoherenceForBNs(BN, narrationNodes,
#                               states = states, statusLR,statusRL)[[3]]
#   DM <- DouvenMeijsCoherenceForBNs(BN,narrationNodes,states)[[3]]
#   OL <- OlssonCoherenceForBNs(BN,narrationNodes,states)[[3]]
#   OLg <- OlssonGeneralizedCoherence(BN, narrationNodes, states)[[1]]
#   SH <- ShogenjiCoherenceForBNs(BN,narrationNodes,states)[[3]]
#   SHg <- ShogenjiGeneralizedCoherence(BN, narrationNodes, states)[[1]]
#   RO <- RocheCoherenceForBNs(BN,narrationNodes,states)[[3]]
#   RA <- RAcoherenceForBNs(BN,narrationNodes,states)[[3]]
#   
#   StructuredCon <- structuredCoherence(BN,narrationNodes,states)$`structured Coherence Con`
#   StructuredAnte <- structuredCoherence(BN,narrationNodes,states)$`structured Coherence Ante`
#   StructuredConS <- structuredCoherence(BN,narrationNodes,states)$`structured Coherence Con scaled`
#   StructuredAnteS <- structuredCoherence(BN,narrationNodes,states)$`structured Coherence Ante scaled`
#     
#   row <- data.frame( 
#              Ol = OL,
#              OlG = OLg,
#              Sh = SH,
#              ShG = SHg,
#              Fit = FI,
#              "DouvenMeijs" = DM,
#              Roche = RO,
#              RA = RA,
#              StrCon = StructuredCon,
#              StrAnte = StructuredAnte,
#              StrConS = StructuredConS,
#              StrAnteS = StructuredAnteS)
# 
# rownames(row) <- paste(paste(exampleName, ":", sep = ""), paste(narrationNodes, sep = "", collapse=""), paste(states, sep = "", collapse=""), sep = " ")
# 
# return(row)
# }
#   

# update to include narration weighting

# 
# CoherencesRow2 <- function (BN, narrationNodes, states,
#                            statusLR = rep("Ind",FitelsonSize(length(narrationNodes))),
#                            statusRL = rep("Ind",FitelsonSize(length(narrationNodes))),
#                            exampleName){
#   FI <- FitelsonCoherenceForBNs(BN, narrationNodes,
#                                 states = states, statusLR,statusRL)[[3]]
#   DM <- DouvenMeijsCoherenceForBNs(BN,narrationNodes,states)[[3]]
#   OL <- OlssonCoherenceForBNs(BN,narrationNodes,states)[[3]]
#   OLg <- OlssonGeneralizedCoherence(BN, narrationNodes, states)[[1]]
#   SH <- ShogenjiCoherenceForBNs(BN,narrationNodes,states)[[3]]
#   SHg <- ShogenjiGeneralizedCoherence(BN, narrationNodes, states)[[1]]
#   RO <- RocheCoherenceForBNs(BN,narrationNodes,states)[[3]]
#   RA <- RAcoherenceForBNs(BN,narrationNodes,states)[[3]]
#   
#   StructuredAnte <- structuredCoherence2(BN,narrationNodes,states)$`structured Coherence Ante`
#   StructuredAnteS <- structuredCoherence2(BN,narrationNodes,states)$`structured Coherence Ante scaled`
#   StructuredNarr <- structuredCoherence2(BN,narrationNodes,states)$`structured Coherence Narr`
#   StructuredNarrS <- structuredCoherence2(BN,narrationNodes,states)$`structured Coherence Narr scaled`
#   
#   
#     
#   row <- data.frame( 
#     Ol = OL,
#     OlG = OLg,
#     Sh = SH,
#     ShG = SHg,
#     Fit = FI,
#     "DouvenMeijs" = DM,
#     Roche = RO,
#     RA = RA,
#     StrAnte = StructuredAnte,
#     StrAnteS = StructuredAnteS,
#     StrNarr = StructuredNarr,
#     StrNarrS = StructuredNarrS)
#   
#   rownames(row) <- paste(paste(exampleName, ":", sep = ""), paste(narrationNodes, sep = "", collapse=""), paste(states, sep = "", collapse=""), sep = " ")
#   
#   return(row)
# }






#___________test on penguins

#test <- CoherencesRow(BN = BirdBN, narrationNodes = c("B","G","P"), states = c("1", "1", "1"), exampleName = "Penguins")






# 
# 
# 
# CoherencesTable2 <- function(BN,scenariosList,
#                             statesList,exampleName){
#   rows <- list()
#   for(s in 1:length(scenariosList)){
#     rows[[s]] <-  CoherencesRow2(BN,scenariosList[[s]],statesList[[s]], 
#                                 exampleName = exampleName)
#   }
#   table <- do.call("rbind", rows)
#   return(table)
# }
# 







