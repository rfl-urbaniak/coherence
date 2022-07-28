# structuredCoherenceNarration(W12BN, c("W1","W2"),c("1","1"))

# 
# BN <- W12BN
# narrationNodes <- c("W1","W2")
# states <-  c("1","1")



# 
# 
# BN <- RegularBN
# narrationNodes  <- c("T","TF")
# states <-  c("1","1")


structuredCoherenceNarration <- function(BN, narrationNodes, states){
  startTime <- proc.time() #start measuring computation time
  JN <- compile(as.grain(BN))
  
  JNnarration <- setEvidence(JN,nodes = narrationNodes, states = states)
  
  
  #find non-root nodes
  parented <- unique(arcs(BN)[,2])
  
  #parented
  
  #assign parents to parented
  parentList <- list()
  for(node in 1:length(parented)){
    parentList[[node]] <- bnlearn::parents(BN,parented[node])
  }
  
  #initiate lists of results
  expConfFull <- list()
  ECSnarrZ <- numeric(length(parented))
  ECSnarrLR <-numeric(length(parented))
  ECSnarrL <-numeric(length(parented))
  
  
  for (i in 1:length(parented)){
  
    consequent <- parented[i]
    
    
    consequentStates <- if (consequent %in% narrationNodes){
      stateOfNode(node = consequent, narrationNodes = narrationNodes, states = states)
    } else {
      findStates(consequent)
    }
    
    
    
    antecedents  <- parentList[i]
    
    
    antecedentStates <- list()
    for(a in 1:length(antecedents[[1]])){
      antecedentStates[[a]] <- if (antecedents[[1]][a] %in% narrationNodes){
        stateOfNode(node = antecedents[[1]][a], narrationNodes = narrationNodes,
                    states = states)
      } else if (!(antecedents[[1]][a] %in% parented)){
        eval(parse(text = paste('dimnames(BN$',antecedents[[1]][a],'$prob)',
                                sep= "")))[[1]]
      } else {
        findStates(antecedents[[1]][a])
      }
    }
    
    
    
    
    #start the outcome table
    variants <-  expand.grid(c(list(consequentStates),antecedentStates))
    colnames(variants) <- c(consequent,antecedents[[1]])
    
    
    
    
    #add the prior of consequent, to be used for Z calculation
    pr <- numeric(nrow(variants))
    for (s in 1:nrow(variants)){
      pr[s] <- as.numeric(querygrain(JN, nodes = consequent)[[1]][as.character(eval(parse(text = paste("variants$",consequent,"[s]",sep=""))))])
    }
    variants <- cbind(variants, priorCons = pr)
    
    #variants
    
    #posteriors
    posteriors <- numeric(nrow(variants))
    for (row in 1:nrow(variants)){
      JNtemp <- setEvidence(JN, nodes = antecedents[[1]], states = as.vector(unlist(variants[row,antecedents[[1]],drop= FALSE]))  )
      posteriors[row]   <-   querygrain(JNtemp, nodes = consequent)[[1]][as.character(eval(parse(text = paste("variants$",consequent,"[row]",sep=""))))]
    }
    variants <- cbind(variants,posteriors)
    
    
    #variants
    
    posteriorsNEG <- numeric(nrow(variants))
    for (row in 1:nrow(variants)){
      rowNodes <- as.vector(unlist(c(antecedents)))
      rowStates <- as.vector(unlist(variants[row,2:(length(antecedents[[1]])+1)]))
      JNAntIfH <- setEvidence(JN, nodes = consequent, states = as.vector(unlist(variants[row,consequent,drop= FALSE])))
      JointAntIfH <- querygrain(JNAntIfH, nodes = antecedents[[1]], type = "joint" )
      JointAntIfH <- aperm(JointAntIfH, rowNodes)
      steps <- numeric(length(rowNodes))
      for(rn in 1:length(rowNodes)){
        steps[rn] <- paste(rowNodes[rn], "=", "\"",rowStates[rn],"\"")
      }
      steps<- gsub(" ", "", steps, fixed = TRUE)
      final <- paste("JointAntIfH[",paste(steps,collapse=","),"]",sep="")
      pAntIfH <- eval(parse(text=final))
      
      JointAnt <- querygrain(JN, nodes = antecedents[[1]], type = "joint" )
      JointAnt <- aperm(JointAnt, rowNodes)
      steps <- numeric(length(rowNodes))
      for(rn in 1:length(rowNodes)){
        steps[rn] <- paste(rowNodes[rn], "=", "\"",rowStates[rn],"\"")
      }
      steps<- gsub(" ", "", steps, fixed = TRUE)
      final <- paste("JointAnt[",paste(steps,collapse=","),"]",sep="")
      pAnt <- eval(parse(text=final))
      posteriorsNEG[row] <- ((1 - pAntIfH) * variants$priorCons[row])/ (1 - pAnt)
    }
    variants <- cbind(variants, posteriorsNEG = posteriorsNEG)
    
    
    
    priorAnte <- numeric(nrow(variants))
    
    for (row in 1:nrow(variants)){
      rowNodes <- as.vector(unlist(c(antecedents)))
      rowStates <- as.vector(unlist(variants[row,2:(length(antecedents[[1]])+1)]))
      PriorJoints <- querygrain(JN,nodes=rowNodes,type="joint")
      PriorJoints <- aperm(PriorJoints, rowNodes)
      steps <- numeric(length(rowNodes))
      for(rn in 1:length(rowNodes)){
        steps[rn] <- paste(rowNodes[rn], "=", "\"",rowStates[rn],"\"")
      }
      steps<- gsub(" ", "", steps, fixed = TRUE)
      final <- paste("PriorJoints[",paste(steps,collapse=","),"]",sep="")
      noquote(final)
      prior <- eval(parse(text=final))
      priorAnte[row] <- prior
    }
    variants$PriorAnte <- priorAnte
    
    #variants
    
    priorAnteNarr <- numeric(nrow(variants))
    
    for (row in 1:nrow(variants)){
      rowNodes <- as.vector(unlist(c(antecedents)))
      rowStates <- as.vector(unlist(variants[row,2:(length(antecedents[[1]])+1)]))
      PriorJoints <- querygrain(JNnarration,nodes=rowNodes,type="joint")
      #PriorJoints
      PriorJoints <- if(length(PriorJoints) == 1){
      # PriorJoints
      } else {
        aperm(PriorJoints, rowNodes)
      }
      steps <- numeric(length(rowNodes))
      for(rn in 1:length(rowNodes)){
        steps[rn] <- paste(rowNodes[rn], "=", "\"",rowStates[rn],"\"")
      }
      steps<- gsub(" ", "", steps, fixed = TRUE)
      final <- paste("PriorJoints[",paste(steps,collapse=","),"]",sep="")
      noquote(final)
      prior <- eval(parse(text=final))
      priorAnteNarr[row] <- ifelse(length(PriorJoints) == 1,PriorJoints,prior)
    }
    variants$PriorAnteNarr <- priorAnteNarr
    
    #variants
    AnteIfC <- numeric(nrow(variants))
    AnteIfnC <- numeric(nrow(variants)) 
    
    
    for (row in 1:nrow(variants)){
      rowNodes <- as.vector(unlist(c(antecedents)))
      rowStates <- as.vector(unlist(variants[row,2:(length(antecedents[[1]])+1)]))
      
      conNode <- as.vector(unlist(c(consequent)))
      conState <- as.character(variants[row,1])
      conStateN <- as.character(1 - as.numeric(conState))
      
      JNC <- setEvidence(JN, nodes = conNode, states = conState) 
      JNnC   <- setEvidence(JN, nodes = conNode, states = conStateN)
      
      AnteIfCJoints <- querygrain(JNC,nodes=rowNodes,type="joint")
      AnteIfnCJoints <- querygrain(JNnC,nodes=rowNodes,type="joint")
      
      AnteIfCJoints <- aperm(AnteIfCJoints, rowNodes)
      AnteIfnCJoints <- aperm(AnteIfnCJoints, rowNodes)
      
      steps <- numeric(length(rowNodes))
      
      for(rn in 1:length(rowNodes)){
        steps[rn] <- paste(rowNodes[rn], "=", "\"",rowStates[rn],"\"")
      }
      steps<- gsub(" ", "", steps, fixed = TRUE)
      
      finalC <- paste("AnteIfCJoints[",paste(steps,collapse=","),"]",sep="")
      finalnC <- paste("AnteIfnCJoints[",paste(steps,collapse=","),"]",sep="")
      
      anteifC <- eval(parse(text=finalC))
      anteifnC <- eval(parse(text=finalnC))
      
      AnteIfC[row] <- anteifC
      AnteIfnC[row] <- anteifnC
    }
    
    variants$AnteIfC <- AnteIfC
    variants$AnteIfnC <- AnteIfnC
    
    
    
    
    
    
    #scaled weights for antecedent
    if(sum(variants$PriorAnte) > 0){
      variants$WeightsAnte <-  variants$PriorAnte / sum(variants$PriorAnte)
    } else {
      variants$WeightsAnte <-  1/nrow(variants)
    }
    
    
    
    #scaled weights for narrationstructuredCoherenceUpdated(W12BN,A,c("1","1"))
    if(sum(variants$PriorAnteNarr) > 0){
      variants$WeightsAnteNarr <-  variants$PriorAnteNarr / sum(variants$PriorAnteNarr)
    } else {
      variants$WeightsAnteNarr <-  1/nrow(variants)
    }
    
    
    
    
    
    #measures
    variants$Z <- Z(posterior = variants$posteriors, prior = variants$priorCons)
    variants$LR <-  variants$AnteIfC / variants$AnteIfnC
    variants$L <- (variants$AnteIfC - variants$AnteIfnC) / (variants$AnteIfC + variants$AnteIfnC)
    #variants$Zweighted <- variants$Z * variants$Weights
    #variants$ZweightedCon <- variants$Z * variants$PriorJoint
    #variants$ZweightedAnte <- variants$Z * variants$PriorAnte
    #variants$ZscaledCon <- variants$Z * variants$WeightsCon
  #  variants$ZscaledAnte <- variants$Z * variants$WeightsAnte
    
    #narration weighted measures
    variants$ZscaledNarr <- variants$Z * variants$WeightsAnteNarr
    variants$LRscaledNarr <- variants$LR * variants$WeightsAnteNarr
    #variants$LRscaledAnte <- variants$LR * variants$WeightsAnte
    variants$LscaledNarr <- variants$L * variants$WeightsAnteNarr
    
    
    
    
    
    expConfFull[[i]] <- list( "Consequent node" = consequent,
                              "Options & calculations" = variants,
                              "ECSnarrZ scaled" = sum(variants$ZscaledNarr),
                              "ECSnarrLR scaled" = sum(variants$LRscaledNarr),
                              "ECSnarrL scaled" = sum(variants$LscaledNarr)
                              )
    ECSnarrZ[i] <- sum(variants$ZscaledNarr)
    ECSnarrLR[i] <- sum(variants$LRscaledNarr)
    ECSnarrL[i] <- sum(variants$LscaledNarr)
    
    
    #ECSanteS[i] <- sum(variants$ZscaledAnte)
    #ECSnarrS[i] <- sum(variants$ZscaledNarr)
  }
  
  #ECSnarrZ
  #ECSnarrLR
  #ECSnarrL
  
  # populationSD <- function( vector ){
  #   sqrt(sum((vector - mean(vector))^2)/(length(vector)))
  # }
  
  # structuredScore <- function(expConf)  if (min(expConf) <= 0) {
  #   (mean(expConf) - populationSD(expConf)) * (min(expConf) +1) - min(expConf)^2
  # } else {
  #   (mean(expConf) - populationSD(expConf))
  # }
  # 
  # 
  # structuredScoreSquared <- function(ECS)  if (min(ECS) <= 0) {
  #   (mean(ECS) - populationSD(ECS)^2) * (min(ECS) +1) - min(ECS)^2
  # } else {
  #   (mean(ECS) - populationSD(ECS)^2)
  # }
  # 
  
  structuredScoreNoSD <- function(ECS)  if (is.na(min(ECS))){
    NA
  } else {
  if (min(ECS) <= 0) {
    (mean(ECS)) * (min(ECS) +1) - min(ECS)^2
  } else {
    (mean(ECS))
  }
    }
    
  
#  structuredScoreNoSD(ECSnarrZ)
  
#  structuredScoreNoSD(ECSnarrLR)
  
#  min(ECSnarrLR) <= 0
  
  #structuredScoreNoSD(ECSnarrZ)
  #structuredScoreNoSD(ECSnarrLR)
  #structuredScoreNoSD(ECSnarrL)
  
  stopTime <- proc.time()
  elapsedTime <- stopTime - startTime
  
  return(list("Full calculations" = expConfFull, 
              "ECSnarrZ" = ECSnarrZ,
              "ECSnarrLR" = ECSnarrLR,
              "ECSnarrLR" = ECSnarrL,
              "structuredZnarr" = structuredScoreNoSD(ECSnarrZ),
              "structuredLRnarr" =
 structuredScoreNoSD(ECSnarrLR),
              "structuredLnarr" =
   structuredScoreNoSD(ECSnarrL),
              "Computation time" = elapsedTime))
  # 
  # 
  #   
  # 
  #   return(list("Full calculations" = expConfFull,
  #               "ECSanteS" = ECSanteS,
  #               "ECSnarrS" = ECSnarrS,
  #               "structured Coherence Ante scaled" = structuredScore(ECSanteS),
  #               "structured Coherence Narr scaled" = structuredScore(ECSnarrS),
  #               "Computation time" = elapsedTime))
}
# 
# 
# 
# 
# 
# 



