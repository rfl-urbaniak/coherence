
#test
#structuredCoherence(BirdBN,narrationNodes,states)[[3]]




structuredCoherence <- function(BN, narrationNodes, states){
startTime <- proc.time() #start measuring computation time
JN <- compile(as.grain(BN))

#find non-root nodes
parented <- unique(arcs(BN)[,2])

#assign parents to parented
parentList <- list()
for(node in 1:length(parented)){
  parentList[[node]] <- parents(BN,parented[node])  
}


#initiate lists of results
expConfFull <- list()
ECScon <- numeric(length(parented))
ECSante <- numeric(length(parented))
ECSconS <- numeric(length(parented))
ECSanteS <- numeric(length(parented))



for (i in 1:length(parented)){
consequent <- parented[i]

consequentStates <- if (consequent %in% narrationNodes){
  stateOfNode(node = consequent, narrationNodes = narrationNodes, states = states)    
} else {
  findStates(consequent)
}

antecedents  <- parentList[i]
antecedents

antecedentStates <- list()
for(a in 1:length(antecedents[[1]])){
  antecedentStates[[a]] <- if (antecedents[[1]][a] %in% narrationNodes){
    stateOfNode(node = antecedents[[1]][a], narrationNodes = narrationNodes, states = states)    
  } else {
    findStates(antecedents[[1]][a])
  }
}

#start the otucome table
variants <-  expand.grid(c(list(consequentStates),antecedentStates))
colnames(variants) <- c(consequent,antecedents[[1]])

#add the prior of consequent, to be used for Z calculation
pr <- numeric(nrow(variants))
for (s in 1:nrow(variants)){
pr[s] <- as.numeric(querygrain(JN, nodes = consequent)[[1]][as.character(eval(parse(text = paste("variants$",consequent,"[s]",sep=""))))])
}
variants <- cbind(variants, priorCons = pr)

#posteriors 
posteriors <- numeric(nrow(variants))
for (row in 1:nrow(variants)){
JNtemp <- setEvidence(JN, nodes = antecedents[[1]], states = as.vector(unlist(variants[row,antecedents[[1]],drop= FALSE]))  )
posteriors[row]   <-   querygrain(JNtemp, nodes = consequent)[[1]][as.character(eval(parse(text = paste("variants$",consequent,"[row]",sep=""))))]
}
variants <- cbind(variants,posteriors)

#priors for conjunctions
priorJoint <- numeric(nrow(variants))

for (row in 1:nrow(variants)){
  rowNodes <- as.vector(unlist(c(consequent,antecedents)))
  rowStates <- as.vector(unlist(variants[row,1:(length(antecedents[[1]])+1)]))
  PriorJoints <- querygrain(JN,nodes=rowNodes,type="joint")
  PriorJoints <- aperm(PriorJoints, rowNodes)
steps <- numeric(length(rowNodes))
  for(rn in 1:length(rowNodes)){
    steps[rn] <- paste(rowNodes[rn], "=", "\"",rowStates[rn],"\"")
  }
  steps<- gsub(" ", "", steps, fixed = TRUE)
  final <- paste("PriorJoints[",paste(steps,collapse=","),"]",sep="")
  prior <- eval(parse(text=final))
  priorJoint[row] <- prior
}

variants$PriorJoint <- priorJoint

#priors for the antecedents
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

#scaled weights for conjunction
if(sum(variants$PriorJoint) > 0){
variants$WeightsCon <-  variants$PriorJoint / sum(variants$PriorJoint)
} else {
variants$WeightsCon <-  1/nrow(variants)
}

#scaled weights for antecedent
if(sum(variants$PriorAnte) > 0){
  variants$WeightsAnte <-  variants$PriorAnte / sum(variants$PriorAnte)
} else {
  variants$WeightsAnte <-  1/nrow(variants)
}


#Z measures
variants$Z <- Z(posterior = variants$posteriors, prior = variants$priorCons)
#variants$Zweighted <- variants$Z * variants$Weights
variants$ZweightedCon <- variants$Z * variants$PriorJoint
variants$ZweightedAnte <- variants$Z * variants$PriorAnte
variants$ZscaledCon <- variants$Z * variants$WeightsCon
variants$ZscaledAnte <- variants$Z * variants$WeightsAnte



expConfFull[[i]] <- list( "Consequent node" = consequent,
                      "Options & calculations" = variants, "ECScon" = sum(variants$ZweightedCon),
                      "ECSante" = sum(variants$ZweightedAnte), "ECScon scaled" = sum(variants$ZscaledCon),
                      "ECSante scaled" = sum(variants$ZscaledAnte))


ECScon <- numeric(length(parented))
ECSante <- numeric(length(parented))
ECSconS <- numeric(length(parented))
ECSanteS <- numeric(length(parented))

ECScon[i]   <-  sum(variants$ZweightedCon)
ECSante[i]  <- sum(variants$ZweightedAnte) 
ECSconS[i]  <- sum(variants$ZscaledCon)
ECSanteS[i] <- sum(variants$ZscaledAnte)
}

#expConf

populationSD <- function( vector ){
  sqrt(sum((vector - mean(vector))^2)/(length(vector)))
}

structuredScore <- function(expConf)  if (min(expConf) <= 0) {
  (mean(expConf) - populationSD(expConf)) * (min(expConf +1)) - min(expConf)^2
} else {
  (mean(expConf) - populationSD(expConf))
}

stopTime <- proc.time()
elapsedTime <- stopTime - startTime

return(list("Full calculations" = expConfFull, "ECScon" = ECScon, "ECSante" = ECSante,
            "ECSconS" = ECSconS,
            "ECSanteS" = ECSanteS,
            "structured Coherence Con" = structuredScore(ECScon), 
            "structured Coherence Ante" = structuredScore(ECSante),
            "structured Coherence Con scaled" = structuredScore(ECSconS),
            "structured Coherence Ante scaled" = structuredScore(ECSanteS),
            "Computation time" = elapsedTime))
}


