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
# 
# BN <-  BirdBNbgp
# narrationNodes <- c("B","G")
# states <- c("1","1")
# 
# graphviz.plot(BN)

#  BN <- robbersBN
#  narrationNodes <- c("MIsP","MIsR")
#  states <- c("1","1")
# # 
# # # 
# BN <- robbersBN
# narrationNodes <- c("MIsP","MIsR")
# narrationStates <- c("1","1")
# evidenceNodes <- c()
# evidenceStates <- c()


Z <- function(posterior,prior){
  d <- posterior - prior
  ifelse(prior == posterior, 0, ifelse(posterior > prior, d/(1-prior), d/prior))
}


# 
# structuredNarr(BN,narrationNodes, states)
# 
# structuredEvi(BN, narrationNodes, narrationStates)
# 
# CoherencesRowEvi(BN, narrationNodes, narrationStates, exampleName = "Robbers")
# 



structuredEvi <- function(BN, narrationNodes, narrationStates, evidenceNodes = c(), evidenceStates = c()){
  startTime <- proc.time() #start measuring computation time
  JN <- compile(as.grain(BN))
  length(evidenceNodes) > 0
  
  if(length(evidenceNodes) > 0){ 
        JNevidence <- setEvidence(JN,nodes = evidenceNodes, states = evidenceStates)
        } else {
        JNevidence <- JN
        }
  
  #find non-root nodes
  parented <- unique(arcs(BN)[,2])

  #assign parents to parented
  parentList <- list()
  for(node in 1:length(parented)){
    parentList[[node]] <- bnlearn::parents(BN,parented[node])
    }
  
  #initiate lists of results
  expConfFull <- list()
  ECSeviZ <- numeric(length(parented))
  ECSeviLR <-numeric(length(parented))
  ECSeviL <-numeric(length(parented))
  
  
  for (i in 1:length(parented)){
    
    consequent <- parented[i]
    consequentStates <- if (consequent %in% narrationNodes){
          stateOfNode(node = consequent, narrationNodes = narrationNodes, states = narrationStates)
          } else {
          findStates(consequent)
          }
  
    antecedents  <- parentList[i]
    antecedentStates <- list()
  
    for(a in 1:length(antecedents[[1]])){
          antecedentStates[[a]] <- if (antecedents[[1]][a] %in% narrationNodes){
                  stateOfNode(node = antecedents[[1]][a], narrationNodes = narrationNodes,
                    states = narrationStates)
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
              pr[s] <- as.numeric(querygrain(JN,
              nodes = consequent)[[1]][as.character(eval(parse(text = paste("variants$",consequent,"[s]",sep=""))))])
              }
    variants <- cbind(variants, priorCons = pr)

    #posteriors
    posteriors <- numeric(nrow(variants))
    for (row in 1:nrow(variants)){
              JNtemp <- setEvidence(JN, nodes = antecedents[[1]],
                      states = as.vector(unlist(variants[row,antecedents[[1]],drop= FALSE])))
              posteriors[row]   <-   querygrain(JNtemp, 
                    nodes = consequent)[[1]][as.character(eval(parse(text = paste("variants$",consequent,"[row]",sep=""))))]
              }
    
    posteriors[row] <- ifelse(is.na(posteriors[row]),0, posteriors[row])
    variants <- cbind(variants,posteriors)
 
    posteriorsNEG <- numeric(nrow(variants))
    for (row in 1:nrow(variants)){
            rowNodes <- as.vector(unlist(c(antecedents)))
            rowStates <- as.vector(unlist(variants[row,2:(length(antecedents[[1]])+1)]))
            JNAntIfH <- setEvidence(JN, nodes = consequent, 
                        states = as.vector(unlist(variants[row,consequent,drop= FALSE])))
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
            posteriorsNEG[row] <- ifelse(is.na(posteriorsNEG[row]),0,posteriorsNEG[row])
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
            prior <- eval(parse(text=final))
            priorAnte[row] <- prior
            }
    variants$PriorAnte <- priorAnte
    
    priorAnteEvi <- numeric(nrow(variants))
    
    for (row in 1:nrow(variants)){
            rowNodes <- as.vector(unlist(c(antecedents)))
            rowStates <- as.vector(unlist(variants[row,2:(length(antecedents[[1]])+1)]))
            PriorJoints <- querygrain(JNevidence,nodes=rowNodes,type="joint")
            PriorJoints <- aperm(PriorJoints, rowNodes)
            steps <- numeric(length(rowNodes))
            for(rn in 1:length(rowNodes)){
                  steps[rn] <- paste(rowNodes[rn], "=", "\"",rowStates[rn],"\"")
                  }
            steps<- gsub(" ", "", steps, fixed = TRUE)
            final <- paste("PriorJoints[",paste(steps,collapse=","),"]",sep="")
            PriorJoints <- if(length(PriorJoints) == 1){
                        PriorJoints
                        } else {
                        aperm(PriorJoints, rowNodes)
                        }
        
            priorAnteEvi[row]  <- if(length(PriorJoints) == 1) PriorJoints else  eval(parse(text=final)) 
            variants$PriorAnteEvi <- priorAnteEvi
            }
            
  
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
    if(sum(variants$PriorAnteEvi) > 0){
            variants$WeightsAnteEvi <-  variants$PriorAnteEvi / sum(variants$PriorAnteEvi)
            } else {
            variants$WeightsAnteEvi <-  1/nrow(variants)
            }
    
    #measures
    variants$Z <- Z(posterior = variants$posteriors, prior = variants$priorCons)
    variants$LR <-  variants$AnteIfC / variants$AnteIfnC
    variants$L <- (variants$AnteIfC - variants$AnteIfnC) / (variants$AnteIfC + variants$AnteIfnC)
    variants$ZscaledEvi <- variants$Z * variants$WeightsAnteEvi
    variants$LRscaledEvi <- variants$LR * variants$WeightsAnteEvi
    #variants$LRscaledAnte <- variants$LR * variants$WeightsAnte
    variants$LscaledEvi <- variants$L * variants$WeightsAnteEvi

    
    structuredScoreNoSD <- function(ECS)  if (is.na(min(ECS))){
                NA
                } else {
                if (min(ECS) <= 0) {
                        (mean(ECS)) * (min(ECS) +1) - min(ECS)^2
                        } else {
                        (mean(ECS))
                }
    }
        

    expConfFull[[i]] <- list( "Consequent node" = consequent,
                          "Options & calculations" = variants,
                          "ECSeviZ scaled" = sum(variants$ZscaledEvi, na.rm =TRUE),
                          "ECSeviLR scaled" = sum(variants$LRscaledEvi, na.rm = TRUE),
                          "ECSeviL scaled" = sum(variants$LscaledEvi, na.rm = TRUE)
                          )

    ECSeviZ[i] <- ifelse(sum(is.infinite(variants$ZscaledEvi))>=1,Inf,
                  sum(variants$ZscaledEvi))
    ECSeviLR[i] <- ifelse(sum(is.infinite(variants$LRscaledEvi))>=1,Inf,sum(variants$LRscaledEvi))
    ECSeviL[i] <- ifelse(sum(is.infinite(variants$LscaledEvi))>=1,Inf,
                          sum(variants$LscaledEvi))
    }
  
  stopTime <- proc.time()
  elapsedTime <- stopTime - startTime
  
  return(list("Full calculations" = expConfFull, 
              "ECSeviZ" = ECSeviZ,
              "ECSeviLR" = ECSeviLR,
              "ECSeviLR" = ECSeviL,
              "structuredZevi" = structuredScoreNoSD(ECSeviZ),
              "structuredLRevi" =
                structuredScoreNoSD(ECSeviLR),
              "structuredLevi" =
                structuredScoreNoSD(ECSeviL),
              "Computation time" = elapsedTime))
  
}
    
