
Rafal <- function(px,pyx,pynx){
  pnx <- 1-px
  py <- pyx*px + pynx * pnx 
  pny <- 1-py
  pnyx <- 1-pyx
  pnynx <- 1-pynx
  Z(pyx,py) * px+ Z(pynx,py) * pnx + Z(pnyx,pny) * px + Z(pnynx,pny) * pnx
}

Alicja <-  function(px,pyx,pynx){
  pnx <- 1-px
  py <- pyx*px + pynx *  pnx
  pny <- 1-py
  pnyx <- 1-pyx
  pnynx <- 1-pynx
  Z(pyx,py) * px * py + Z(pynx,py) * pnx * py + Z(pnyx,pny) * px * pny + Z(pnynx,pny) * pnx * pny
}

Joint <-  function(px,pyx,pynx){
  pnx <- 1-px
  py <- pyx*px + pynx *  pnx
  pny <- 1-py
  pnyx <- 1-pyx
  pnynx <- 1-pynx
  XY <- pyx * px
  XnY <- pnyx * px
  nXY <-pynx * pnx
  nXnY <- pnynx * pnx
  Z(pyx,py) * XY  + Z(pynx,py) * nXY + Z(pnyx,pny) * XnY + Z(pnynx,pny) * nXnY
}

pyx <- seq(0,1,by=0.01)
pynx <- seq(0,1,by=0.01)
options <- expand.grid(pyx,pynx)
colnames(options) <- c("pyx","pynx")

options$r1 <- Rafal(.1,options$pyx,options$pynx)
options$a1 <- Alicja(.1,options$pyx,options$pynx)
options$j1 <- Joint(.1,options$pyx,options$pynx)




rplot1 <- scatter3D(options$pyx,options$pynx,options$r1,pch=19,cex=.2,byt="g",alpha=0.6,theta=30, phi=20,ticktype="simple",xlab="pyx", ylab="pynx",zlab="rafal",main="Rafal,  prior x =.1",colvar=NULL)


aplot1 <- scatter3D(options$pyx,options$pynx,options$a1,pch=19,cex=.2,byt="g",alpha=.6,theta=40, phi=30,ticktype="simple",xlab="pyx",ylab="pynx",zlab="alicja",  main="Alicja, prior x =.1",colvar=NULL)

jplot1 <- scatter3D(options$pyx,options$pynx,options$j1,pch=19,cex=.2,byt="g",alpha=.6,theta=40, phi=30,ticktype="simple",xlab="pyx",ylab="pynx",zlab="joint",
                    main="Joint, prior x =.1",colvar=NULL)


options$r2 <- Rafal(.5,options$pyx,options$pynx)
options$a2 <- Alicja(.5,options$pyx,options$pynx)
options$j2 <- Joint(.5,options$pyx,options$pynx)



rplot2 <- scatter3D(options$pyx,options$pynx,options$r2,pch=19,cex=.2,byt="g",alpha=.6,theta=40, phi=30,ticktype="simple",xlab="pyx",ylab="pynx",zlab="rafal",
                    main="Rafal, prior x =.5",colvar=NULL)

aplot2 <- scatter3D(options$pyx,options$pynx,options$a2,pch=19,cex=.2,byt="g",alpha=.6,theta=20, phi=30,ticktype="simple",xlab="pyx",ylab="pynx",zlab="alicja",
                    main="Alicja, prior x =.5",colvar=NULL)

jplot2 <- scatter3D(options$pyx,options$pynx,options$j2,pch=19,cex=.2,byt="g",alpha=.6,theta=25, phi=30,ticktype="simple",xlab="pyx",ylab="pynx",zlab="joint",
                    main="Joint, prior x =.5",colvar=NULL)


#aplot2


options$r3 <- Rafal(.9,options$pyx,options$pynx)
options$a3 <- Alicja(.9,options$pyx,options$pynx)
options$j3 <- Joint(.9,options$pyx,options$pynx)


rplot3 <- scatter3D(options$pyx,options$pynx,options$r2,pch=19,cex=.2,byt="g",alpha=.6,theta=40, phi=30,ticktype="simple",xlab="pyx",ylab="pynx",zlab="rafal",
                    main="Rafal, prior x =.9",colvar=NULL)

aplot3 <- scatter3D(options$pyx,options$pynx,options$a2,pch=19,cex=.2,byt="g",alpha=.6,theta=20, phi=30,ticktype="simple",xlab="pyx",ylab="pynx",zlab="alicja",
                    main="Alicja, prior x =.9",colvar=NULL)

jplot3 <- scatter3D(options$pyx,options$pynx,options$j2,pch=19,cex=.2,byt="g",alpha=.6,theta=15, phi=40,ticktype="simple",xlab="pyx",ylab="pynx",zlab="joint",
                    main="Joint, prior x =.9",colvar=NULL)


#jplot1
#jplot2 
#jplot3


#_________________________-

#test
#structuredCoherence(BirdBN,narrationNodes,states)[[3]]


BN <- robbersBN

graphviz.plot(BN)

narrationNodes <- c("MIsP","MIsR")
states <- c("1","1")

structuredCoherence(robbersBN,c("MIsP","MIsR"),c("1","1"))


structuredCoherence <- function(BN,narrationNodes,states){

JN <- compile(as.grain(BN))

#find non-root nodes
parented <- unique(arcs(BN)[,2])

parented

#assign parents to parented
parentList <- list()
for(node in 1:length(parented)){
  parentList[[node]] <- parents(BN,parented[node])  
}


parentList

#initiate lists of results
expConfFull <- list()
expConf <- numeric(length(parented))


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
    stateOfNode(node = antecedents[[1]][a], narrationNodes = narrationNodes, states = states)    
  } else {
    findStates(antecedents[[1]][a])
  }
}



#start the otucome table
variants <-  expand.grid(c(list(consequentStates),antecedentStates))
colnames(variants) <- c(consequent,antecedents[[1]])


variants


#add the prior of consequent, to be used for Z calculation
pr <- numeric(nrow(variants))
for (s in 1:nrow(variants)){
pr[s] <- as.numeric(querygrain(JN, nodes = consequent)[[1]][as.character(eval(parse(text = paste("variants$",consequent,"[s]",sep=""))))])
}
variants <- cbind(variants, priorCons = pr)


variants


#and so for posteriors and weights
posteriors <- numeric(nrow(variants))
for (row in 1:nrow(variants)){
JNtemp <- setEvidence(JN, nodes = antecedents[[1]], states = as.vector(unlist(variants[row,antecedents[[1]],drop= FALSE]))  )
posteriors[row]   <-   querygrain(JNtemp, nodes = consequent)[[1]][as.character(eval(parse(text = paste("variants$",consequent,"[row]",sep=""))))]
}
variants <- cbind(variants,posteriors)


variants


#weighing with conjunctions
weights <- numeric(nrow(variants))

for (row in 1:nrow(variants)){
  rowNodes <- as.vector(unlist(c(consequent,antecedents)))
  rowStates <- as.vector(unlist(variants[row,1:(length(antecedents[[1]])+1)]))
  
  PriorJoints <- querygrain(JN,nodes=rowNodes,type="joint")
  PriorJoints <- aperm(PriorJoints, rowNodes)
  
#node.ordering(BN)
#nodes(BN)
#rowNodes[order(match(rowNodes,node.ordering(BN)))]
#rowNodesOrdered <- rowNodes[order(match(rowNodes,nodes(BN)))]
#rowStatesOrdered <- rowStates[order(match(rowNodes,nodes(BN)))] 
#  rowNodesOrdered <- rowNodes[order(match(rowNodes,node.ordering(BN)))]
#  rowStatesOrdered <- rowStates[order(match(rowNodes,node.ordering(BN)))] 
#rowNodesOrdered <- rowNodes[order(match(rowNodes,dimnames(PriorJoints)))]

steps <- numeric(length(rowNodesOrdered))
  for(rn in 1:length(rowNodesOrdered)){
    steps[rn] <- paste(rowNodesOrdered[rn], "=", "\"",rowStatesOrdered[rn],"\"")
  }
  steps<- gsub(" ", "", steps, fixed = TRUE)
  steps
  final <- paste("PriorJoints[",paste(steps,collapse=","),"]",sep="")
  noquote(final)
  prior <- eval(parse(text=final))
  weights[row] <- prior
}

#weights
# #weighing with conjunctions
# for (row in 1:nrow(variants)){
#   rowNodes <- as.vector(unlist(c(consequent,antecedents)))
#   rowStates <- as.vector(unlist(variants[row,1:(length(antecedents[[1]])+1)]))
#   weights[row] <- FindPriorJN(JN,nodes = rowNodes, states = rowStates)
# }


# #weighting with antecedents
# for (row in 1:nrow(variants)){
#   rowNodes <- as.vector(unlist(c(antecedents)))
#   rowStates <- as.vector(unlist(variants[row,2:(length(antecedents[[1]])+1)]))
#   weights[row] <- FindPriorJN(JN,nodes = rowNodes, states = rowStates)
# }


variants$JointPr <- weights

if(sum(variants$JointPr) > 0){
variants$Weights <-  variants$JointPr / sum(variants$JointPr)
} else {
variants$Weights <-  1/nrow(variants)
}


variants


#add Zs
variants$Z <- Z(posterior = variants$posteriors, prior = variants$priorCons)
#variants$Zweighted <- variants$Z * variants$Weights
variants$Zweighted <- variants$Z * variants$Weights


#sum(variants$Zweighted)


expConfFull[[i]] <- list( "Consequent node" = consequent,
                      "Options & calculations" = variants, "Expected arrow strength" = sum(variants$Zweighted))



expConf[i] <- sum(variants$Zweighted)
}

#expConf
expConfFull


populationSD <- function( vector ){
  sqrt(sum((vector - mean(vector))^2)/(length(vector)))
}


structuredScore <- function(expConf)  if (min(expConf) <= 0) {
  (mean(expConf) - populationSD(expConf)) * (min(expConf +1)) - min(expConf)^2
} else {
  (mean(expConf) - populationSD(expConf))
}



return(list("Full calculations" = expConfFull, "Expected influences" = expConf, "structured Coherence" = structuredScore(expConf)))
}

