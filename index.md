---
title: "Probabilistic coherence in Bayesian networks"
layout: page
toc: true
output:
  md_document:
    variant: markdown_github
    preserve_yaml: true
---


### Brief description

This is a short manual for the coherence calculations of various measures of coherence discussed in our paper with [Alicja Kowalewska](https://www.cmu.edu/dietrich/philosophy/people/phd/alicja-kowalewska.html), published in *AI & Law*.


- scripts containing definitions of functions calculating various coherence measures are in the folder [measures](https://github.com/rfl-urbaniak/coherence/tree/master/code/measures).
- a bunch of wrappers we use are defined in scripts to be found in the [utils](https://github.com/rfl-urbaniak/coherence/tree/master/code/utils) folder.



**Contents**
* TOC
{:toc}


### Installing and loading packages


First, you need to install the relevant R libraries. The only tricky package to install is `bnlearn`. This is a bit
tricky, because some of them have to be installed through BiocManager.
One way to go is this:

``` r
install.packages("https://www.bnlearn.com/releases/bnlearn_latest.tar.gz", repos = NULL, type = "source")
install.packages("BiocManager")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install()
BiocManager::install(c("graph", "Rgraphviz"))

install.packages("https://www.bnlearn.com/releases/bnlearn_latest.tar.gz", repos = NULL, type = "source")
```

Then load the libraries we use (you need to have them installed first):

``` r
library(bnlearn)
library(Rgraphviz)
library(gRain)
```
### Loading scripts and setup

Supposing you have the working directory and the folders in it, load the functions:

``` r
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
```

Define also the Z confirmation measure

``` r
Z <- function(posterior,prior){
  d <- posterior - prior
  ifelse(prior == posterior, 0, ifelse(posterior > prior, d/(1-prior), d/prior))
}
```




### Building BNs

Let's work through the example of the Sally Clark BN.

First, build the DAGs.

``` r
scStage0DAG <- model2network("[Amurder][Bmurder|Amurder]")
scDAG <- model2network("[Abruising|Amurder][Adisease|Amurder][Bbruising|Bmurder][Bdisease|Bmurder][Amurder][Bmurder|Amurder]")
graphviz.plot(scStage0DAG)
graphviz.plot(scDAG)
```

Now build CPTs and put them together with the DAGs.

``` r
AmurderProb <-prior.CPT("Amurder","0","1",0.921659)
AbruisingProb <- single.CPT("Abruising","Amurder","1","0","0","1",0.01,0.05)
AdiseaseProb <- single.CPT("Adisease","Amurder","1","0","0","1",0.05,0.001)
BbruisingProb <- single.CPT("Bbruising","Bmurder","1","0","0","1",0.01,0.05)
BdiseaseProb <- single.CPT("Bdisease","Bmurder","1","0","0","1",0.05,0.001)
BmurderProb <- single.CPT("Bmurder","Amurder","0","1","0","1",0.9993604,1-0.9998538)

scStage0CPT <-  list(Amurder=AmurderProb,
                     Bmurder = BmurderProb)

scCPT <- list(Amurder=AmurderProb,Adisease = AdiseaseProb,
                      Bmurder = BmurderProb,Bdisease=BdiseaseProb,
                      Abruising = AbruisingProb,Bbruising = BbruisingProb)
              
scStage0BN <- custom.fit(scStage0DAG, scStage0CPT)
scBN <- custom.fit(scDAG,scCPT)
```



### Coherence calculations

As arguments to coherence-calculating functions we need to indicate not only the BN, but also which nodes are the narration nodes, and what states of those nodes are part of the narration. Like this:


``` r
scStage0nodes <- c("Amurder","Bmurder")
scNodes <- c("Amurder","Bmurder", "Abruising", "Bbruising","Adisease","Bdisease")

BN <- scStage0BN

structuredNoSD(scStage0BN,scStage0nodes,c("0","0"))
#or
(structuredNoSD(scBN, SCnodes,c("0","0","1","1","0","0"))
```

The output should be self-explanatory given the content of the paper. If you just care about the final scores of all possible scenarios, do this:

``` r
#calculate structured coherence of all st
sc0structured <- round(c(structuredNoSD(scStage0BN,scStage0nodes,c("0","0"))$structuredNoSD,
                         structuredNoSD(scStage0BN,scStage0nodes,c("1","1"))$structuredNoSD,
                         structuredNoSD(scStage0BN,scStage0nodes,c("0","1"))$structuredNoSD,
                         structuredNoSD(scStage0BN,scStage0nodes,c("1","0"))$structuredNoSD),4)
```

Similarly, you can calculate other coherence scores:

``` r
FitelsonCoherenceForBNs(scStage0BN,scStage0nodes,c("0","0"))
DouvenMeijsCoherenceForBNs(scStage0BN,scStage0nodes,c("0","1"))
OlssonCoherenceForBNs(scStage0BN,scStage0nodes,c("0","0"))
ShogenjiCoherenceForBNs(scStage0BN,scStage0nodes,c("0","1"))
RocheCoherenceForBNs(scStage0BN,scStage0nodes,c("0","1"))
```






