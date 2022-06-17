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


- scripts containing definitions of functions calculating various coherence measures are in the folder [measures](https://github.com/rfl-urbaniak/coherence/tree/master/code/measures).


### Recommended readings


Our code requires the use of `bnlearn` package, which can be read about in ["Bayesian Networks
With Examples in R"](https://www.routledge.com/Bayesian-Networks-With-Examples-in-R/Scutari-Denis/p/book/9781482225587) by Marco Scutari and  Jean-Baptiste Denis.




**Contents**
* TOC
{:toc}



### Set-up


First, you need to install the relevant R libraries. The crucial one is `bnlearn` [by Marco Scutari](https://www.bnlearn.com/about/) (who was also kind enough to include some functionalities when I inquired about them). This is a bit
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
```
