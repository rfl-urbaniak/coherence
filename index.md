---
title: " "
layout: page
toc: true
#title: Bayesian Networks for the Legal Probabilism SEP entry
output:
  md_document:
    variant: markdown_github
    preserve_yaml: true
---


### Brief description



### Recommended readings

Before we move on, if you're interested in BNs, here are three really good sources to get you started:

- ["Bayesian Networks and Probabilistic Inference in Forensic Science"](https://www.wiley.com/en-cg/Bayesian+Networks+for+Probabilistic+Inference+and+Decision+Analysis+in+Forensic+Science,+2nd+Edition-p-9780470979730) by Taroni, Aitken, Garbolino and Biedermann. This is clearly the one with forensic science in the focus.

- For the more mathematically minded, ["Learning Bayesian Networks"](https://www.amazon.com/Learning-Bayesian-Networks-Richard-Neapolitan/dp/0130125342) by Neapolitan is pretty awesome.

- For a very accessible and fascinating account of the multiplicity of applications of Bayesian Neworks, check our ["Risk Assessment and Decision Analysis with Bayesian Networks"](https://www.routledge.com/Risk-Assessment-and-Decision-Analysis-with-Bayesian-Networks/Fenton-Neil/p/book/9781138035119) by Fenton and Neil.



- They don't contain R code, though. For a great treatment on the use of `bnlearn` package, read ["Bayesian Networks
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
