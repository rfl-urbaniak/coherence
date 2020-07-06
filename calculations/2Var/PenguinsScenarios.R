source("utils/Coherences2Variables.R")

# Case probabilities
G <- 0.5
B <- 0.5
P <- 0.01
GAndB <- 0.01
PAndB <- 0.01
PAndG <- 0.01
GOrB <- 0.99
POrB <- 0.5
POrG <- 0.5
BIfG <- 0.02
NotBIfNotG <- 0.01/0.51
NotBIfNotP <- 0.5/0.99
NotGIfNotP <- 0.5/0.99
BIfP <- 1
GIfP <- 1

Coherences4Scenarios("G", "B", G, B, GAndB, GOrB, NotBIfNotG, BIfG)

Coherences4Scenarios("P", "B", P, B, PAndB, POrB, NotBIfNotP, BIfP)

Coherences4Scenarios("P", "G", P, G, PAndG, POrG, NotGIfNotP, GIfP)
