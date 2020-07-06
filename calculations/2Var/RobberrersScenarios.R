source("utils/Coherences2Variables.R")

Coherences4Scenarios("P","R",
                     A=0.8, B=0.8, AAndB=0.6, AOrB=1,
                     NotBIfNotA=0, BIfA=0.75)
