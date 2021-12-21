source("calculations/RobberersFunction.R")
source("calculations/RobberersFunctionGeneral.R")


#For all 10 being P
VaryingCoherences <- list()
OnlyR <- 0
OnlyP <- seq(0,1,by=0.1)
for (P in OnlyP){
  Wynix <- list(RobbereresCoherences(OnlyP = P, OnlyR = OnlyR, Both = 1-P-OnlyR))
VaryingCoherences[[P*10+1]] <- Wynix
}

do.call(rbind, VaryingCoherences)
AllAreP <- as.data.frame(t(as.data.frame(VaryingCoherences)))
rownames(AllAreP)<-NULL
AllAreP <- cbind(OnlyP,AllAreP)
AllArePLong <- melt(AllAreP,id.vars="OnlyP")


ggplot(AllArePLong,aes(x=OnlyP,y=value,color=variable,lty=variable))+geom_line()+theme_minimal()

### Let's have 8 P, one OnlyR, and  OnlyP goes from 0 to 0.8.
VaryingCoherences <- list()
OnlyR <- 0.1
OnlyP <- seq(0,0.8,by=0.1)
for (P in OnlyP){
  Wynix <- list(RobbereresCoherences(OnlyP = P, OnlyR = OnlyR, Both = 1-P-OnlyR))
  VaryingCoherences[[P*10+1]] <- Wynix
}

do.call(rbind, VaryingCoherences)
EightPOneOnlyR <- as.data.frame(t(as.data.frame(VaryingCoherences)))
rownames(EightPOneOnlyR)<-NULL
EightPOneOnlyR <- cbind(OnlyP,EightPOneOnlyR)
EightPOneOnlyLong <- melt(EightPOneOnlyR,id.vars="OnlyP")

ggplot(EightPOneOnlyLong,aes(x=OnlyP,y=value,color=variable,lty=variable))+geom_line()+theme_minimal()




## Now imagine 1 OnlyR, 1-6 OnlyP, 2 are neither
VaryingCoherences <- list()
OnlyR <- 0.1
None <- 0.2
OnlyP <- seq(0.1,0.6,by=0.1)
for (P in OnlyP){
  Wynix <- list(RobbereresCoherencesGeneral(OnlyP = P, OnlyR = OnlyR, Both = 1 - P-OnlyR-None,None = None))
  VaryingCoherences[[P*10+1]] <- Wynix
}



VaryingCoherences <- VaryingCoherences[-1]

Usual <- as.data.frame(t(as.data.frame(VaryingCoherences)))

rownames(Usual)<-NULL
Usual <- cbind(OnlyP,Usual)
UsualLong <- melt(Usual,id.vars="OnlyP")

ggplot(UsualLong,aes(x=OnlyP,y=value,color=variable,lty=variable))+geom_line()+theme_minimal()

