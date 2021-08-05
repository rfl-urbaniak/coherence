

#now look at correlations with probability

SCall <- rbind(SCnoEvidence,SCstageA,SCstageB)


SCall
colnames(SCall)

#high coherence with various pr, low coherence, low pr
strPlot <- ggplot(SCall)+geom_jitter(aes(y = Probability, x = Structured), size = 1, alpha = .5,
                                     width = 0.05, height = 0.05)+
  theme_tufte()+xlab("Coherence")+labs(color = "Coherence measure")+ggtitle("Structured")

strPlot

fitPlot <- ggplot(SCall) +
  geom_jitter(aes(y = Probability, x = Fitelson), size = 1, alpha = .5,
              width = 0.05, height = 0.05) +
  theme_tufte()+xlab("Coherence")+ggtitle("Fitelson")

fitPlot

ogPlot <- ggplot(SCall) +
  geom_jitter(aes(y = Probability, x = SCall$`Olsson-Glass`), size = 1, alpha = .5,
              width = 0.05, height = 0.05)  +
  theme_tufte()+xlab("Coherence")+labs(color = "Coherence measure")+ggtitle("Olsson-Glass")

ogPlot

rochPlot <-ggplot(SCall)+ geom_jitter(aes(y = Probability, x = Roche), size = 1, alpha = .5, 
                                      width = 0.05, height = 0.05)+
  theme_tufte()+xlab("Coherence")+labs(color = "Coherence measure")+ggtitle("Roche")

rochPlot


ogPlot

#wow, coherence high, prob tends to be low!
dmPlot <- ggplot(SCall) +
  geom_jitter(aes(y = Probability, x = SCall$`Douven-Meijs`), size = 1, alpha = .5, width = .15, height = 0.05) +
  theme_tufte()+xlab("Coherence")+labs(color = "Coherence measure")+ggtitle("Douven-Meijs")


dmPlot

shPlot <- ggplot(SCall)+geom_jitter(aes(y = Probability, x = Shogenji), size = 1, alpha = .7, width = 1, height = 0.05)+
  theme_tufte()+xlab("Coherence")+labs(color = "Coherence measure")+ggtitle("Shogenji")



shPlot

png(file="../images/cohPlots.png", 
    units="in", 
    width=6, 
    height=6, 
    pointsize = 1,
    res=400)
grid.arrange(strPlot, fitPlot, ogPlot, rochPlot, dmPlot, shPlot, nrow = 3)

dev.off()









nrow(SCall)



