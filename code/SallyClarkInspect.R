library(GGally)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(dplyr)
library(kableExtra)

scJoint <- readRDS(file = "tables/scJointTable.RDS")

scJoint

scJoint$Evaluation <- c(scJoint$Priors[1:4], scJoint$Posteriors[5:12])


ggcorr(scJoint[,c(3:9,11)], method = c("pairwise", "spearman"),
        digits = 4, low = "steelblue", mid = "white",
       high = "darkred", midpoint =.5,
       geom = "tile", label = TRUE, label_size=4, label_round =2, layout.exp =1,
       label_alpha = TRUE,hjust = 0.75)

png(file="../images/scSpearman.png", 
    units="in", 
    width=6, 
    height=6, 
    res=400)
ggcorr(scJoint[,c(3:9,11)], method = c("pairwise", "spearman"),
       digits = 4, low = "steelblue", mid = "white",
       high = "darkred", midpoint =.5,
       geom = "tile", label = TRUE, label_size=4, label_round =2, layout.exp =1,
       label_alpha = TRUE,hjust = 0.75)
dev.off()


library(corrplot)
library(ggcorrplot)


library(Hmisc)
cortest <- Hmisc::rcorr(as.matrix(scJoint[,c(3:9,11)]), type = "spearman")

str(cortest)




ggcorrplot(scJoint[,c(3:9,11)], lab = TRUE, type = "lower",
           p.mat = cortest$P)


p.mat <- cor_pmat(as.matrix(scJoint[,c(3:9,11)]), method = "spearman", ,exact=FALSE)


round(p.mat,3)[,1:6] %>% kable(format = "latex",booktabs=T,
                     linesep = "",  escape = FALSE, 
                     caption = "Head of the religion dataset.") %>%  
  kable_styling(latex_options=c("scale_down"))





#high coherence with various pr, low coherence, low pr
strPlot <- ggplot(scJoint)+geom_jitter(aes(y = Evaluation, x = Structured), size = 1, alpha = .5,
                                     width = 0.05, height = 0.05)+
  theme_tufte()+xlab("Coherence")+labs(color = "Coherence measure")+ggtitle("Structured")

strPlot

fitPlot <- ggplot(scJoint) +
  geom_jitter(aes(y = Evaluation, x = Fitelson), size = 1, alpha = .5,
              width = 0.05, height = 0.05) +
  theme_tufte()+xlab("Coherence")+ggtitle("Fitelson")

fitPlot

ogPlot <- ggplot(scJoint) +
  geom_jitter(aes(y = Evaluation, x = scJoint$`Olsson-Glass`), size = 1, alpha = .5,
              width = 0.05, height = 0.05)  +
  theme_tufte()+xlab("Coherence")+labs(color = "Coherence measure")+ggtitle("Olsson-Glass")

ogPlot

rochPlot <-ggplot(scJoint)+ geom_jitter(aes(y = Evaluation, x = Roche), size = 1, alpha = .5, 
                                      width = 0.05, height = 0.05)+
  theme_tufte()+xlab("Coherence")+labs(color = "Coherence measure")+ggtitle("Roche")

rochPlot


ogPlot

#wow, coherence high, prob tends to be low!
dmPlot <- ggplot(scJoint) +
  geom_jitter(aes(y = Evaluation, x = scJoint$`Douven-Meijs`), size = 1, alpha = .5, width = .15, height = 0.05) +
  theme_tufte()+xlab("Coherence")+labs(color = "Coherence measure")+ggtitle("Douven-Meijs")


dmPlot

shPlot <- ggplot(scJoint)+geom_jitter(aes(y = Evaluation, x = Shogenji), size = 1, alpha = .7, width = 1, height = 0.05)+
  theme_tufte()+xlab("Coherence")+labs(color = "Coherence measure")+ggtitle("Shogenji")

shPlot

grid.arrange(strPlot, fitPlot, ogPlot, rochPlot, dmPlot, shPlot, nrow = 3)

library(gridExtra)

png(file="../images/cohPlots.png", 
    units="in", 
    width=6, 
    height=6, 
    pointsize = 1,
    res=400)
grid.arrange(strPlot, fitPlot, ogPlot, rochPlot, dmPlot, shPlot, nrow = 3)

dev.off()









nrow(SCall)



