scJointNew <- readRDS("tables/SCjointNew.RDS")


scJointNew

ncol(scJointNew)

measure <- list()
result1 <- as.logical(rep(6, FALSE))
for (i in 1:6){
measure[[i]] <- scJointNew[,i+2]
##S0: 00 > 01 , 00 > 10, 11>01, 11>10
result1[i] <- as.logical((measure[[i]][1] > measure[[i]][3]) & (measure[[i]][1] > measure[[i]][4]) & 
      (measure[[i]][2]>measure[[i]][3]) & (measure[[i]][2] > measure[[i]][4]) &
  (measure[[i]][1+4] > measure[[i]][3+4]) & (measure[[i]][1+4] > measure[[i]][4+4]) & 
  (measure[[i]][2+4]>measure[[i]][3+4]) & (measure[[i]][2+4] > measure[[i]][4+4]) &
  (measure[[i]][1+8] > measure[[i]][3+8]) & (measure[[i]][1+8] > measure[[i]][4+8]) & 
  (measure[[i]][2+8]>measure[[i]][3+8]) & (measure[[i]][2+8] > measure[[i]][4+8])) 
}

result1


# 11 stage 1 > 11 stage 0
measure <- list()
result2 <- as.logical(rep(6, FALSE))
for (i in 1:6){
measure[[i]] <- scJointNew[,i+2]
result2[i] <- as.logical((measure[[i]][6] > measure[[i]][2]))
}


result2


# 00 stage 1 < 00 stage 0
measure <- list()
result3 <- as.logical(rep(6, FALSE))
for (i in 1:6){
  measure[[i]] <- scJointNew[,i+2]
  result3[i] <- as.logical((measure[[i]][1] > measure[[i]][5]))
}


result3


# 10 stage 2 < 01 stage 2
measure <- list()
result4 <- as.logical(rep(6, FALSE))
for (i in 1:6){
  measure[[i]] <- scJointNew[,i+2]
  result4[i] <- as.logical((measure[[i]][12] < measure[[i]][11]))
}


result4

# 00 stage 2 > 00 stage 1
measure <- list()
result5 <- as.logical(rep(6, FALSE))
for (i in 1:6){
  measure[[i]] <- scJointNew[,i+2]
  result5[i] <- as.logical((measure[[i]][5] < measure[[i]][9]))
}

result5



# 11 stage 2 < 11 stage 1
measure <- list()
result6 <- as.logical(rep(6, FALSE))
for (i in 1:6){
  measure[[i]] <- scJointNew[,i+2]
  result6[i] <- as.logical((measure[[i]][10] < measure[[i]][6]))
}


result6





truths <- rbind(result1,result2,result3,result4,result5,result6)

colMeans(truths)


truths

colnames(scJointNew)
colnames(truths) <- colnames(scJointNew)[3:8]

rownames(truths) <- c("11 and 00 > 10 and 01","11 Stage 1 > 11 Stage 0",
                      "00 Stage 1 < 00 Stage 0", "10 Stage 2 < 01 Stage 2",
                      "00 Stage 2 > 00 Stage 1", "11 Stage 2 < 11 Stage 1"
)

library(kableExtra)

truths %>%  kable(format = "latex",booktabs=T,
                  linesep = "",  escape = FALSE, 
                  caption = "Satisfaction of intuitions about the Sally Clark problem.") %>%  
  kable_styling(latex_options=c("scale_down"))















