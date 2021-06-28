
#R: reads
#A: advice
BooksDAG <- model2network("[R|A][A]")

#graphviz.plot(BooksDAG)

#say I advice you should read every tenth book you ask me about
Aprob <- priorCPT("A",prob1 = .01)

#you read every 10th you consider and ask me about, and my advice increases the prob to 0.15.
Rprob <- singleCPT(eNode = "R", hNode = "A", probEifHS1 = .15 , probEifHS2 =  .1)

BooksCPT <- list(A=Aprob, R= Rprob)

BooksBN <- custom.fit(BooksDAG, BooksCPT)
#BooksBN

#graphviz.chart(BooksBN, type="barprob")
 
