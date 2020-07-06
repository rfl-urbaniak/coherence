CPkable0 <- function(bn,node){
  ref <-  paste(bn,'$',node,'[[4]]', sep="")
  
  eval(parse(text =  ref)) %>% kable(format = "latex",booktabs=T,
                                     col.names = c(node, "Pr"),
                                     linesep = "") %>%   kable_styling(latex_options=c("striped"))
}

#CPkable0("BirdBN","G")




CPkable1 <- function(bn, node){
  parent <- paste(bn,'$',node, '$', 'parents[1]', sep="")
  parent <- eval(parse(text =  parent))
  ref <-  paste(bn,'$',node,'[[4]]', sep="")
  table <- eval(parse(text =  ref)) %>% kable(format = "latex",booktabs=T,
                                              #col.names = c(node, "1", "0"),
                                              linesep = "") %>%   kable_styling(latex_options=c("striped"))
  eval(parse(text = paste('add_header_above(table, c(\"', node, '\",','\"', parent, '\"', '=2), line = FALSE )', sep="")))
}




CPkable2 <- function(bn, node){
    ref <-  paste(bn,'$',node,'[[4]]', sep="")
    parents <- paste(bn,'$',node, '$', 'parents', sep="")
    parents <- eval(parse(text =  parents))
    
table <- eval(parse(text =  ref)) %>% kable(format = "latex",booktabs=T,
                                            col.names = c(node, "", "", "Pr"), linesep = "") %>%   kable_styling(latex_options=c("striped"))


eval(parse(text = paste('add_header_above(table, c(\"\",', '\"', parents[1], '\" =1, \"', parents[2], '\"=1,', '\"\"),line = FALSE)', sep="")))
}




#_______
# This produces LaTeX tables for results

tableLaTeX <- function(table){
table %>% kable(format = "latex",booktabs=T,
                        #col.names = c(node, "1", "0"),
                        linesep = "",  escape = FALSE) %>%   kable_styling(latex_options=c("striped"))
}
















