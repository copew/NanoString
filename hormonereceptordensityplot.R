####er, pr and her2 expression across samples
#density plot

library(ggplot2)

dim(nanoStringIC)
colnames(nanoStringIC)
nanoStringIC[which(rownames(nanoStringIC)=="ESR1"), ]

er_nanostring <- data.frame(nanoStringIC[c(which(rownames(nanoStringIC)=="ESR1")), ])
colnames(er_nanostring) <- "expression"
#View(er_nanostring)
ggplot(er_nanostring, aes(x=log10(expression+0.5)))+
  geom_density(size=1)+
  xlab("Normalised Count (log scale)")+
  ylab("Frequency")+
  theme_bw(base_size=16)+
  ggtitle("Expression of ER")



er_mbexpression <- data.frame(mb_expression[c(which(rownames(mb_expression)=="ESR1")), ])


# rownames(er_mbexpression)
# plot(er_nanostring)
#   


##her2

GRB7_nanostring <- data.frame(nanoStringIC[c(which(rownames(nanoStringIC)=="GRB7")), ])
colnames(GRB7_nanostring) <- "expression"
#View(GRB7_nanostring)
ggplot(GRB7_nanostring, aes(x=log10(expression+0.5)))+
  geom_density(size=1)+
  xlab("Normalised Count (log scale)")+
  ylab("Frequency")+
  theme_bw(base_size=16)+
  ggtitle("Expression of GRB7")
