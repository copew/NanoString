#loading packages
library(ggplot2)
library(ggthemes)

###loading data
RIN_Conc <- read.csv("~/OneDrive - University Of Cambridge/Documents/PhD/Nanostring/RIN_Conc.csv", stringsAsFactor=FALSE)
View(RIN_Conc)

###converting them into numeric
#RIN_Conc$RIN <- as.factor(RIN_Conc$RIN)
RIN_Conc$RIN <- as.numeric(RIN_Conc$RIN)
summary(RIN_Conc)

##plot concentration

ggplot(RIN_Conc, aes(x=Concentration)) + 
  geom_histogram(binwidth=20)+ 
  theme_bw(base_size = 16)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20))+
  scale_y_continuous(name="Frequency")

ggplot(RIN_Conc, aes(x=RIN)) + 
  geom_histogram(binwidth=0.1)+ 
  theme_bw(base_size=16)+
  scale_y_continuous(name="Frequency")
