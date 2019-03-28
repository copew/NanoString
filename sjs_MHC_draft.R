psn<-read.table("~/Documents/psnumber.txt",sep="\t",stringsAsFactors = F, strip.white = T,quote = "",header = T)


HLA1 <- read_csv("~/Documents/OneDrive - University Of Cambridge/Documents/PhD/Collaborations/MHC heterozygosity/HLA1.csv")
head(HLA1)
HLA1$diff=HLA1$LN-HLA1$Breast
HLA1[HLA1$diff<0,"stat"]<-"decrease"
HLA1[HLA1$diff>0,"stat"]<-"increase"
HLA1[HLA1$diff==0,"stat"]<-"no change"
HLA1<-HLA1[,!colnames(HLA1)%in%c("diff")]
HLA1<-merge(HLA1,psn,by.x="ID",by.y="Surgery.path..No.",all.x=T)

library(ggplot2)
library(reshape2)
HLA=melt(HLA1,id.vars = c("ID","stat","Study.No.","bx.path.no."))
head(HLA)
ggplot(HLA,aes(x=variable,y=value,group=ID))+
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_grid(~stat)

head(HLA)

ggplot(HLA[HLA$stat=="decrease",],aes(x=variable,y=value))+
  geom_boxplot()+
  theme_minimal()

wilcox.test(HLA[HLA$stat=="decrease"& HLA$variable=="Breast","value"],
            HLA[HLA$stat=="decrease"& HLA$variable=="LN","value"]
            )

