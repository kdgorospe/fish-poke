# COMBINE mu.bRG plots
rm(list=ls())
library(ggplot2)

# READ-IN mu.bRG estimates for both Herbivore and Total Reef Fish Analyses

setwd("~/Analyses/fish-stock/input")
herb<-read.csv("par_estimates_mu.bRG_quantiles-Herbivores.csv")
tot<-read.csv("par_estimates_mu.bRG_quantiles-TotalFish.csv")
names(herb)[2]<-"Lower"
names(tot)[2]<-"Lower"
names(herb)[6]<-"Upper"
names(tot)[6]<-"Upper"
names(herb)[4]<-"Mean"
names(tot)[4]<-"Mean"

drivers<-c("Intercept", "Coral", "CoralxCoral", "Sand", "CCA",
           "Depth", "Complexity", "Visibility", "SST", "Waves", "WavesxWaves", "Human Density")

tot[,1]<-drivers
herb[,1]<-drivers
names(tot)[1]<-"drivers"
names(herb)[1]<-"drivers"

tot<-cbind(tot, "total reef fish") 
herb<-cbind(herb, "herbivores") 
names(tot)[7]<-"analysis"
names(herb)[7]<-"analysis"

alldat<-rbind(tot, herb)

p=ggplot(data=alldat, aes(x=alldat$drivers, y=alldat$Mean, fill=analysis)) + 
  geom_point(aes(x=alldat$drivers, y=alldat$Mean, colour=analysis), position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin=Lower, ymax=Upper, colour=analysis), width=0.2, position=position_dodge(width=0.5))+
  labs(x="", y="coefficient") +
  scale_color_manual(values=c("red", "blue")) +
  geom_hline(aes(yintercept=0), linetype="dotted", colour="black")+
  theme_light()+
  coord_flip()+
  theme(axis.text.y=element_text(colour="black"), 
        axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, colour="black", size=10), 
        legend.position="bottom", legend.title=element_blank())


setwd("~/Analyses/fish-stock/RESULTS")
pdf(file="mu.bRGestimates_COMBINED.pdf")
print(p)
dev.off()