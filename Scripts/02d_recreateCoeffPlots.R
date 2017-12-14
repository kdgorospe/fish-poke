# RECREATE COEFFICIENT PLOTS
rm(list=ls())
library(ggplot2)


setwd("~/Analyses/fish-stock/input")
# Read-in sector coefficient estimates
coeff.dat<-read.csv("par_estimates_b_quantiles-TotalFish.csv")
#coeff.dat<-read.csv("par_estimates_b_quantiles-Herbivores.csv")


# Set up dataframe for graphing
B<-coeff.dat
names(B)[1]<-"nameColumn"
names(B)[2]<-"Low"
names(B)[4]<-"Mid"
names(B)[6]<-"High"
row.names(B)<-B$nameColumn
B<-subset(B, select=-nameColumn)

# Read-in island coefficient estimates
#coeff.IS<-read.csv("par_estimates_mu.bIS_quantiles-TotalFish.csv")
coeff.IS<-read.csv("par_estimates_mu.bIS_quantiles-Herbivores.csv")
mu.bIS<-coeff.IS
names(mu.bIS)[1]<-"nameColumn"
names(mu.bIS)[2]<-"Low"
names(mu.bIS)[4]<-"Mid"
names(mu.bIS)[6]<-"High"
row.names(mu.bIS)<-mu.bIS$nameColumn
mu.bIS<-subset(mu.bIS, select=-nameColumn)


drivers<-c("Intercept", "Coral", "Coral x Coral", "Sand", "CCA",
           "Depth", "Complexity", "Water Clarity", "SST", "Waves", "Waves x Waves", "Human Density")
nsector<-25
nisland<-7

# SPLIT UP B Matrix
setwd("~/Analyses/RESULTS/fish-stock")
for(i in 1:length(drivers))
{
  B.plot<-B[((i*nsector)-(nsector-1)):(i*nsector),] # Get b estimates
  row.names(B.plot)<-gsub(x=row.names(B.plot), pattern=".*\\[|\\]", replacement="") # pattern=".*\\[| .*\\]" Remove everything before the "[" and the final "]"
  row.names(B.plot)<-gsub(x=row.names(B.plot), pattern=paste(" ", drivers[i], sep=""), replacement="") # Remove the driver
  mu.bIS.plot<-mu.bIS[((i*nisland)-(nisland-1)):(i*nisland),] # Get island-level mu's
  row.names(mu.bIS.plot)<-gsub(x=row.names(mu.bIS.plot), pattern=".*\\[|\\]", replacement="") # pattern=".*\\[| .*\\]" Remove everything before the "[" and the final "]"
  row.names(mu.bIS.plot)<-gsub(x=row.names(mu.bIS.plot), pattern=paste(" ", drivers[i], sep=""), replacement="") # Remove the driver
  for(k in 1:dim(mu.bIS.plot)[1])
  {
    sec1.index<-grep(row.names(mu.bIS.plot)[k], row.names(B.plot))[1] # get the first sector for each island and create a column in mu.bIS.plot for it
    mu.bIS.plot$xSector1[k]<-row.names(B.plot)[sec1.index]
    sec2.all<-grep(row.names(mu.bIS.plot)[k], row.names(B.plot)) # get the last sector for each island and create a column in mu.bIS.plot for it
    sec2.index<-grep(row.names(mu.bIS.plot)[k], row.names(B.plot))[length(sec2.all)]
    mu.bIS.plot$xSector2[k]<-row.names(B.plot)[sec2.index]
  }
  p<-ggplot(B.plot, aes(x=row.names(B.plot), y=B.plot[,3])) +
    geom_segment(data=mu.bIS.plot, aes(x=mu.bIS.plot$xSector1[1], xend=mu.bIS.plot$xSector2[1], y=mu.bIS.plot$Mid[1],  yend=mu.bIS.plot$Mid[1]), linetype="solid", colour="gray70", lineend="butt", size=2)+
    geom_segment(data=mu.bIS.plot, aes(x=mu.bIS.plot$xSector1[2], xend=mu.bIS.plot$xSector2[2], y=mu.bIS.plot$Mid[2],  yend=mu.bIS.plot$Mid[2]), linetype="solid", colour="gray70", lineend="butt", size=2)+
    geom_segment(data=mu.bIS.plot, aes(x=mu.bIS.plot$xSector1[3], xend=mu.bIS.plot$xSector2[3], y=mu.bIS.plot$Mid[3],  yend=mu.bIS.plot$Mid[3]), linetype="solid", colour="gray70", lineend="butt", size=2)+
    geom_segment(data=mu.bIS.plot, aes(x=mu.bIS.plot$xSector1[4], xend=mu.bIS.plot$xSector2[4], y=mu.bIS.plot$Mid[4],  yend=mu.bIS.plot$Mid[4]), linetype="solid", colour="gray70", lineend="butt", size=2)+
    geom_segment(data=mu.bIS.plot, aes(x=mu.bIS.plot$xSector1[5], xend=mu.bIS.plot$xSector2[5], y=mu.bIS.plot$Mid[5],  yend=mu.bIS.plot$Mid[5]), linetype="solid", colour="gray70", lineend="butt", size=2)+
    geom_segment(data=mu.bIS.plot, aes(x=mu.bIS.plot$xSector1[6], xend=mu.bIS.plot$xSector2[6], y=mu.bIS.plot$Mid[6],  yend=mu.bIS.plot$Mid[6]), linetype="solid", colour="gray70", lineend="butt", size=2)+
    geom_segment(data=mu.bIS.plot, aes(x=mu.bIS.plot$xSector1[7], xend=mu.bIS.plot$xSector2[7], y=mu.bIS.plot$Mid[7],  yend=mu.bIS.plot$Mid[7]), linetype="solid", colour="gray70", lineend="butt", size=2)+
    geom_errorbar(aes(ymin=B.plot[,1], ymax=B.plot[,5]), size=0.6, width=0.3, colour="darkorange3") +
    geom_hline(aes(yintercept=0), linetype="dotted", colour="black") +
    labs(x="", title=drivers[i], y="coefficient") +
    theme_light()+
    theme(text = element_text(size=22), axis.text.y=element_text(colour="black"), axis.text.x=element_text(angle=90, hjust=1, vjust=1, colour="black", size=16))+
    geom_point(colour="darkorange4", size=1.8, shape=23, fill="darkorange3")
  
  
  new.file<-paste("model3a_par_estimates_b_", drivers[i],".pdf",sep="")
  pdf(file=new.file)
  print(p)
  dev.off()
}