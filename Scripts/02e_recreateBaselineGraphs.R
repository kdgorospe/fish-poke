# RECREATE MINIMAL IMPACT BIOMASS GRAPHS
rm(list=ls())
library(ggplot2)

setwd("~/Analyses/fish-stock/input")
dat.bt<-read.csv("mappingData_minimalImpactEstimates_bt-Herbivores.csv")

## REORDER AXIS from highest to lowest natural (i.e., minimal impact) fish biomass levels 
sc.order<-dat.bt$SC[order(dat.bt$Mid, decreasing=FALSE)]
sc.order
sc.index<-order(dat.bt$Mid, decreasing=FALSE) 
# set order from lowest to highest (i.e., increasing bioamss levels) - later, coords_flip() function corrects this to decreasing biomass levels

# LEVELS should be sorted based on sc.index - GRAPHING order is based on order of levels
dat.bt$SC<-factor(dat.bt$SC, levels(dat.bt$SC)[sc.index])
levels(dat.bt$SC)

natlimits.bt <- aes(xmax = axis.order + 0.45, xmin = axis.order - 0.45, ymax = MinimalImpactUpper, ymin= MinimalImpactLower)

p<-ggplot(data=dat.bt, aes(x=SC, y=Mid)) + 
  geom_point(aes(x=SC, y=Mid), colour="white") +
  geom_rect(natlimits.bt, fill = "steelblue4", alpha=.6) +
  geom_errorbar(aes(ymin=PresentDayLower, ymax=PresentDayUpper), colour="black", size=1.1, width=0.5) +
  geom_point(aes(x=SC, y=PresentDay), shape=23, colour="black", fill="black") +
  geom_point(aes(x=SC, y=Mid), colour="white") +
  labs(y=expression(paste("Biomass (g",m^{-2}, ")")), x="") +  
  theme_light()+
  theme(text = element_text(size=18), axis.text.x=element_text(vjust=0.5, colour="black", size=18), 
        legend.position="none") +
  coord_flip()
setwd("~/Analyses/RESULTS/fish-stock")
pdf(file="sectorLevelModeledMinimalImpact_backtransBiomass_independentChains_avgQuantiles.pdf")
print(p)
dev.off()


# RECREATE ABSOLUTE RECOVERY GRAPHS:
rm(list=ls())
library(ggplot2)

setwd("~/Analyses/fish-stock/input")
dat<-read.csv("mappingData_recoveryEstimates-Herbivores.csv")

## REORDER AXIS from highest to lowest natural (i.e., minimal impact) fish biomass levels 
sc.order<-dat$SC[order(dat$Mid, decreasing=FALSE)]
sc.order
sc.index<-order(dat$Mid, decreasing=FALSE) 
# set order from lowest to highest (i.e., increasing bioamss levels) - later, coords_flip() function corrects this to decreasing biomass levels

# LEVELS should be sorted based on sc.index - GRAPHING order is based on order of levels
dat$SC<-factor(dat$SC, levels(dat$SC)[sc.index])
levels(dat$SC)

names(dat)[c(2,6)]<-c("UpperRP", "LowerRP")
names(dat)[4]<-"MidRP"

p<-ggplot(data=dat, aes(x=SC, y=MidRP)) + 
  geom_errorbar(aes(ymin=LowerRP, ymax=UpperRP), colour="slateblue", size=1.2, width=0.5) +
  geom_point(aes(x=SC, y=MidRP), size=2, colour="slateblue") +
  labs(y=expression(paste("Biomass (g",m^{-2}, ") Recovery Potential")), x="") +  
  theme_light()+
  theme(text = element_text(size=18), axis.text.x=element_text(vjust=0.5, colour="black", size=18), 
        legend.position="none") +
  coord_flip()
setwd("~/Analyses/RESULTS/fish-stock")
pdf(file="sectorLevelRecoveryPotential_independentChains_avgQuantiles.pdf")
print(p)
dev.off()


# RECREATE PERCENT RECOVERY GRAPHS:
rm(list=ls())
library(ggplot2)

setwd("~/Analyses/fish-stock/input")
dat<-read.csv("mappingData_percentRecoveryEstimates-Herbivores.csv")


## REORDER AXIS from highest to lowest natural (i.e., minimal impact) fish biomass levels 
sc.order<-dat$SC[order(dat$Mid, decreasing=FALSE)]
sc.order
sc.index<-order(dat$Mid, decreasing=FALSE) 
# set order from lowest to highest (i.e., increasing bioamss levels) - later, coords_flip() function corrects this to decreasing biomass levels

# LEVELS should be sorted based on sc.index - GRAPHING order is based on order of levels
dat$SC<-factor(dat$SC, levels(dat$SC)[sc.index])
levels(dat$SC)

names(dat)[c(2,6)]<-c("UpperPercent", "LowerPercent")
names(dat)[4]<-"MidPercent"


p<-ggplot(data=dat, aes(x=SC, y=MidPercent)) + 
  geom_errorbar(aes(ymin=LowerPercent, ymax=UpperPercent), colour="forestgreen", size=1.2, width=0.5) +
  geom_point(aes(x=SC, y=MidPercent), size=2, colour="forestgreen") +
  labs(y=expression(paste("Biomass (g",m^{-2}, ") Percent Recovery Potential")), x="") +  
  theme_light()+
  theme(text = element_text(size=18), axis.text.x=element_text(vjust=0.5, colour="black", size=18), 
        legend.position="none") +
  coord_flip()
setwd("~/Analyses/RESULTS/fish-stock")
pdf(file="sectorLevelPercentRecoveryPotential_independentChains_avgQuantiles.pdf")
print(p)
dev.off()