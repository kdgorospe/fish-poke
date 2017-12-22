# create site-level data of functional group biomass (Mariska's list)
rm(list=ls())
#library()


groupdat<-read.csv("~/Analyses/fish-stock/input/FunctionalGroupSpecies-UPDATED20171221.csv")
load("~/Analyses/fish-stock/input/TMPwsd_REAthru2017-SpeciesLevel.RData") # by SPECIES
fishcodes<-read.csv("~/Analyses/fish-stock/input/LIST_OF_FISH_SPECIES 20161024.csv")


# BELOW NO LONGER NECESSARY - corrected original input file
# CHECK THAT scientific names in groupdat are all present in fishcodes (master species list)
# check groupdat$scientific.name against the "TAXONNAME" column instead of the "SCIENTIFIC_NAME" column
# not all of these are genus/species - some are just genus (e.g., Scarus sp)
#nomatch<-groupdat[!(groupdat$SCIENTIFIC.NAME %in% fishcodes$TAXONNAME),]
#nomatch
#"Gymnothorax steindachneri" #mispelled
#"Kyphosus sp" #convention is to use "Genus sp"
#"Chlorurus sordidus" #still using OLD scientific designation
#"Gymnothorax rueppellii" #mispelled
#"Mobulidae" #old group name (just delete this - Ray family extremely zero-inflated)

for(i in 1:length(levels(groupdat$Functional.group))){
  nextgroup<-levels(groupdat$Functional.group)[i]
  groupspecies<-groupdat[groupdat$Functional.group==nextgroup,]$SCIENTIFIC.NAME
  # Next convert SPECIES NAMES to 4-letter CODES
  groupcodes<-fishcodes[match(groupspecies, fishcodes$TAXONNAME),]$SPECIES 
  # to get functional group biomass: (1) find species columns (2) sum across rows
  colnums<-match(groupcodes, names(wsd))
  colnums<-na.omit(colnums) #just means that the species (altho part of our master species list) is not part of our data (wsd)
  groupsum<-apply(wsd[,colnums], MARGIN=1, FUN=sum)
  wsd<-cbind(wsd, groupsum)
  names(wsd)[length(wsd)]<-levels(groupdat$Functional.group)[i]
}


wsd<-subset(wsd, OBS_YEAR>2011 & OBS_YEAR<2016 & REGION=="MHI") #limit to pre-global bleaching years (Q: does visibility estimation method change across bleaching years?)
#wsd<-subset(wsd, OBS_YEAR>2011 & REGION=="MHI") #include all MHI data since standardizing complexity measurements
save(wsd, file="TMPwsd_FunctionalGroupMHI.RData")


# MAKE HISTOGRAMS of each of the functional group columns, including 2016 vs excluding 2016
# Probably won't make that much a difference - proportionally you should see about the same number of zeroes for a given species
lastcol<-grep("TotFish", names(wsd))
zero.df<-NA
for(i in (lastcol+1):dim(wsd)[2]){
  fish<-names(wsd)[i]
  num.zero<-sum(wsd[,fish]==0)
  zero.text<-paste("Number of zeroes = ", num.zero, sep="")
  pct.zero<-round(num.zero/(dim(wsd)[1])*100, digits = 1)
  pct.text<-paste("(", pct.zero, "%)", sep="")
  title.text<-paste(zero.text, pct.text)
  zero.df.nextrow<-as.data.frame(cbind(fish, num.zero, pct.zero))
  zero.df<-rbind(zero.df, zero.df.nextrow)
  
  setwd("~/Analyses/_RESULTS/fish-stock")
  histfile<-paste("functional_histo_", fish, "_.pdf", sep="")
  pdf(file=histfile)
  hist(wsd[,fish],breaks=30,main=title.text, xlab=fish)
  dev.off()
  
  wsd[,fish][(wsd[,fish]==0)]<-0.0001
  logfile<-paste("functional_histo_", fish, "_ln.pdf", sep="")
  pdf(file=logfile)
  hist(log(wsd[,fish]),breaks=30, main="ln biomass", xlab=fish)
  dev.off()
}

zero.df<-zero.df[-1,]
write.csv(zero.df, "_functionalGroupZeroInflation.csv", quote=FALSE, row.names=FALSE)


###################################################################################
###################################################################################
###################################################################################
# REPEAT for consumer-level groups - ie, secondary, primary, planktivore, piscivore
load("~/Analyses/fish-stock/input/TMPwsd_MHI_AllHerbivoresThru2016.RData")
consumer.wsd<-wsd
consumer.study<-subset(consumer.wsd, OBS_YEAR>2011 & OBS_YEAR<2016 & REGION=="MHI")


lastcol<-grep("nReps", names(consumer.study))

zero.df<-NA
for(i in (lastcol+1):dim(consumer.study)[2]){
  fish<-names(consumer.study)[i]
  num.zero<-sum(consumer.study[,fish]==0)
  zero.text<-paste("Number of zeroes = ", num.zero, sep="")
  pct.zero<-round(num.zero/(dim(consumer.study)[1])*100, digits = 1)
  pct.text<-paste("(", pct.zero, "%)", sep="")
  title.text<-paste(zero.text, pct.text)
  zero.df.nextrow<-as.data.frame(cbind(fish, num.zero, pct.zero))
  zero.df<-rbind(zero.df, zero.df.nextrow)
  
  setwd("~/Analyses/_RESULTS/fish-stock")
  histfile<-paste("consumer_histo_", fish, "_.pdf", sep="")
  pdf(file=histfile)
  hist(consumer.study[,fish],breaks=30,main=title.text, xlab=fish)
  dev.off()
  
  consumer.study[,fish][(consumer.study[,fish]==0)]<-0.0001
  logfile<-paste("consumer_histo_", fish, "_ln.pdf", sep="")
  pdf(file=logfile)
  hist(log(consumer.study[,fish]),breaks=30, main="ln biomass", xlab=fish)
  dev.off()
}

zero.df<-zero.df[-1,]
write.csv(zero.df, "_consumerGroupZeroInflation.csv", quote=FALSE, row.names=FALSE)

