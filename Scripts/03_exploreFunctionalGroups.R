# create site-level data of functional group biomass (Mariska's list)

rm(list=ls())
#library()


groupdat<-read.csv("~/Analyses/fish-stock/input/FunctionalGroupSpecies.csv")
load("~/Analyses/fish-stock/input/TMPwsd_REAthru2017-SpeciesLevel.RData") # by SPECIES
fishcodes<-read.csv("~/Analyses/fish-stock/input/LIST_OF_FISH_SPECIES 20161024.csv")


# BELOW NO LONGER NECESSARY - corrected original input file
# CHECK THAT scientific names in groupdat are all present in fishcodes (master species list)
# check groupdat$scientific.name against the "TAXONNAME" column instead of the "SCIENTIFIC_NAME" column
# not all of these are genus/species - some are just genus (e.g., Scarus sp)
nomatch<-groupdat[!(groupdat$SCIENTIFIC.NAME %in% fishcodes$TAXONNAME),]
nomatch
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
  groupsum<-rowSums(wsd[,colnums])
  wsd<-cbind(wsd, groupsum)
  names(wsd)[length(wsd)]<-levels(groupdat$Functional.group)[i]
}



# MAKE HISTOGRAMS of each of the functional group columns, including 2016 vs excluding 2016
# Probably won't make that much a difference - proportionally you should see about the same number of zeroes for a given species

wsd.mhi<-subset(wsd, OBS_YEAR>2011, OBS_YEAR<2016)






