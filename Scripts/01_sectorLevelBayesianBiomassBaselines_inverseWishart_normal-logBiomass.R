rm(list=ls())
library(reshape) # needed for melt()
library(rjags) 
library(xtable) 
library(coda)
library(gdata)    # needed for drop.levels()
library(car)      # needed for Variance Inflation Factor calculation
library(ggplot2)
library(corrplot)

##### CHANGE RESPONSE VARIABLE AS NEEDED: "PRIMARY" VS "INSTTotFishMinusSJRTB"

setwd("~/Analyses/fish-stock")
#load("~/Analyses/fish-stock/input/TMPwsd_MHI_AllHerbivoresThru2016.RData") # by CONSUMER GROUPS
#load("~/Analyses/fish-stock/input/TMPwsd_REAthru2017-SpeciesLevel.RData") # by SPECIES
load("~/Analyses/fish-stock/input/TMPwsd_FunctionalGroupMHI-REAthru2015.RData") # by FUNCTIONAL GROUP
mhi<-wsd


#### GIVE "mhi" fish data to TOMOKO join with OCEANOGRAPHY data in ArcMAP

### FILES below are result of joining dat above with Jamie's latest oceanography data by averaging three nearest neighbors to fish sites
### For now, using Jamie's latest updated oceanography metrics (summarized thru 31 Dec 2014 - except waves which is Dec 2013)
chl.dat<-read.csv("~/Analyses/fish-stock/input/oceanInput/ChlA_fishDat_Join.csv")
irr.dat<-read.csv("~/Analyses/fish-stock/input/oceanInput/PAR_fishDat_Join.csv")
sst.dat<-read.csv("~/Analyses/fish-stock/input/oceanInput/SST_fishDat_Join.csv")
wav.dat<-read.csv("~/Analyses/fish-stock/input/oceanInput/Wave_fishDat_Join.csv")

chl.dat<-subset(chl.dat, select=-POINT_FREQUENCY)
irr.dat<-subset(irr.dat, select=-POINT_FREQUENCY)
sst.dat<-subset(sst.dat, select=-POINT_FREQUENCY)
wav.dat<-subset(wav.dat, select=-POINT_FREQUENCY)


# Merge all ocean dat together 
ocean.common<-names(chl.dat)[1:75] #### NOTE: COLUMN NAMES ARE HARD-CODED; NEED TO REVISIT THIS IF NEW DATAFILES ARE EVER USED
ocean.dat<-merge(chl.dat, irr.dat, by=ocean.common, all=TRUE)
ocean.dat<-merge(ocean.dat, sst.dat, by=ocean.common, all=TRUE)
ocean.dat<-merge(ocean.dat, wav.dat, by=ocean.common, all=TRUE)


### All data including data from post-bleaching years
table(mhi$OBS_YEAR, mhi$REGION)
#MHI NWHI PRIAs SAMOA S.MARIAN N.MARIAN
#2007   0  117     0     0        0        0
#2008   0    0    96   112        0        0
#2009   0  181    29     0       82       95
#2010 184  118   179   241        0        0
#2011   0  141    30     0      219      135
#2012 163   91   231   223        0        0
#2013 287    0     0     0        0        0
#2014   0   89    45     0      210      148
#2015 294   96   291   338        0        0
#2016 256  182    30   185        0        0
#2017   0    0    81     0      172      159

table(ocean.dat$OBS_YEAR, ocean.dat$REGION)
#MHI
#2012 163
#2013 287
#2015 294
#2016 256

# i.e. REA data (mhi) and oceanography data (ocean.dat) have the same # of sites - merge should be perfect

### MERGE with fish data - simplify the merge by removing most common columns
ocean.dat<-subset(ocean.dat, select=-c(LATITUDE, LONGITUDE, DATE_, ANALYSIS_SEC, ANALYSIS_STRATA, ANALYSIS_YEAR, SEC_NAME,
                                     METHOD, OBS_YEAR, REGION, REGION_NAME, ISLAND, REEF_ZONE, DEPTH_BIN, HABITAT_CODE,
                                     DEPTH, HARD_CORAL, MA, CCA, SAND, OTHER_BENTHIC, MEAN_SH, SD_SH_DIFF, MAX_HEIGHT,
                                     VISIBILITY, nCounts, nReps))

### All data including post-bleaching 2016 data
common<-c("SITEVISITID", "SITE")
alldat<-merge(mhi, ocean.dat, by=common)
### Only the study data - 2012 thru 2015
study.dat<-subset(alldat, OBS_YEAR<2016 & OBS_YEAR>2011)


# Join with Humans and Site Data
siteDat<-read.csv("~/Analyses/fish-stock/input/SITE MASTER2016.csv")

#W Again - simplify the merge by removing most common columns
#Slight differences in the object class for "DATE_" and number of decimal numbers for "LAT" and "LONG"
sub.dat<-subset(study.dat, select=-c(DATE_, LATITUDE, LONGITUDE, ANALYSIS_YEAR, SEC_NAME))
#common<-names(siteDat)[names(siteDat) %in% names(study.dat)]
common<-c("SITE", "SITEVISITID", "REGION", "ISLAND", "REEF_ZONE", "OBS_YEAR", "DEPTH_BIN")
dat<-merge(sub.dat, siteDat, by=common)



### MERGE with benthic image analysis data (CPCe and CNET)
# Read in benthic image analysis (CPCe and CNET)
benthic.cnet<-read.csv(file="~/Analyses/fish-stock/input/Kelvin MHI BenthicImageAnalysis_CNET.csv", header=TRUE)
benthic.cpce<-read.csv(file="~/Analyses/fish-stock/input/Kelvin MHI BenthicImageAnalysis_CPCE.csv", header=TRUE)

## Aggregate morphology columns in CPCE data
# Encrusting corals:
enc.cols<-benthic.cpce[,grep("_ENC", names(benthic.cpce))]
# Sum and create new column that is identical to the column in benthic.cnet
benthic.cpce$CORAL_Encrusting.hard.coral<-apply(enc.cols, MARGIN=1, FUN=sum)

# Massive corals:
mass.cols<-benthic.cpce[,grep("_MASS", names(benthic.cpce))]
# Sum and create new column that is identical to the column in benthic.cnet
benthic.cpce$CORAL_Massive.hard.coral<-apply(mass.cols, MARGIN=1, FUN=sum)

# Branching corals:
bran.cols<-benthic.cpce[,grep("_BR", names(benthic.cpce))]
# Sum and create new column that is identical to the column in benthic.cnet
benthic.cpce$CORAL_Branching.hard.coral<-apply(bran.cols, MARGIN=1, FUN=sum)

common.cats<-names(benthic.cnet)[names(benthic.cnet) %in% names(benthic.cpce)]
benthic.cnet.common<-subset(x=benthic.cnet, select=common.cats)
benthic.cpce.common<-subset(x=benthic.cpce, select=common.cats)
benthic<-rbind(benthic.cnet.common, benthic.cpce.common)

#Again, when merging, don't merge by date, latitude, or longitude. 

common2<-names(benthic)[names(benthic) %in% names(dat)]
common<-common[!(common %in% c("DATE_", "LATITUDE", "LONGITUDE"))]
benthic<-subset(benthic, select=-c(DATE_, LATITUDE, LONGITUDE))
common2<-c("SITE", "REGION", "ISLAND", "REEF_ZONE", "OBS_YEAR", "DEPTH_BIN")
dat<-merge(dat, benthic, by=common2)
write.csv(dat, file="~/Analyses/_RESULTS/fish-stock/allDat_2012-2015_JOIN.csv", quote=FALSE)
### DAT drops from 744 rows to 730 rows (which sites are missing??)

# Sites that drop out:
#mhi$SITE[!mhi$SITE %in% dat$SITE]
#[1] LAN-00202 MAI-00472 MAI-00470 MOL-00482 KAU-00253 OAH-00432 OAH-00485
#[8] OAH-00444 OAH-00421 OAH-00428 OAH-00404 OAH-00385 OAH-00391 NII-01338
# ALL HAVE MISSING BENTHIC IMAGE ANALYSIS DATA (COULD POTENTIALLY BRING IN VISUAL ESTIMATED DATA, BUT THIS WOULD BE INCONSISTENT - HAVE ENOUGH SITES, OK WITH DROPPING THESE)


## ADD HABITAT CATEGORY ID VARIABLES
# ADD ID VARIABLE FOR REEF vs NONREEF
#reef.hab<-(dat$HABITAT_CODE=="AGR" | dat$HABITAT_CODE=="APR" | dat$HABITAT_CODE=="APS")
reef.hab<-(dat$HABITAT_CODE=="AGR")
dat$REEF<-as.numeric(reef.hab)

# ADD ID VARIABLE FOR PAVEMENT vs NON-PAVEMENT
#pave.hab<-(dat$HABITAT_CODE=="PAV" | dat$HABITAT_CODE=="PPR" | dat$HABITAT_CODE=="PSC" | dat$HABITAT_CODE=="SCR" | dat$HABITAT_CODE=="RRB")
pave.hab<-(dat$HABITAT_CODE=="PAV")
dat$PAV<-as.numeric(pave.hab)

# ADD ID VARIABLE FOR ROB vs NONROB
rob.hab<-dat$HABITAT_CODE=="ROB" 
dat$ROB<-as.numeric(rob.hab)

# ADD ID VARIABLE FOR APR vs NONAPR
apr.hab<-dat$HABITAT_CODE=="APR" 
dat$APR<-as.numeric(apr.hab)

# ADD ID VARIABLE FOR APS vs NONAPS
aps.hab<-dat$HABITAT_CODE=="APS" 
dat$APS<-as.numeric(aps.hab)

# ADD ID VARIABLE FOR MIX vs NONMIX
mix.hab<-dat$HABITAT_CODE=="MIX" 
dat$MIX<-as.numeric(mix.hab)

# ADD ID VARIABLE FOR PPR vs PPR
ppr.hab<-dat$HABITAT_CODE=="PPR" 
dat$PPR<-as.numeric(ppr.hab)

# ADD ID VARIABLE FOR PSC vs NONPSC
psc.hab<-dat$HABITAT_CODE=="PSC" 
dat$PSC<-as.numeric(psc.hab)

# ADD ID VARIABLE FOR RRB vs NONRRB
rrb.hab<-dat$HABITAT_CODE=="RRB" 
dat$RRB<-as.numeric(rrb.hab)

# ADD ID VARIABLE FOR SAG vs NONSAG
sag.hab<-dat$HABITAT_CODE=="SAG" 
dat$SAG<-as.numeric(sag.hab)

# ADD ID VARIABLE FOR SCR vs NONSCR
scr.hab<-dat$HABITAT_CODE=="SCR" 
dat$SCR<-as.numeric(scr.hab)
    
# WILL HAVE TO DROP LEVELS FOR OTHER hierarchical levels (e.g., REGION) if analysis expands to that
dat$ISLAND<-drop.levels(dat$ISLAND)
dat$SEC_NAME<-drop.levels(dat$SEC_NAME)
    

################################################
### Select which SPECIES/GROUP on which to focus:

# TOTAL REEF FISH
#fish<-"INSTTotFishMinusSJRTB" # FULL DATASET - 730 sites

# CONSUMER GROUPS
#fish<-"PRIMARY" # 4 zeroes
#fish<-"SECONDARY" # no zeroes
#fish<-"PLANKTIVORE" # 41 zeroes
#fish<-"PISCIVORE" # 246 zeroes

# FUNCTIONAL GROUPS
fish<-"Fish Invertivores"
  
  
  
  
######################################### Select response column
#indicator <- "Site-level Average Length"
indicator <- "Site-level Biomass"
focus <- paste("log",fish,sep="")
#focus <- paste("cuberoot_log",fish,sep="")
response <- paste("log (", fish, " biomass)", sep="")

########################################## Histograms
num.zero<-sum(dat[,fish]==0)
zero.text<-paste("Number of zeroes = ", num.zero, sep="")


setwd("~/Analyses/_RESULTS/fish-stock")
histfile<-paste("histo_", fish, "_.pdf", sep="")
pdf(file=histfile)
hist(dat[,fish],breaks=30,main=zero.text, xlab=indicator)
dev.off()
#hist(dat[dat[,fish]>0, fish])

histfile<-paste("histo_", fish, "_ln.pdf", sep="")
pdf(file=histfile)
hist(log(dat[,fish]),breaks=20, main="ln biomass", xlab=indicator)
dev.off()
# Creates a more normal distribution



# No longer exploring zero-inflation model
#hist(log(dat[dat[,fish]>0,fish]))
hist(dat$HARD_CORAL,breaks=10, xlab="Hard Coral")


hist(dat$HUMANS20,breaks=10)
hist(log(dat$HUMANS20),breaks=10)
#hist(dat$HUMANS20^2,breaks=10)

#hist(log(dat$HUMANS20^2),breaks=10)
hist(dat$HUMANS200,breaks=10, xlab="Humans 200")
hist(log(dat$HUMANS200),breaks=10, xlab="Humans 200")
#dat$HUMANS200<-log(dat$HUMANS200)

##########################################TRANSFORMATIONS
# Log Fish: Could not get non-logged data to converge

### FIRST, FOR HERBIVORES ANALYSIS (i.e., fish<-"PRIMARY"), there are four sites with zeroes, remove these
### OTHERWISE they create NA terms (symbolized by "-Inf")
#dat<-dat[dat[,fish]>0,]


### FOR PLANKTIVORE ANALYSIS with more zero-inflation, replace zeroes with small number
if(fish="PLANKTIVORE")
{
dat[,fish][(dat[,fish]==0)]<-0.0001
hist(dat[,fish],breaks=30,main=fish, xlab=indicator)

# now log
dat <- cbind(dat, log(dat[,fish])) # No longer need + 1 since this isn't a zero-inflated model
names(dat)[grep("fish", names(dat))]<-paste("log",fish,sep="")
plankfile<-paste("histo_", fish, "_ln.pdf", sep="")
pdf(file=plankfile) # ie - REPLACE ln(biomass) graph outputed above which automatically removed undefined zeroes
hist(dat[,(paste("log",fish,sep=""))],breaks=30,main=fish, xlab=indicator)
dev.off()
}


#biomass<-grep(paste("log",fish,sep=""), names(dat))
#dat <- cbind(dat, dat[,biomass]^(1/3)) # Create a column for the cuberoot of fish biomass
#names(dat)[dim(dat)[2]]<-paste("cuberoot_log",fish,sep="")

# Need to "log" humans, right skewed
# Log Humans
dat<-cbind(dat, log(dat$HUMANS20))
names(dat)[which(names(dat)=="log(dat$HUMANS20)")]<-"logHUMANS20"

dat<-cbind(dat, log(dat$HUMANS200))
names(dat)[which(names(dat)=="log(dat$HUMANS200)")]<-"logHUMANS200"

# Include coral + coral^2 as predictors
# For DIVER VISUAL ESTIMATES:
dat<-cbind(dat, (dat$HARD_CORAL)^2) 
names(dat)[which(names(dat)=="(dat$HARD_CORAL)^2")]<-"HARD_CORALsq"

# For BENTHIC IMAGE ANALYSIS NUMBERS:
dat<-cbind(dat, (dat$Total.Coral)^2)
names(dat)[which(names(dat)=="(dat$Total.Coral)^2")]<-"Total.sqCoral"

dat<-cbind(dat, (dat$CORAL_Branching.hard.coral)^2)
names(dat)[which(names(dat)=="(dat$CORAL_Branching.hard.coral)^2")]<-"CORALsq_Branching.hard.coral"

dat<-cbind(dat, (dat$CORAL_Encrusting.hard.coral)^2)
names(dat)[which(names(dat)=="(dat$CORAL_Encrusting.hard.coral)^2")]<-"CORALsq_Encrusting.hard.coral"

dat<-cbind(dat, (dat$CORAL_Massive.hard.coral)^2)
names(dat)[which(names(dat)=="(dat$CORAL_Massive.hard.coral)^2")]<-"CORALsq_Massive.hard.coral"

dat<-cbind(dat, (dat$HUMANS20)^2)
names(dat)[which(names(dat)=="(dat$HUMANS20)^2")]<-"HUMANS20sq"

dat<-cbind(dat, (dat$DEPTH)^2)
names(dat)[which(names(dat)=="(dat$DEPTH)^2")]<-"DEPTHsq"

dat<-cbind(dat, (dat$VISIBILITY)^2)
names(dat)[which(names(dat)=="(dat$VISIBILITY)^2")]<-"VISIBILITYsq"

dat<-cbind(dat, (dat$logHUMANS20)^2)
names(dat)[which(names(dat)=="(dat$logHUMANS20)^2")]<-"logHUMANS20sq"

dat<-cbind(dat, (dat$MEAN_Waves_MEAN)^2)
names(dat)[which(names(dat)=="(dat$MEAN_Waves_MEAN)^2")]<-"meanWAVESsq"

################################################################################
################################################################################
################################################################################
setwd("~/Analyses/_RESULTS/fish-stock")

## USE covars LIST BELOW TO: (1) CREATE SCATTERPLOTS between RESPONSE and each COVAR
## (2) Test for colinearity

covars<-c("VISIBILITY", "DEPTH", "MEAN_SH", "SD_SH_DIFF", "MAX_HEIGHT", 
          "logHUMANS20", "logHUMANS200",
          "Total.Coral", "Total.sqCoral",
          "MA_Macroalga", "SED_Sediment", "TURF_Turf.Alga", "CCA_Coralline.Alga", 
          "MEAN_Chla_MEAN", "MEAN_Chla_SD", "MEAN_Chla_CLIM_MAX", "MEAN_Chla_ANOM_MAX", "MEAN_Chla_ANOM_FREQ", 
          "MEAN_Irradiance_MEAN", "MEAN_Irradiance_SD", "MEAN_Irradiance_CLIM_MAX", "MEAN_Irradiance_ANOM_MAX", "MEAN_Irradiance_ANOM_FREQ", 
          "MEAN_SST_MEAN", "MEAN_SST_SD", "MEAN_SST_CLIM_MAX", "MEAN_SST_ANOM_MAX", "MEAN_SST_ANOM_FREQ", 
          "MEAN_Waves_MEAN", "meanWAVESsq", "MEAN_Waves_SD", "MEAN_Waves_CLIM_MAX", "MEAN_Waves_ANOM_MAX", "MEAN_Waves_ANOM_FREQ"
          ) 

dat.covars<-subset(dat, select=covars)
dat.covars<-na.omit(dat.covars)


# PAIRWISE SCATTERPLOTS:
#setwd("~/Analyses/_RESULTS/fish-stock")
#pdf(file="01Figure_correlationpairs.pdf")
#pairs(dat.covars)
#dev.off()

# CALCULATE ALL PAIRWISE CORRELATIONS

# Pearson's for all continuous vars
cor.covars<-cor(dat.covars)
write.csv(cor.covars, file="02aFigure_Cor.covars_0.5.csv")
cor.covars.test<-abs(cor.covars)>0.5
write.csv(cor.covars.test, file="02bFigure_Cor.covars_0.5.test.csv")

# Spearman's for all ranked, ordered vars (e.g., DACOR data)
cor.spear<-cor(dat.covars, method="spearman")
write.csv(cor.spear, file="02cFigure_Cor.spear_0.5.csv")
cor.spear.test<-abs(cor.spear)>0.5
write.csv(cor.spear.test, file="02dFigure_Cor.spear_0.5.test.csv")

setwd("~/Analyses/_RESULTS/fish-stock")
pdf(file="02eFigure_correlationVisual.pdf")
corrplot(cor.covars, insig="p-value")
dev.off()

setwd("~/Analyses/_RESULTS/fish-stock")
pdf(file="02fFigure_correlationSpearVisual.pdf")
corrplot(cor.spear, insig="p-value")
dev.off()

########################################## SCATTERPLOTS
#### CREATE SCATTERPLOTS OF ALL COVARIATES (decide later which ones to include in analysis)

# Select all site ID and hierarchical ID information
xa <- c("LATITUDE", "LONGITUDE", "SITE", "SITEVISITID", "REGION", "ISLAND", "REEF_ZONE", "HABITAT_CODE", "OBS_YEAR", "DEPTH_BIN", "SEC_NAME", "METHOD")

dat.scatter<-cbind(dat[xa], dat[focus], dat[covars])
dat.scatter<-na.omit(dat.scatter)

y <- dat.scatter[[focus]]
## Use double brackets to ensure that y is returned as a vector (not a data frame column)

scatter.final<-as.data.frame(cbind(y, dat.scatter[covars], dat.scatter$ISLAND))
names(scatter.final)[length(scatter.final)]<-"ISLAND"

setwd("~/Analyses/_RESULTS/fish-stock")

# COMMENT OUT scatterplot outputs since these are the same for all repeat analyses
for(i in 1:dim(scatter.final[covars])[2])
{
  newfile=paste("scatterplot_", colnames(dat.scatter[covars])[i], "_v_", focus, ".pdf", sep="")
  pdf(file=newfile)
  print(qplot(data=scatter.final, x=get(covars[i]), y=y, colour=ISLAND, xlab=covars[i], ylab=focus))      
  dev.off()
}




## BRUTE FORCE - calculate VIF for all non-correlated variables
xi <- c("Total.Coral", "Total.sqCoral", 
        "MA_Macroalga", "SED_Sediment", "TURF_Turf.Alga", "CCA_Coralline.Alga",
        "DEPTH", "MEAN_SH", "VISIBILITY",
        "MEAN_Chla_MEAN", "MEAN_Chla_ANOM_FREQ",
        "MEAN_Irradiance_MEAN", "MEAN_Irradiance_ANOM_MAX",
        "MEAN_SST_SD", "MEAN_SST_MEAN", "MEAN_SST_ANOM_FREQ", 
        "MEAN_Waves_MEAN", "meanWAVESsq", "MEAN_Waves_ANOM_FREQ",
        "logHUMANS20"
        )
################################################################################ 

# Select all site ID and hierarchical ID information
xa <- c("LATITUDE", "LONGITUDE", "SITE", "SITEVISITID", "REGION", "ISLAND", "REEF_ZONE", "HABITAT_CODE", "OBS_YEAR", "DEPTH_BIN", "SEC_NAME", "METHOD", "DATE_")
#xa <- c("LATITUDE", "LONGITUDE", "SITE", "SITEVISITID", "REGION", "ISLAND", "REEF_ZONE", "OBS_YEAR", "DEPTH_BIN", "SEC_NAME", "ANALYSIS_STRATA", "ANALYSIS_SEC", "METHOD")


### REMOVE ALL NA Data
dat.final<-cbind(dat[xa], dat[focus], dat[xi])
dat.final<-na.omit(dat.final)

######################## WHAT should the minimum sample size be within each sector?
minN <- 2 # for now, use 2 as a minimum (i.e., keep all samples except MAI_HANA and MAI_MOLOKINI which each have N=1);
# next lowest is MAI_NW=5 and OAH_KAENA=9

N.check<-as.data.frame(table(dat.final$SEC_NAME))
limitedSector<-N.check[(N.check[,2] < minN),][1] # Grab the sector name of all sectors that do not meet the minimum N requirement

# DROP all data points that fail the minN requirement
# For now, only drop MAI_MOLOKINI (n=1) and MAI_HANA (n=1); Keep MAI_NW (n=5)
# NOTE: Below is hard-coded - need to adjust if you want to drop other sectors
testSector <- dat.final$SEC_NAME != limitedSector[1,1] # DROP MAI_HANA
dat.final<-dat.final[testSector,]
testSector.2 <- dat.final$SEC_NAME != limitedSector[2,1] # DROP MAI_MOLOKINI
dat.final<-dat.final[testSector.2,]
#testSector.3 <- dat.final$SEC_NAME != limitedSector[3,1] 
#dat.final<-dat.final[testSector.3,]
#testSector.4 <- dat.final$SEC_NAME != limitedSector[4,1] 
#dat.final<-dat.final[testSector.4,]

# DROP LEVELS
dat.final$SEC_NAME<-drop.levels(dat.final$SEC_NAME)
dat.final$DATE_<-drop.levels(dat.final$DATE_)

# re-specify response ("y") vector (now that sites have been drop)
y <- dat.final[[focus]]

######## CREATE INTERACTIONS BETWEEN ALL VARIABLES 
interact.dat<-data.frame(y, dat.final[xi])
form <- y ~ .^2
mat.dat<-model.matrix(form, data=interact.dat)

######### option 1: BRUTE FORCE - include all interactions in the analysis
#mat.nointercept<-mat.dat[,-1] # remove the intercept for now

######### option 2: include all solo covariates + all interactions with ONE specific variable
# SELECT a FOCAL COVARIATE and all interactions with this covariate
#focal<-"DEPTH" # TOTAL.CORAL and Total.sqCoral together???
#cols.interact<-grep(focal, colnames(mat.dat))
#cols.interact<-cols.interact[-1] # Remove the first column as this is the SOLO covariate
#mat.nointercept<-mat.dat[,c(2:(length(xi)+1),cols.interact)] # Select all solo covars + all interactions with the FOCAL covariate
# If you need to remove HABITAT:HABITAT INTERACTIONS
#mat.nointercept<-mat.nointercept[,1:(length(colnames(mat.nointercept))-3)]

######## option 3: INCLUDE A SPECIFIED LIST OF COVARIATES:
#spec.interact<-c("SED_Sediment:DEPTH", 
#                 "DEPTH:VISIBILITY", "DEPTH:logHUMANS20")

#solo.covars<-colnames(mat.dat)[2:(length(xi)+1)]
#mat.nointercept<-mat.dat[,c(solo.covars,spec.interact)]


####### option 4: NO INTERACTIONS:
solo.covars<-colnames(mat.dat)[2:(length(xi)+1)]
mat.nointercept<-mat.dat[,solo.covars]

#all.datforMariska<-cbind(y, dat.final[xa], mat.nointercept)
#write.csv(all.datforMariska, file="allrawdata_MHI.csv", quote=FALSE, row.names=FALSE)

########################################## FINAL DATA CLEANING - Standardize covariates
# set scale variables by two standard deviations (the default in R is to scale by one SD; see Gelman and Hill pg 56)
covars.sd<-apply(mat.nointercept, MARGIN=2, FUN=sd)
Xb <- scale(mat.nointercept, center=TRUE, scale=2*covars.sd) #covariates for densities

# Versus standardizing ONLY the continuous variables and leaving categorical (BINARY) predictors as is
#no.habs<-mat.nointercept[,!(colnames(mat.nointercept)%in%c("MIX", "PAV", "REEF", "ROB"))]
#only.habs<-mat.nointercept[,(colnames(mat.nointercept)%in%c("MIX","PAV","REEF","ROB"))]
#covars.sd<-apply(no.habs, MARGIN=2, FUN=sd)
#Xb.nohab <- scale(no.habs, center=TRUE, scale=2*covars.sd) #covariates for densities
#Xb<-cbind(Xb.nohab, only.habs)

Xb.df<-as.data.frame(Xb)

########################################## CALCULATE VARIANCE INFLATION FACTORS
# BRUTE FORCE - all variables minus CoralSQ and WavesSQ - it's OK for these to be correlated with others

fit <- lm(y~Total.Coral + 
          MA_Macroalga + SED_Sediment + TURF_Turf.Alga + CCA_Coralline.Alga +
          DEPTH + MEAN_SH + VISIBILITY + 
          MEAN_Chla_MEAN + MEAN_Chla_ANOM_FREQ + 
          MEAN_Irradiance_MEAN + MEAN_Irradiance_ANOM_MAX +
          MEAN_SST_SD + MEAN_SST_MEAN + MEAN_SST_ANOM_FREQ +
          MEAN_Waves_MEAN + MEAN_Waves_ANOM_FREQ + 
          logHUMANS20, data=Xb.df)

vif.test.1<-vif(fit)

vif.test.1

write.csv(vif.test.1, file="vif.test.1.csv", quote=FALSE)


### REMOVE HIGHEST vif (TURF_Turf.Alga), test vif again
fit <- lm(y~Total.Coral + 
            MA_Macroalga + SED_Sediment + CCA_Coralline.Alga +
            DEPTH + MEAN_SH + VISIBILITY + 
            MEAN_Chla_MEAN + MEAN_Chla_ANOM_FREQ + 
            MEAN_Irradiance_MEAN + MEAN_Irradiance_ANOM_MAX +
            MEAN_SST_SD + MEAN_SST_MEAN + MEAN_SST_ANOM_FREQ +
            MEAN_Waves_MEAN + MEAN_Waves_ANOM_FREQ + 
            logHUMANS20, data=Xb.df)

vif.test.2<-vif(fit)

vif.test.2

write.csv(vif.test.2, file="vif.test.2.csv", quote=FALSE)


### REMOVE NEXT HIGHEST vif (MEAN_Irradiance_ANOM_MAX) then test vif again
fit <- lm(y~Total.Coral + 
            MA_Macroalga + SED_Sediment + CCA_Coralline.Alga +
            DEPTH + MEAN_SH + VISIBILITY + 
            MEAN_Chla_MEAN + MEAN_Chla_ANOM_FREQ + 
            MEAN_Irradiance_MEAN + 
            MEAN_SST_SD + MEAN_SST_MEAN + MEAN_SST_ANOM_FREQ +
            MEAN_Waves_MEAN + MEAN_Waves_ANOM_FREQ + 
            logHUMANS20, data=Xb.df)

vif.test.3<-vif(fit)

vif.test.3

write.csv(vif.test.3, file="vif.test.3.csv", quote=FALSE)

##########################################################################################
##########################################################################################
# FINAL variable selection for matrix Xb
##########################################################################################
##########################################################################################


# Simplify analysis
# REMOVE IRRADIANCE - not a clear mechanism for affecting fish biomass; also not converging
# REMOVE SST ANOM FREQ - not converging

# LIST OF ALL VARIABLES THAT PASS COLLINEARITY / MULTI-COLLINEARITY tests:
finalcovar <- c("Total.Coral", "Total.sqCoral", 
                "MA_Macroalga", "SED_Sediment", "CCA_Coralline.Alga", 
                "DEPTH", "MEAN_SH", "VISIBILITY",
                "MEAN_Chla_MEAN", "MEAN_Chla_ANOM_FREQ", 
                "MEAN_SST_SD", "MEAN_SST_MEAN",
                "MEAN_Waves_MEAN", "meanWAVESsq", "MEAN_Waves_ANOM_FREQ",
                "logHUMANS20"
                )      

# PLANKTIVORE: Significant Drivers ONLY
#finalcovar <- c("Total.Coral", "Total.sqCoral", 
#                "MA_Macroalga", "SED_Sediment", "CCA_Coralline.Alga", 
#                "DEPTH", "MEAN_SH", "VISIBILITY",
#                "MEAN_Chla_MEAN",
#                "MEAN_Waves_MEAN", "meanWAVESsq",
#                "logHUMANS20"
#)      

# LIST OF ALL SIGNIFICANT DRIVERS FOR FINAL MANUCRIPT (total fish or herbivores) MODEL:
#finalcovar <- c("Total.Coral", "Total.sqCoral", 
#                "SED_Sediment", "CCA_Coralline.Alga", 
#                "DEPTH", "MEAN_SH", "VISIBILITY",
#                "MEAN_SST_MEAN",
#                "MEAN_Waves_MEAN", "meanWAVESsq",
#                "logHUMANS20"
#)      


Xb<-Xb[,finalcovar]

INTERCEPT<-rep(1, length.out=dim(Xb)[1])
Xb <- cbind(INTERCEPT, Xb)


# Set GROUP-LEVEL (ISLAND) index
IS <- as.integer(dat.final$ISLAND)
SC <- as.integer(dat.final$SEC_NAME)


################################################################################
### CREATE NEW DATA FRAME FOR SIMULATIONS: Xb.humansim ########################
################################################################################
################################################################################
##########  HUMAN SIMULATION 1: increase population by defined growth rate #####
################################################################################
################################################################################
#Pfinal = Pinitial * e ^ (r*t); 
#where P = final and initial pop size; 
#r = growth rate (e.g., use 0.016 for 1.68%); 
#t = time
################################################################################
# FIRST, add population growth to original data then re-scale
#r=0.008 # 0.8% per year is based on an estimate by State of Hawaii's Department of Business, Economic Development, and Tourism
#t=40
#dat$HUMANSIM<-dat$HUMANS20 * exp(r*t)
#dat$logHUMANSIM<-log(dat$HUMANSIM)

################################################################################
################################################################################
########## HUMAN SIMULATION 2: BASELINES (HUMANS.sim = 0 or = min(HUMANS) ######
################################################################################ 
################################################################################
dat$HUMANSIM<-min(dat$HUMANS20)
#dat$HUMANSIM<-min(dat$HUMANS200)
dat$logHUMANSIM<-log(dat$HUMANSIM)
dat<-cbind(dat, (dat$logHUMANSIM)^2) 
names(dat)[which(names(dat)=="(dat$logHUMANSIM)^2")]<-"logHUMANSIMsq"
dat<-cbind(dat, (dat$HUMANSIM)^2) 
names(dat)[which(names(dat)=="(dat$HUMANSIM)^2")]<-"HUMANSIMsq"


################################################################################
################################################################################
########  SIMULATE CORAL MORTALITY and/or LOSS OF COMPLEXITY  ##################
################################################################################ 
################################################################################
#mortality<-0.88 # e.g., 0.9 = 90% remaining, i.e., 10% reduction 
# Here 0.88 comes from the Pacific Ocean average of how much coral cover declines after bleaching events
# Baker et al. 2008 in: Estuarine, Coastal and Shelf Science
#dat$CORALSIM<-dat$HARD_CORAL*mortality
#dat$CORALSIMsq<-dat$CORALSIM^2
#complexity<-0.88 # Currently no reference for this
#dat$COMPLEXITYSIM<-dat$SD_SH_DIFF*complexity

### REMINDER: Complexity here is calculated as follows:
# 1. Calculate a WEIGHTED MEAN of the substrate height categories
# i.e., 5 categories are 0-10cm, 20-50cm, 50-100cm, 100-150cm, 150cm+
# The midpoint of each category (5cm, 25cm, 75cm, 125cm, and mean of 150cm and max height)
# ..is then multiplied by the percent which that category covers the area (e.g., 10%, 30%), summed across categories, and then divided by the total (i.e., 100)
# 2. Calculate a STANDARD DEVIATION of the substrate heigh categories using the WEIGHTED MEAN calculated above
# i.e., Same formula for standard deviation, but use the WEIGHTED MEAN as the "mean"

################################################################################
### SELECT THE CORRECT "SIMULATION" VARIABLES (e.g., logHUMANSIM, CORALSIM + CORALSIMsq, COMPLEXITYSIM, etc)
### NOTE: For scale() function to work, order of xsim variables must correspond with order of xi variables 
################################################################################
xsim<-xi
human.col<-grep("HUMANS", xsim)
xsim[human.col]<-"logHUMANSIM"


# Select all site ID and hierarchical ID information
xa <- c("LATITUDE", "LONGITUDE", "SITE", "SITEVISITID", "REGION", "ISLAND", "REEF_ZONE", "OBS_YEAR", "DEPTH_BIN", "SEC_NAME", "METHOD")

### REMOVE ALL NA Data
dat.sim<-cbind(dat[xa], dat[focus], dat[xsim])
dat.sim<-na.omit(dat.sim)


############### WHAT should the minimum sample size be within each sector?
# minN is same as above for dat.final
# THE SAME SECTORS THAT WERE DROPPED in our data should be dropped from Xb.humansim too
# NOTE: Below is hard-coded - need to adjust if you want to drop other sectors
N.check<-as.data.frame(table(dat.sim$SEC_NAME))
limitedSector<-N.check[(N.check[,2] < minN),][1] # Grab the sector name of all sectors that do not meet the minimum N requirement

testSector <- dat.sim$SEC_NAME != limitedSector[1,1] # DROP MAI_HANA
dat.sim<-dat.sim[testSector,]
testSector.2 <- dat.sim$SEC_NAME != limitedSector[2,1] # DROP MAI_MOLOKINI
dat.sim<-dat.sim[testSector.2,]


dat.sim$SEC_NAME<-drop.levels(dat.sim$SEC_NAME)



######## CREATE INTERACTIONS BETWEEN ALL VARIABLES 
interact.sim<-data.frame(y, dat.sim[xsim])
form <- y ~ .^2
mat.sim<-model.matrix(form, data=interact.sim)

######### option 1: BRUTE FORCE - include all interactions in the analysis
#mat.sim.nointercept<-mat.sim[,-1]

######### option 2: include all solo covariates + all interactions with ONE specific variable
# SELECT a FOCAL COVARIATE and all interactions with this covariate
#focal<-"logHUMANSIM" #already defined above for Xb matrix
#cols.interact<-grep(focal, colnames(mat.sim))
#cols.interact<-cols.interact[-1] # Remove the first column as this is the SOLO covariate
#mat.sim.nointercept<-mat.sim[,c(2:(length(xsim)+1),cols.interact)] # Select all solo covars + all interactions with the FOCAL covariate

#mat.sim.nointercept<-mat.sim.nointercept[,1:(length(colnames(mat.sim.nointercept))-3)]

######## option 3: INCLUDE A SPECIFIED LIST OF COVARIATES:

# IF NECESSARY, SPECIFY HUMAN INTERACTIONS (replace logHUMANS20 with logHUMANSIM)
#spec.interact[grep("logHUMANS20", spec.interact)]<-"DEPTH:logHUMANSIM"
#solo.covars<-colnames(mat.sim)[2:(length(xsim)+1)]
#mat.sim.nointercept<-mat.sim[,c(solo.covars,spec.interact)]

#all.datforMariska<-cbind(y, dat.final[xa], mat.nointercept)
#write.csv(all.datforMariska, file="allrawdata_MHI.csv", quote=FALSE, row.names=FALSE)

####### option 4: NO INTERACTIONS:
solo.covars<-colnames(mat.sim)[2:(length(xsim)+1)]
mat.sim.nointercept<-mat.sim[,solo.covars]

########################################## FINAL DATA CLEANING
# Standardize covariates
# NOTE: scale this matrix by the mean and SD of the ORIGINAL matrix mean(mat.nointercept) and covars.sd from above; 
Xb.humansim <- scale(mat.sim.nointercept, center=apply(mat.nointercept, MARGIN=2, FUN=mean), scale=2*covars.sd) #covariates for densities
finalsim<-finalcovar

### CHANGE human variable in finalcovar from logHUMANS20 to logHUMANSIM:::
human.col2<-grep("logHUMANS20", finalsim)
finalsim[human.col2]<-"logHUMANSIM"

Xb.humansim <- Xb.humansim[,finalsim]

# Versus standardizing ONLY the continuous variables and leaving categorical (BINARY) predictors as is
#no.habs.sim<-mat.sim.nointercept[,!(colnames(mat.sim.nointercept)%in%c("MIX", "PAV", "REEF", "ROB"))]
#only.habs.sim<-mat.sim.nointercept[,(colnames(mat.sim.nointercept)%in%c("MIX","PAV","REEF","ROB"))]
#Xb.humansim.nohab <- scale(no.habs.sim, center=apply(no.habs, MARGIN=2, FUN=mean), scale=2*covars.sd) #covariates for densities
#Xb.humansim<-cbind(Xb.humansim.nohab, only.habs.sim)


# Add IDENTITY column (for intercept) to all data matrices (Xa, Xb, Xa.humansim, Xb.humansim)
INTERCEPT<-rep(1, length.out=dim(Xb.humansim)[1])
Xb.humansim <- cbind(INTERCEPT, Xb.humansim)

nobs <- length(y)          #same as sites now, but not in future version
nbeta <- ncol(Xb)   #number of covariates for density
nsite <- nrow(Xb)        #number of sites (each row is a site)
nisland <- length(unique(dat.final$ISLAND))     #number of unique islands
nregion <- length(unique(dat.final$REGION))
nsector <- length(unique(dat.final$SEC_NAME))  


# CREATE a dataframe/matrix of sector ID numbers per island
sectorinfo<-as.data.frame(cbind(IS, SC))
sectorinfo<-unique(sectorinfo)
sectortable<-table(sectorinfo)
numsectors<-apply(sectortable, MARGIN=1, FUN=sum)
sectorIDNumberPerIsland<-cumsum(numsectors)
sectorIDNumberPerIsland<-c(0,sectorIDNumberPerIsland)
names(sectorIDNumberPerIsland)<-NULL

################################################################
################################################################
# BAYESIAN MODEL ###############################################
################################################################
################################################################
# code modified from Gelman and Hill 2007 - pg 378

W.B<-diag(nbeta)

### FROM SESYNC JAGSPRIMER:
#data = list(
#  n = nrow(Logistic),
#  x = as.double(Logistic$PopulationSize),
#  y = as.double(Logistic$GrowthRate))
#y = as.double(Logistic$GrowthRate) creates a y vector required by
#the JAGS program from the column in your data frame called GrowthRate. You might want
#to know why we assigned the data as a double rather than as an integer. The answer is that
#the execution of JAGS is about 5 times faster on double precision than on integers.

mhi.data <- list(y=y, Xb=Xb, N=nobs, nisland=nisland, W.B=W.B, nbeta=nbeta, SC=SC, sectorIDNumberPerIsland=sectorIDNumberPerIsland, Xb.humansim=Xb.humansim) #IS=IS, nvar=nvar, nsite=nsite,

## MATCH chain.b with expected sign of each covariate in colnames(Xb) - this is used for supplying initial values to the MCMC
chain.b<-c(1, 
           1, -1,
           -1, -1, 1,
           1, 1, 1,
           -1, -1,
           -1, -1, 
           1, -1, -1,
           -1)

#           ,rep(0, numb.interactions))

# binary variables are the last columns
#chain.b<-c(1, 
#           1, -1,
#           -1, -1,
#           1, 1, 1,
#           1, -1,
#           -1,
#           rep(0, numb.interactions)
#           )

################################################################################################################################
################################################################################################################################
# SUPPLY INITIAL VALUES - although we are initializing all the raw, unscaled values below (e.g., mu.bRG.raw, mu.bIS.raw, BSC.raw),
# we are actually interested in the derived quantities (mu.RG, mu.bIS, B[SC])
mhi.inits <- list(list(sig.y=0.2, 
                       mu.bRG.raw=c(0.2*chain.b),
                       xi.bRG=runif(n=nbeta,max=1),
                       mu.bIS.raw=matrix(data=0.2*chain.b, nrow=nisland, ncol=nbeta, byrow=TRUE),
                       xi.bIS=runif(n=nbeta,max=1),
                       Tau.BRG.raw=rWishart(n=1, df=nbeta+1, Sigma=diag(nbeta)), 
                       Tau.BIS.raw=rWishart(n=nisland, df=nbeta+1, Sigma=diag(nbeta)), 
                       BSC.raw=matrix(data=0.2*chain.b, nrow=nsector, ncol=nbeta, byrow=TRUE)), 
                  list(sig.y=0.2, 
                       mu.bRG.raw=c(0.3*chain.b),
                       xi.bRG=runif(n=nbeta,max=1),
                       mu.bIS.raw=matrix(data=0.3*chain.b, nrow=nisland, ncol=nbeta, byrow=TRUE),
                       xi.bIS=runif(n=nbeta,max=1),
                       Tau.BRG.raw=rWishart(n=1, df=nbeta+1, Sigma=diag(nbeta)), 
                       Tau.BIS.raw=rWishart(n=nisland, df=nbeta+1, Sigma=diag(nbeta)), 
                       BSC.raw=matrix(data=0.3*chain.b, nrow=nsector, ncol=nbeta, byrow=TRUE)), 
                  list(sig.y=0.2, 
                       mu.bRG.raw=c(0.1*chain.b),
                       xi.bRG=runif(n=nbeta,max=1),
                       mu.bIS.raw=matrix(data=0.1*chain.b, nrow=nisland, ncol=nbeta, byrow=TRUE),
                       xi.bIS=runif(n=nbeta,max=1),
                       Tau.BRG.raw=rWishart(n=1, df=nbeta+1, Sigma=diag(nbeta)), 
                       Tau.BIS.raw=rWishart(n=nisland, df=nbeta+1, Sigma=diag(nbeta)), 
                       BSC.raw=matrix(data=0.1*chain.b, nrow=nsector, ncol=nbeta, byrow=TRUE)) 
)

# THIS WRITES A TEXT FILE LATER READ BY JAGS
cat("model{
    # Define non-varying priors:
    tau.y <- pow(sig.y, -2)
    sig.y ~ dunif(0,100)
    
    # Define means of domain-level betas for multivar distribution and domain and island-level scaling factors
    for (k in 1:(nbeta)){
    mu.bRG[k] <- xi.bRG[k]*mu.bRG.raw[k]    
    mu.bRG.raw[k] ~ dnorm(0, 0.1)
    xi.bRG[k] ~ dunif(0,1)    
    }
    
    for (k in 1:(nbeta)){
    xi.bIS[k] ~ dunif(0,1) 
    }
    
    #define model
    for (i in 1:N) {
    mu[i] <- inprod(B[SC[i],],Xb[i,])
    y[i] ~ dnorm(mu[i],tau.y)

    #Simulations:
    #First, simulate yreplicate data to: (1) compare with humansim and (2) evaluate full model fit
    y.rep[i] ~ dnorm(mu[i],tau.y)
    
    #Then, simulate data of altered human and other drivers
    mu.humansim[i] <- inprod(B[SC[i],],Xb.humansim[i,])
    y.humansim[i] ~ dnorm(mu.humansim[i],tau.y)

    # Then, calculate difference between backtransformed y.humansim and y.rep - i.e., recovery potential
    # Back transform biomass - original transformation was log(x+1)
    yhum.bt[i] <- exp(y.humansim[i])
    yrep.bt[i] <- exp(y.rep[i])
    y.recovery[i] <- yhum.bt[i]-yrep.bt[i]

    #Then, calculate expected percent recovery - i.e., Difference from baseline / Current levels
    y.pctrecov[i] <- y.recovery[i]/yrep.bt[i]*100
    }

    # Calculate Bayesian P-values for mean and sd
    # function step(z) returns a 1 if z > 0, returns 0 otherwise
    mean.y <- mean(y[])
    mean.y.rep <- mean(y.rep[])
    pvalue.mean <- step(mean.y.rep - mean.y)
    
    sd.y <- sd(y[])
    sd.y.new <- sd(y.rep[])
    pvalue.sd <- step(sd.y.new - sd.y)

    # Define coefficients (i.e., varying sector-level intercept and slopes) in terms of multiplicative (i.e., scaling) factors 
    # Drawn from a multi-variate distribution with island-level means
    for (m in 1:nisland){
    for(j in (sectorIDNumberPerIsland[m]+1):sectorIDNumberPerIsland[m+1]){
    for(k in 1:nbeta){
    B[j,k] <- xi.bIS[k]*BSC.raw[j,k]  
    }
    BSC.raw[j,1:nbeta] ~ dmnorm(mu.bIS.raw[m,], Tau.BIS.raw[,,m])
    }
    }

    # Define means of island-level betas drawn from a multi-variate distribution with region-level means
    # Notice that the scaling factor (xi.bIS) scales both the sector-level Betas (BSC.raw, above) and…
    # the island-level means (mu.bIS.raw, below) from which they’re drawn from
    for(m in 1:nisland){
    for (k in 1:(nbeta)){
    mu.bIS[m,k] <- xi.bIS[k]*mu.bIS.raw[m,k]
    }
    mu.bIS.raw[m,1:nbeta] ~ dmnorm(mu.bRG.raw[], Tau.BRG.raw[,,])
    }

    # Set up covariance matrices using inverse-Wishart model, with df = number of coefficients (including intercept) + 1
    # domain-level
    Tau.BRG.raw[1:nbeta,1:nbeta,1] ~ dwish(W.B[,], B.df)
    B.df <- nbeta+1
    Sigma.BRG.raw[1:nbeta,1:nbeta] <- inverse(Tau.BRG.raw[,,])
    # island-level
    for(m in 1:nisland){
    Tau.BIS.raw[1:nbeta,1:nbeta,m] ~ dwish(W.B[,], B.df)
    Sigma.BIS.raw[1:nbeta,1:nbeta,m] <- inverse(Tau.BIS.raw[,,m])
    }
    
    # Calculate standard deviation (sigma) for each intercept and slope and 
    # Correlations (rho) between intercepts and slopes using same scaling factors as above
    # Domain-level
    for (k in 1:nbeta){
    sigma.bRG[k] <- abs(xi.bRG[k])*sqrt(Sigma.BRG.raw[k,k])
    }
    
    for (k in 1:nbeta){
    for (k.prime in 1:nbeta){
    rho.bRG[k,k.prime] <- Sigma.BRG.raw[k,k.prime]/sqrt(Sigma.BRG.raw[k,k]*Sigma.BRG.raw[k.prime,k.prime])
    }
    }
    
    # Island-level
    for(m in 1:nisland){
    for (k in 1:nbeta){
    sigma.bIS[m,k] <- abs(xi.bIS[k])*sqrt(Sigma.BIS.raw[k,k,m])
    }
    }
    
    for (m in 1:nisland){
    for (k in 1:nbeta){
    for (k.prime in 1:nbeta){
    rho.bIS[k,k.prime,m] <- Sigma.BIS.raw[k,k.prime,m]/sqrt(Sigma.BIS.raw[k,k,m]*Sigma.BIS.raw[k.prime,k.prime,m])
    }
    }
    }
    
    }",
    file="model3a.txt" )

############################################################################################# 
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################

# ADAPTATION
start.time.adaptation<-Sys.time()
jm3a <- jags.model(file = "model3a.txt", 
                   data = mhi.data, 
                   n.chains = length(mhi.inits), 
                   inits = mhi.inits, 
                   n.adapt = 5000)
end.time.adaptation<-Sys.time()
# SESYNC - no more than ~8000



# Now, we can run the chain further, while monitoring parameters
pars<-c(
  "B", 
  "mu.bRG",
  "mu.bIS",
  "sig.y",
  "sigma.bRG",
  "sigma.bIS",
  "rho.bIS",
  "rho.bRG",
  "y.rep", "y.humansim", "y.recovery", "y.pctrecov",
  "pvalue.mean", "pvalue.sd")
start.time.coda<-Sys.time()
#n.iter=1000000 # Keep this outside the coda.samples function so that it matches n.iter for jags.samples further below
#z3a <- coda.samples(jm3a, var=pars, n.iter=n.iter, thin=50) 
#n.iter=1000000 # one million
#n.iter=500000 # LONG RUN
n.iter=200000 # NEW LONG RUN
#n.iter=50000 # SHORT RUN
#n.iter=5000 # PRACTICE RUN
n.thin=10
z3a <- coda.samples(jm3a, var=pars, n.iter=n.iter, thin=n.thin) 
end.time.coda<-Sys.time()
# EXAMPLE: if n.adapt = 5000, n.iter=1000, thin=10 - result is a list 100 steps long but starting on step 5000 and ending on step 6000 (trimming every 10)
# AND burn.in refers to the STEP number; so even though the list is only 100steps long, you should set burn.in between 5000 and <6000

# The n.adapt samples (from jags.model function), are NOT the same as the BURN-IN period
# The trace plots illustrate the posterior samples from "n.iter" iterations. 
# Summaries of these samples, after a suitable burn-in, are then used for parameter inference.
# Choose burn.in period based on inspecting traceplots 
# This must be > then n.adapt # total chain length = N.ADAPT + N.ITER, so add N.ADAPT to BURN.IN also to get final chain length

### EFFICIENT COMPUTING??  - chop off burn-in right away and delete (z3a)
#z3a.burnin<-window(z3a.pars, start=burn.in)
#burn.in <- 905000
#burn.in <- 405000 # LONG RUN
burn.in <- 105000 # NEW LONG RUN
#burn.in <- 45000 # SHORT RUN
#burn.in <- 6000 # PRACTICE RUN
z3a.burnin<-window(z3a, start=burn.in)
rm(z3a)

pvals.cols<-grep("pvalue", colnames(z3a.burnin[[1]]))
pval.table<-as.data.frame(summary(z3a.burnin[1:3][,pvals.cols])$statistics)
write.table(x=pval.table, file="bayesianPValues.txt", quote=FALSE, row.names=TRUE)


# Another way to output model samples:
#start.time<-Sys.time()
#z3a.j <- jags.samples(jm3a, variable.names=c("pvalue.mean", "pvalue.sd"), n.iter=n.iter, thin=1) 
#end.time<-Sys.time()
#z3a.j outputs the means of each vector - for pvalue.mean and pvalue.sd this is a vector of 0s and 1s, based on whether they passed the test statistic e.g., step(mean.y.rep - mean.y)
#pvals<-as.data.frame(cbind(mean(z3a.j$pvalue.mean[]), mean(z3a.j$pvalue.sd[])))
#names(pvals)<-c("P-valueMean", "P-valueSD")
#write.table(x=pvals, file="bayesianPValues.txt", quote=FALSE, row.names=FALSE)

## RENAME MCMC variables
isles<-levels(dat.final$ISLAND)
sects<-levels(dat.final$SEC_NAME)
sects
drivers<-colnames(Xb)
drivers

################################################################################
################################################################################
################################################################################
################################################################################
## HARD-CODE driver and sector names here for graphing outputs
################################################################################
sects<-c("Hawaii-Hamakua", "Hawaii-Kona", "Hawaii-Puna", "Hawaii-Southeast",
         "Kauai-East", "Kauai-Na Pali",
         "Lanai-North", "Lanai-South",
         "Maui-Kahului", "Maui-Kihei", "Maui-Lahaina", "Maui-Northeast", "Maui-Northwest",
         "Molokai-Northwest", "Molokai-Pali", "Molokai-South", "Molokai-West",
         "Niihau-East", "Niihau-Lehua", "Niihau-West",
         "Oahu-East", "Oahu-Kaena", "Oahu-Northeast", "Oahu-Northwest", "Oahu-South")

### NOTE: Changed Oahu-North to Oahu-NORTHWEST
### TWO REASONS: (1) sects must be in alphabetical order - i.e., same as LEVELS(dat$Sector) - if downstream re-ordering of axes is to work
### (2) easier to use match(sects, levels(dat$Sector)) and grep if patterns can't be confused (e.g., Oahu-North would atch Oahu-North and Oahu-Northeast)

#drivers<-c("Intercept", "Coral", "Coral x Coral", "Sand", "CCA",
#           "Depth", "Complexity", "Visibility", "SST", "Waves", "Waves x Waves", "Human Density")

## REMINDER - in model, A and B matrix is formed with j (rows) as islands and k (columns) as drivers/covariates: A[j,k] <- xi.a[k]*A.raw[j,k]  
## Check order of colnames in mcmc object
# NOTICE that the order of columns goes: A[1,1], A[2,1], A[3,1], A[4,1] - i.e., A[ISLAND 1, DRIVER 1], A[ISLAND 2, DRIVER 1], A[ISLAND 3, DRIVER 1]
# so the proper way to create a paired list of isles and drivers to replace these column names is:
sector.index.names<-expand.grid(sects,drivers)
sector.index.combo<-paste(sector.index.names$Var1, sector.index.names$Var2) 
isle.index.names<-expand.grid(isles,drivers)
isle.index.combo<-paste(isle.index.names$Var1, isle.index.names$Var2)


# Rename MCMC variables here: 
B.cols<-grep(glob2rx("B[*]"), colnames(z3a.burnin[[1]]))
colnames(z3a.burnin[[1]])[B.cols]<-paste("B[", sector.index.combo, "]", sep="")
colnames(z3a.burnin[[2]])[B.cols]<-paste("B[", sector.index.combo, "]", sep="")
colnames(z3a.burnin[[3]])[B.cols]<-paste("B[", sector.index.combo, "]", sep="")

mu.bIS.cols<-grep(glob2rx("mu.bIS[*]"), colnames(z3a.burnin[[1]]))
colnames(z3a.burnin[[1]])[mu.bIS.cols]<-paste("mu.bIS[", isle.index.combo, "]", sep="")
colnames(z3a.burnin[[2]])[mu.bIS.cols]<-paste("mu.bIS[", isle.index.combo, "]", sep="")
colnames(z3a.burnin[[3]])[mu.bIS.cols]<-paste("mu.bIS[", isle.index.combo, "]", sep="")

mu.bRG.cols<-grep(glob2rx("mu.bRG[*]"), colnames(z3a.burnin[[1]]))
colnames(z3a.burnin[[1]])[mu.bRG.cols]<-paste("mu.bRG[", drivers, "]", sep="")
colnames(z3a.burnin[[2]])[mu.bRG.cols]<-paste("mu.bRG[", drivers, "]", sep="")
colnames(z3a.burnin[[3]])[mu.bRG.cols]<-paste("mu.bRG[", drivers, "]", sep="")

# REMINDER - in model, rho.a and rho.b are formed as rho.a[k,m] <- Sigma.A.raw[k,m]/sqrt(Sigma.A.raw[k,k]*Sigma.A.raw[m,m])
# with k as nalpha and m as nalpha
## Check order of colnames in mcmc object
# rho.a.cols<-grep(glob2rx("rho.a[*]"), colnames(z3a[[1]]))
# colnames(z3a[[1]])[rho.a.cols]
# NOTICE that the order of columns goes: rho.a[1,1], A[2,1], A[3,1], A[4,1] - i.e., rho.a[DRIVER 1 DIRVER 1], rho.a[DRIVER 2, DRIVER 1], rho.a[DRIVER 3, DRIVER 1]
# so the proper way to create a paired list of isles and drivers to replace these column names is:

reg.corr.names<-expand.grid(drivers,drivers)
reg.corr.combo<-paste(reg.corr.names$Var1, reg.corr.names$Var2)

isle.corr.names<-expand.grid(drivers,drivers,isles)
isle.corr.combo<-paste(isle.corr.names$Var1, isle.corr.names$Var2, isle.corr.names$Var3)

rho.bIS.cols<-grep(glob2rx("rho.bIS[*]"), colnames(z3a.burnin[[1]]))
colnames(z3a.burnin[[1]])[rho.bIS.cols]<-paste("rho.bIS[", isle.corr.combo, "]", sep="")
colnames(z3a.burnin[[2]])[rho.bIS.cols]<-paste("rho.bIS[", isle.corr.combo, "]", sep="")
colnames(z3a.burnin[[3]])[rho.bIS.cols]<-paste("rho.bIS[", isle.corr.combo, "]", sep="")

rho.bRG.cols<-grep(glob2rx("rho.bRG[*]"), colnames(z3a.burnin[[1]]))
colnames(z3a.burnin[[1]])[rho.bRG.cols]<-paste("rho.bRG[", reg.corr.combo, "]", sep="")
colnames(z3a.burnin[[2]])[rho.bRG.cols]<-paste("rho.bRG[", reg.corr.combo, "]", sep="")
colnames(z3a.burnin[[3]])[rho.bRG.cols]<-paste("rho.bRG[", reg.corr.combo, "]", sep="")

sig.y.cols<-grep(glob2rx("sig.y"), colnames(z3a.burnin[[1]]))

sigma.bIS.cols<-grep(glob2rx("sigma.bIS[*]"), colnames(z3a.burnin[[1]]))
colnames(z3a.burnin[[1]])[sigma.bIS.cols]<-paste("sigma.bIS[", isle.index.combo, "]", sep="")
colnames(z3a.burnin[[2]])[sigma.bIS.cols]<-paste("sigma.bIS[", isle.index.combo, "]", sep="")
colnames(z3a.burnin[[3]])[sigma.bIS.cols]<-paste("sigma.bIS[", isle.index.combo, "]", sep="")

sigma.bRG.cols<-grep(glob2rx("sigma.bRG[*]"), colnames(z3a.burnin[[1]]))
colnames(z3a.burnin[[1]])[sigma.bRG.cols]<-paste("sigma.bRG[", drivers, "]", sep="")
colnames(z3a.burnin[[2]])[sigma.bRG.cols]<-paste("sigma.bRG[", drivers, "]", sep="")
colnames(z3a.burnin[[3]])[sigma.bRG.cols]<-paste("sigma.bRG[", drivers, "]", sep="")

# Limit convergence diagnostics to model parameters:
z3a.pars<-z3a.burnin[,c(B.cols, mu.bIS.cols, mu.bRG.cols, rho.bIS.cols, rho.bRG.cols,  sig.y.cols, sigma.bIS.cols, sigma.bRG.cols)]

####################################################################################
####################################################################################
####################################################################################
####################################################################################
# TRACEPLOTS
setwd("~/Analyses/_RESULTS/fish-stock")
png(filename="model3a_traceplot%01d.png", width=1080, height=960, pointsize=24)
plot(z3a.pars) 
dev.off()

# CONVERGENCE DIAGNOSTICS: Use Gelman-Rubin - similar to an ANOVA of the MCMC chains: 
# how does within-chain variance compare with between-chain variance
# i.e, have the chains “forgotten” their initial values, and are their outputs indistinguishable
#start.time<-Sys.time()
#g<-gelman.diag(z3a.pars)
#end.time<-Sys.time()

#start.time<-Sys.time()
#gelman.plot(z3a)
#end.time<-Sys.time()

# Error message: Error in chol.default(W) : the leading minor of order 113 is not positive definite
#Why the error message?
#na.rows<-grep("NaN", g)
#g[na.rows]
## From Plummer: That error comes from multivariate version of the Gelman-Rubin diagnostic. 
## The covariance matrix of the parameters is not positive definite. Possible reasons are
## 1) You are monitoring a parameter that is constant (variance = 0)
## 2) Two of the monitored parameters are highly correlated (correlation = 1)

### EITHER, remove all rho.a and rho.b parameters
### REMOVE ALL rho.b parameters (below) or use work-around script for gelman.diag function()
#rho.b.cols<-grep(glob2rx("rho.b[*]"), colnames(z3a.pars[[1]]))
#z3a.gelman.pars<-z3a.pars[1:3][,-c(rho.b.cols)]

## WORK AROUND: can pass variables individually to the gelman.diag function; Use code below for MCMC object, z
g <- matrix(NA, nrow=nvar(z3a.burnin), ncol=2)
for (v in 1:nvar(z3a.burnin)) {
  g[v,] <- gelman.diag(z3a.burnin[,v])$psrf
}
# g is the same gelman.diag dataframe output that would be produced if the correlation parameters (rho's) did not produce error messages
rownames(g)<-colnames(z3a.burnin[[1]])
colnames(g)<-c("Point est.", "Upper C.I.")
write.csv(x=g, file="gelman-rubindiag.csv", quote=FALSE, row.names=TRUE)

g.df<-as.data.frame(g)

# split Gelman-Rubin diagnostics into different dataframes for each family of parameters:
B<-g.df[grep("B\\[", rownames(g.df)),]
mu.bIS<-g.df[grep("mu.bIS", rownames(g.df)),]
mu.bRG<-g.df[grep("mu.bRG", rownames(g.df)),]
rho.bIS<-g.df[grep("rho.bIS", rownames(g.df)),]
rho.bRG<-g.df[grep("rho.bRG", rownames(g.df)),]

sig.y<-g.df[grep("sig.y", rownames(g.df)),]
sigma.bIS<-g.df[grep("sigma.bIS", rownames(g.df)),]
sigma.bRG<-g.df[grep("sigma.bRG", rownames(g.df)),]

par.names<-c("B", "mu.bIS", "mu.bRG", "rho.bIS", "rho.bRG", "sig.y", "sigma.bIS", "sigma.bRG")

# FIRST, just do a BRUTE FORCE PLOT:
for(i in 1:length(par.names))
{
  gelmanrubin.plot<-get(par.names[i])
  # replace rownames with graphing friendly names
  row.names(gelmanrubin.plot)<-gsub(x=row.names(gelmanrubin.plot), pattern=".*\\[|\\]", replacement="") # remove everything BEFORE the "[" and remove the final "]"
  # CONVERGENCE indicated by test-statistic values close to 1 (i.e., Upper CI < 1.1)
  p<-ggplot(gelmanrubin.plot, aes(x=rownames(gelmanrubin.plot), y=gelmanrubin.plot$'Point est.')) +
    geom_errorbar(aes(ymin=gelmanrubin.plot$'Point est.', ymax=gelmanrubin.plot$'Upper C.I.'), size=1, width=0.1, colour="blue") +
    geom_point(colour="blue") +
    geom_hline(aes(yintercept=1.1), linetype="dashed", colour="red") +
    labs(x="Parameter", y="Gelman-Rubin Test Statistic") +
    theme(text = element_text(size=22), axis.text.x=element_text(angle=90, vjust=0.5, hjust=1, colour="black", size=10))
  new.file<-paste("gelmanrubin_plot_", par.names[i],".pdf",sep="")
  pdf(file=new.file)
  print(p)
  dev.off()
}

# All parameter graphs above need to be further subdivided (except sig.y, rho.bRG, and sigma.bRG)

# SPLIT UP B matrix graph
setwd("~/Analyses/_RESULTS/fish-stock")
for(i in 1:length(drivers))
{
  B.plot<-B[((i*nsector)-(nsector-1)):(i*nsector),]
  row.names(B.plot)<-gsub(x=row.names(B.plot), pattern=".*\\[|\\]", replacement="") # pattern=".*\\[| .*\\]" Remove everything before the "[" and everything between the "space" and the "]"
  row.names(B.plot)<-gsub(x=row.names(B.plot), pattern=drivers[i], replacement="")
  p<-ggplot(B.plot, aes(x=row.names(B.plot), y=B.plot$'Point est.')) +
    geom_errorbar(aes(ymin=B.plot$'Point est.', ymax=B.plot$'Upper C.I.'), size=1, width=0.1, colour="blue") +
    geom_point(colour="blue") +
    geom_hline(aes(yintercept=1.1), linetype="dashed", colour="red") +
    labs(x="Sector", title=drivers[i], y="Gelman-Rubin Test Statistic") +
    theme(text = element_text(size=22), axis.text.x=element_text(angle=90, vjust=0.5, hjust=1, colour="black", size=10))
  new.file<-paste("gelmanrubin_plot_B_", drivers[i],".pdf",sep="")
  pdf(file=new.file)
  print(p)
  dev.off()
}

# SPLIT UP mu.bIS graph
setwd("~/Analyses/_RESULTS/fish-stock")
for(i in 1:length(drivers))
{
  mu.bIS.plot<-mu.bIS[((i*nisland)-(nisland-1)):(i*nisland),]
  row.names(mu.bIS.plot)<-gsub(x=row.names(mu.bIS.plot), pattern=".*\\[|\\]", replacement="") # pattern=".*\\[| .*\\]" Remove everything before the "[" and everything between the "space" and the "]"
  row.names(mu.bIS.plot)<-gsub(x=row.names(mu.bIS.plot), pattern=" .*", replacement="")
  p<-ggplot(mu.bIS.plot, aes(x=isles, y=mu.bIS.plot$'Point est.')) +
    geom_errorbar(aes(ymin=mu.bIS.plot$'Point est.', ymax=mu.bIS.plot$'Upper C.I.'), size=1, width=0.1, colour="blue") +
    geom_point(colour="blue") +
    geom_hline(aes(yintercept=1.1), linetype="dashed", colour="red") +
    labs(x="Island", title=drivers[i], y="Gelman-Rubin Test Statistic") +
    theme(text = element_text(size=22), axis.text.x=element_text(angle=90, vjust=0.5, hjust=1, colour="black", size=10))
  new.file<-paste("gelmanrubin_plot_mu.bIS_", drivers[i],".pdf",sep="")
  pdf(file=new.file)
  print(p)
  dev.off()
}

# SPLIT UP rho.bIS graph
setwd("~/Analyses/_RESULTS/fish-stock")
corr.pairs<-length(reg.corr.combo)
for(i in 1:length(isles))
{
  rho.bIS.plot<-rho.bIS[((i*corr.pairs)-(corr.pairs-1)):(i*corr.pairs),]
  row.names(rho.bIS.plot)<-gsub(x=row.names(rho.bIS.plot), pattern=".*\\[|\\]", replacement="") # pattern=".*\\[| .*\\]" Remove everything before the "[" and everything between the "space" and the "]"
  row.names(rho.bIS.plot)<-gsub(x=row.names(rho.bIS.plot), pattern=isles[i], replacement="")
  p<-ggplot(rho.bIS.plot, aes(x=reg.corr.combo, y=rho.bIS.plot$'Point est.')) +
    geom_errorbar(aes(ymin=rho.bIS.plot$'Point est.', ymax=rho.bIS.plot$'Upper C.I.'), size=1, width=0.1, colour="blue") +
    geom_point(colour="blue") +
    geom_hline(aes(yintercept=1.1), linetype="dashed", colour="red") +
    labs(x="Island", title=isles[i], y="Gelman-Rubin Test Statistic") +
    theme(text = element_text(size=22), axis.text.x=element_text(angle=90, vjust=0.5, colour="black", size=10))
  new.file<-paste("gelmanrubin_plot_rho.bIS_", isles[i],".pdf",sep="")
  pdf(file=new.file)
  print(p)
  dev.off()
}
# warnings are for missing values (SELF-CORRELATIONS i.e., Total.Coral vs. Total. Coral)


# SPLIT UP sigma.bIS graph
setwd("~/Analyses/_RESULTS/fish-stock")
for(i in 1:length(drivers))
{
  sigma.bIS.plot<-sigma.bIS[((i*nisland)-(nisland-1)):(i*nisland),]
  row.names(sigma.bIS.plot)<-gsub(x=row.names(sigma.bIS.plot), pattern=".*\\[| .*\\]", replacement="") # pattern=".*\\[| .*\\]" Remove everything before the "[" and everything between the "space" and the "]"
  p<-ggplot(sigma.bIS.plot, aes(x=isles, y=sigma.bIS.plot$'Point est.')) +
    geom_errorbar(aes(ymin=sigma.bIS.plot$'Point est.', ymax=sigma.bIS.plot$'Upper C.I.'), size=1, width=0.1, colour="blue") +
    geom_point(colour="blue") +
    geom_hline(aes(yintercept=1.1), linetype="dashed", colour="red") +
    labs(x="Island", title=drivers[i], y="Gelman-Rubin Test Statistic") +
    theme(text = element_text(size=22), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, colour="black", size=10))
  new.file<-paste("gelmanrubin_plot_sigma.bIS_", drivers[i],".pdf",sep="")
  pdf(file=new.file)
  print(p)
  dev.off()
}


write.csv(rownames(g.df)[g.df$`Upper C.I.` > 1.1], file="gelmanRubin_upperCI_parsThatFailed.csv", row.names=TRUE, quote=FALSE)
write.csv(rownames(g.df)[g.df$`Upper C.I.` < 1.1], file="gelmanRubin_upperCI_parsThatPassed.csv", row.names=TRUE, quote=FALSE)


write.csv(rownames(g.df)[g.df$`Point est.` > 1.1], file="gelmanRubin_pointEst_parsThatFailed.csv", row.names=TRUE, quote=FALSE)
write.csv(rownames(g.df)[g.df$`Point est.` < 1.1], file="gelmanRubin_pointEst_parsThatPassed.csv", row.names=TRUE, quote=FALSE)

################################################################################
################################################################################
### CONTINUE WITH FULL MODEL EVALUATION OUTPUTS ################################
### OUTPUT SIMULATED DATA (y.rep) ##############################################
################################################################################
################################################################################
y.rep.cols<-grep("y.rep", colnames(z3a.burnin[[1]]))

### sim.df used later below for model eval outputs (e.g., observed vs. predicted)
### for this I use 95% credible intervals so use summary() function to get quantiles
z3a.sim<-z3a.burnin[,c(y.rep.cols)] 
sim.summary<-summary(z3a.sim, start = burn.in)
sim.df<-as.data.frame(sim.summary$quantiles)

# Get yreps
yreps<-sim.df[grep("y.rep", rownames(sim.df)),]
names(yreps) <- c("yreps2.5", "yreps25", "yreps50", "yreps75", "yreps97.5")

# calculate rsquared
r.sq<-cor(yreps$yreps50, y) 
r.sq.2<-round(r.sq, digits=2)

# calculate coverage

y.outside<-rep(NA, length(y))
overpredicting<-y<yreps$yreps2.5
underpredicting<-y>yreps$yreps97.5
y.outside[overpredicting]<-"OVER"
y.outside[underpredicting]<-"UNDER"
y.outside[is.na(y.outside)]<-"CI OVERLAP"
inside<-y.outside=="CI OVERLAP"
coverage<-sum(inside)/length(inside)*100 
coverage.2<-round(coverage, digits=2)

# Calculate residuals
residuals<-y-yreps$yreps50 #residual = observed value minus predicted value

#### Plot Residuals 
yrep.corr<-as.data.frame(cbind(y, yreps, y.outside, residuals))
res<-ggplot(yrep.corr, aes(x=yrep.corr$yreps50, y=yrep.corr$residuals))+
  geom_point()+
  labs(x="predicted", y="residual")
new.file<-paste("model3a_residuals.pdf",sep="")
pdf(file=new.file)
print(res)
dev.off()

# Get corresponding Predictors
covar.all<-dat.final[finalcovar]
names(covar.all)<-drivers[-1]
res.bycovar<-as.data.frame(cbind(covar.all, residuals))

### PLOT Residuals vs Predictors (are there any non-linear responses?)
for (i in 1:(length(finalcovar))){
  resVsPred<-ggplot(res.bycovar, aes(x=res.bycovar[,i], y=res.bycovar$residuals))+
    geom_point()+
    labs(x=names(res.bycovar)[i], y="residual")
  new.file<-paste("model3a_residualsVs", names(res.bycovar)[i],".pdf",sep="")
  pdf(file=new.file)
  print(resVsPred)
  dev.off()
}

############################################################ PLOT Predicted vs. Observed 
yrep.full<-as.data.frame(cbind(y, yreps, y.outside))

fit<-ggplot(yrep.full, aes(x=yrep.full$y, y=yrep.full$yreps50))+
  geom_errorbar(aes(ymin=yrep.full$yreps2.5, ymax=yrep.full$yreps97.5, colour=y.outside)) +
  scale_color_manual(values=c("gray65", "blue2", "red2")) +
  geom_point(x=yrep.full$y, y=yrep.full$yreps50)+
  labs(x="observed", y="predicted") +
  geom_abline(aes(intercept=0, slope=1), colour="black") +
  annotate("text", x=0, y=max(yrep.full$yreps97.5), hjust=0, label=paste("R squared = ", r.sq.2, sep="")) +
  annotate("text", x=0, y=max(yrep.full$yreps97.5)*0.95, hjust=0, label=paste("Coverage = ", coverage.2, "%", sep="")) +
  theme(text = element_text(size=18),
        axis.text.y=element_text(size=18),
        axis.text.x=element_text(vjust=0.5, size=18),
        legend.position="bottom")
pdf(file="model3a_FullModel_predictedVsObserved.pdf")
print(fit)
dev.off()

# PLOT without error bars
fit<-ggplot(yrep.full, aes(x=yrep.full$y, y=yrep.full$yreps50))+
  geom_point()+
  labs(x="observed", y="predicted")+
  geom_abline(aes(intercept=0, slope=1), colour="black")+
  annotate("text", x=0, y=max(yrep.full$y, yrep.full$yreps50), hjust=0,  label=paste("R squared = ", r.sq.2, sep=""))+
  annotate("text", x=0, y=max(yrep.full$y, yrep.full$yreps50)*0.95, hjust=0, label=paste("Coverage = ", coverage.2, "%", sep=""))
pdf(file="model3a_FullModel_predictedVsObserved2.pdf")
print(fit)
dev.off()

# Set up data frame to Color Code Model Output Graphs by covariate, island, and sector
yrep.full.covars<-as.data.frame(cbind(y, yreps, y.outside, Xb, dat.final$ISLAND))
names(yrep.full.covars)[8:19]<-drivers
names(yrep.full.covars)[length(yrep.full.covars)]<-"ISLAND"
yrep.full.covars<-as.data.frame(cbind(yrep.full.covars, dat.final$SEC_NAME))
names(yrep.full.covars)[length(yrep.full.covars)]<-"SECTOR"

nointercept.drivers<-drivers[-1]

# Color code by COVARIATE - USE CONTINUOUS COLORS 
for(i in 1:length(nointercept.drivers))
{
  fit<-ggplot(yrep.full.covars, aes(x=yrep.full.covars$y, y=yrep.full.covars$yreps50))+
    geom_point(aes(color=yrep.full.covars[nointercept.drivers[i]]))+
    scale_colour_gradientn(colours=c("yellow", "blue"), name=nointercept.drivers[i])+
    labs(x="observed", y="predicted")+
    geom_abline(aes(intercept=0, slope=1), colour="black")+
    theme(legend.position="bottom", legend.key.size=unit(1, "cm"))
  covarname<-paste("model3a_FullModel_CovarPredictedVsObserved_", nointercept.drivers[i], ".pdf", sep="")
  pdf(file=covarname)
  print(fit)
  dev.off()
}

# Color Code by ISLAND - USE DISCREET COLOR GRADIENT - any differences in model's performance for different islands?
fit<-ggplot(yrep.full.covars, aes(x=yrep.full.covars$y, y=yrep.full.covars$yreps50))+
  geom_point(aes(color=yrep.full.covars$ISLAND))+
  scale_colour_brewer(name="ISLAND", palette="Dark2")+
  labs(colour="ISLAND", x="observed", y="predicted")+
  geom_abline(aes(intercept=0, slope=1), colour="black")+
  theme(legend.position="bottom")
pdf(file="model3a_FullModel_PredictedVsObserved_ISLAND.pdf")
print(fit)
dev.off()


# Code SECTOR by SHAPE and COLOR - any differences in model's performance for different sectors?
fit<-ggplot(yrep.full.covars, aes(x=yrep.full.covars$y, y=yrep.full.covars$yreps50, group=SECTOR, color=SECTOR, shape=SECTOR))+
  scale_shape_manual(values=1:nlevels(yrep.full.covars$SECTOR))+
  geom_point()+
  labs(colour="SECTOR", x="observed", y="predicted")+
  geom_abline(aes(intercept=0, slope=1), colour="black")+
  theme(legend.position="bottom")
pdf(file="model3a_FullModel_PredictedVsObserved_SECTOR_.pdf")
print(fit)
dev.off()


# CODE BELOW IS FOR EXPLORING THE ROLE OF HABITAT CODES - final model does not include these variables but still interesting
# NEED TO ADD HABITAT ID COLUMN (right now, we just have one binary column PER habitat category)
yrep.habid<-yrep.full.covars
yrep.habid<-cbind(yrep.habid, dat.final$HABITAT_CODE)
names(yrep.habid)[dim(yrep.habid)[2]]<-"HABITAT_CODE"

# ADD ID VARIABLE FOR PAV vs NONPAV
pave.hab<-(dat.final$HABITAT_CODE=="PAV")
yrep.habid$PAV<-as.numeric(pave.hab)

# ADD ID VARIABLE FOR ROB vs NONROB
rob.hab<-dat.final$HABITAT_CODE=="ROB" 
yrep.habid$ROB<-as.numeric(rob.hab)

# ADD ID VARIABLE FOR REEF vs NONREEF
#reef.hab<-(dat$HABITAT_CODE=="AGR" | dat$HABITAT_CODE=="APR" | dat$HABITAT_CODE=="APS")
reef.hab<-(dat.final$HABITAT_CODE=="AGR")
yrep.habid$REEF<-as.numeric(reef.hab)

pav.test<-yrep.habid$PAV>0 # because covars are scaled, PAV is no longer 1 vs. 0 (but positive vs negative)
reef.test<-yrep.habid$REEF>0
rob.test<-yrep.habid$ROB>0
yrep.habid$HAB[pav.test]<-"PAV"
yrep.habid$HAB[reef.test]<-"AGR"
yrep.habid$HAB[rob.test]<-"ROB"
other.test<-is.na(yrep.habid$HAB)
levels(yrep.habid$HAB)[length(yrep.habid$HAB)+1]<-"OTHER"
yrep.habid$HAB[other.test]<-"OTHER"
yrep.habid$HAB<-drop.levels(yrep.habid$HAB)

## ALL POINTS with HABITAT CODE
fit<-ggplot(yrep.habid, aes(x=yrep.habid$y, y=yrep.habid$yreps50, group=HABITAT_CODE, color=HABITAT_CODE, shape=HABITAT_CODE))+
  scale_shape_manual(values=1:nlevels(yrep.habid$HABITAT_CODE))+
  geom_point()+
  labs(x="observed", y="predicted")+
  ggtitle(levels(yrep.habid$SECTOR)[i])+
  geom_abline(aes(intercept=0, slope=1), colour="black")+
  theme(legend.position="bottom")
pdf(file=paste("model3a_FullModel_PredictedVsObserved_HABITATCODE.pdf", sep=""))
print(fit)
dev.off()


## Make separate predicted vs observed plot for EACH sector and color-code by habitat type
for (i in 1:length(levels(yrep.habid$SECTOR)))
{
  fit.sector<-subset(yrep.habid, SECTOR==levels(yrep.habid$SECTOR)[i])
  fit<-ggplot(fit.sector, aes(x=fit.sector$y, y=fit.sector$yreps50, group=HABITAT_CODE, color=HABITAT_CODE, shape=HABITAT_CODE))+
    scale_shape_manual(values=1:nlevels(fit.sector$HABITAT_CODE))+
    geom_point()+
    coord_cartesian(xlim=c(min(yrep.full.covars$y),max(yrep.full.covars$y)), ylim=c(min(yrep.full.covars$yreps50),max(yrep.full.covars$yreps50)))+
    labs(x="observed", y="predicted")+
    ggtitle(levels(fit.sector$SECTOR)[i])+
    geom_abline(aes(intercept=0, slope=1), colour="black")
  pdf(file=paste("model3a_FullModel_PredictedVsObserved_SECTOR_", levels(yrep.habid$SECTOR)[i], ".pdf", sep=""))
  print(fit)
  dev.off()
}

####################################################################
#### PLOT PARAMETER ESTIMATES ######################################
####################################################################
####################################################################

# USE CUSTOMIZED QUANTILES TO GRAPH COEFFICIENT PLOTS
#quantilesofinterest=c(.99975, .75, .5, .25, .00025) # i.e., 95% confidence intervals
quantilesofinterest=c(.83, .75, .5, .25, .17) # i.e., 66% confidence intervals
allchain.quantiles<-as.data.frame(NA)
for(i in 1:length(z3a.pars)){
  
  
  z3a.pars.df<-as.data.frame(z3a.pars[[i]])
  par.quantiles<-apply(X=z3a.pars.df, MARGIN=2, FUN=quantile, probs=quantilesofinterest)
  par.quantiles<-t(par.quantiles)
  par.quantiles<-as.data.frame(par.quantiles)
  allchain.quantiles<-as.data.frame(cbind(allchain.quantiles, par.quantiles))
}

## AVERAGE ACROSS THE THREE CHAINS; 

firstquantile<-paste(quantilesofinterest[1]*100, "%", sep="")
firstquant.cols<-grep(firstquantile, names(allchain.quantiles))
allchain.firstquant<-as.data.frame(apply(allchain.quantiles[,firstquant.cols], MARGIN=1, FUN=mean))
names(allchain.firstquant)<-firstquantile

df.quantiles<-allchain.firstquant

quantilelist<-paste(quantilesofinterest[-1]*100, "%", sep="")
#quantilelist<-c("75%", "50%", "25%", "17%")
for(i in 1:length(quantilelist)){
  quant<-quantilelist[i]
  #quant.cols<-grep(quant, names(allchain.quantiles))
  quant.cols<-which(names(allchain.quantiles)==quant)
  allchain.quant<-as.data.frame(apply(allchain.quantiles[,quant.cols], MARGIN=1, FUN=mean))
  names(allchain.quant)<-quantilelist[i]
  df.quantiles<-cbind(allchain.quant, df.quantiles)
}

coeff.df<-df.quantiles # Use coeff.df for coefficient plots below



B<-coeff.df[grep("B\\[", rownames(coeff.df)),]
mu.bIS<-coeff.df[grep("mu.bIS", rownames(coeff.df)),]
mu.bRG<-coeff.df[grep("mu.bRG", rownames(coeff.df)),]
rho.bIS<-coeff.df[grep("rho.bIS", rownames(coeff.df)),]
rho.bRG<-coeff.df[grep("rho.bRG", rownames(coeff.df)),]
sig.y<-coeff.df[grep("sig.y", rownames(coeff.df)),]
sigma.bIS<-coeff.df[grep("sigma.bIS", rownames(coeff.df)),]
sigma.bRG<-coeff.df[grep("sigma.bRG", rownames(coeff.df)),]


par.names<-c("B", "mu.bIS", "mu.bRG", "rho.bIS", "rho.bRG", "sig.y", "sigma.bIS", "sigma.bRG")

# BRUTE FORCE PLOT:
setwd("~/Analyses/_RESULTS/fish-stock")
for(i in 1:length(par.names))
{
  par.plot<-get(par.names[i])
  row.names(par.plot)<-gsub(x=row.names(par.plot), pattern=".*\\[|\\]", replacement="") # remove everything BEFORE the "[" and remove the final "]"
  p<-ggplot(par.plot, aes(x=rownames(par.plot), y=par.plot[,3])) +
    geom_errorbar(aes(ymin=par.plot[,1], ymax=par.plot[,5]), size=1, width=0.1, colour="blue") +
    geom_point(colour="blue") +
    geom_hline(aes(yintercept=0), linetype="dashed", colour="red") +
    labs(x="", title="", y="coefficient") +
    coord_cartesian(ylim=c(-10, 15))+
    coord_flip()+
    theme(text = element_text(size=22), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, colour="black", size=10))
  new.file<-paste("model3a_par_estimates_", par.names[i],".pdf",sep="")
  pdf(file=new.file)
  print(p)
  dev.off()
}

#####################
# All parameter graphs above need to be further subdivided (except sig.y, rho.bRG, and sigma.bRG)

# SPLIT UP B Matrix
setwd("~/Analyses/_RESULTS/fish-stock")
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
  p<-ggplot(B.plot, aes(x=sects, y=B.plot[,3])) +
    geom_segment(data=mu.bIS.plot, aes(x=mu.bIS.plot$xSector1[1], xend=mu.bIS.plot$xSector2[1], y=mu.bIS.plot$`50%`[1],  yend=mu.bIS.plot$`50%`[1]), linetype="solid", colour="gray70", lineend="butt", size=2)+
    geom_segment(data=mu.bIS.plot, aes(x=mu.bIS.plot$xSector1[2], xend=mu.bIS.plot$xSector2[2], y=mu.bIS.plot$`50%`[2],  yend=mu.bIS.plot$`50%`[2]), linetype="solid", colour="gray70", lineend="butt", size=2)+
    geom_segment(data=mu.bIS.plot, aes(x=mu.bIS.plot$xSector1[3], xend=mu.bIS.plot$xSector2[3], y=mu.bIS.plot$`50%`[3],  yend=mu.bIS.plot$`50%`[3]), linetype="solid", colour="gray70", lineend="butt", size=2)+
    geom_segment(data=mu.bIS.plot, aes(x=mu.bIS.plot$xSector1[4], xend=mu.bIS.plot$xSector2[4], y=mu.bIS.plot$`50%`[4],  yend=mu.bIS.plot$`50%`[4]), linetype="solid", colour="gray70", lineend="butt", size=2)+
    geom_segment(data=mu.bIS.plot, aes(x=mu.bIS.plot$xSector1[5], xend=mu.bIS.plot$xSector2[5], y=mu.bIS.plot$`50%`[5],  yend=mu.bIS.plot$`50%`[5]), linetype="solid", colour="gray70", lineend="butt", size=2)+
    geom_segment(data=mu.bIS.plot, aes(x=mu.bIS.plot$xSector1[6], xend=mu.bIS.plot$xSector2[6], y=mu.bIS.plot$`50%`[6],  yend=mu.bIS.plot$`50%`[6]), linetype="solid", colour="gray70", lineend="butt", size=2)+
    geom_segment(data=mu.bIS.plot, aes(x=mu.bIS.plot$xSector1[7], xend=mu.bIS.plot$xSector2[7], y=mu.bIS.plot$`50%`[7],  yend=mu.bIS.plot$`50%`[7]), linetype="solid", colour="gray70", lineend="butt", size=2)+
    geom_errorbar(aes(ymin=B.plot[,1], ymax=B.plot[,5]), size=0.6, width=0.3, colour="darkorange3") +
    geom_hline(aes(yintercept=0), linetype="dotted", colour="black") +
    labs(x="", title=drivers[i], y="coefficient") +
    theme_light()+
    theme(text = element_text(size=22), axis.text.y=element_text(colour="black"), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, colour="black", size=10))+
    geom_point(colour="darkorange4", size=1.8, shape=23, fill="darkorange3")
    
    
  new.file<-paste("model3a_par_estimates_b_", drivers[i],".pdf",sep="")
  pdf(file=new.file)
  print(p)
  dev.off()
}

### ORIGINAL GRAPH - with no ISLAND-LEVEL MEANS
#p<-ggplot(B.plot, aes(x=sects, y=B.plot[,3])) +
#  geom_errorbar(aes(ymin=B.plot[,1], ymax=B.plot[,5]), size=1, width=0.1, colour="blue") +
#  geom_point(colour="blue") +
#  geom_hline(aes(yintercept=0), linetype="dashed", colour="red") +
#  geom_errorbarh(y=mu.bIS.plot$`50%`, xmin=mu.bIS.plot$xSector1, xmax=mu.bIS.plot$xSector2, linetype="dashed", colour="green")+
#  labs(x="", title=drivers[i], y="effect") +
#  theme(text = element_text(size=22), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, colour="black", size=10))+
#  new.file<-paste("model3a_par_estimates_b_", drivers[i],".pdf",sep="")
#pdf(file=new.file)
#print(p)
#dev.off()

# SPLIT UP mu.bIS graph
setwd("~/Analyses/_RESULTS/fish-stock")
for(i in 1:length(drivers))
{
  mu.bIS.plot<-mu.bIS[((i*nisland)-(nisland-1)):(i*nisland),]
  row.names(mu.bIS.plot)<-gsub(x=row.names(mu.bIS.plot), pattern=".*\\[|\\]", replacement="") # pattern=".*\\[| .*\\]" Remove everything before the "[" and everything between the "space" and the "]"
  row.names(mu.bIS.plot)<-gsub(x=row.names(mu.bIS.plot), pattern=" .*", replacement="")
  p<-ggplot(mu.bIS.plot, aes(x=isles, y=mu.bIS.plot[,3])) +
    geom_errorbar(aes(ymin=mu.bIS.plot[,1], ymax=mu.bIS.plot[,5]), size=1, width=0.1, colour="blue") +
    geom_point(colour="blue") +
    geom_hline(aes(yintercept=0), linetype="dashed", colour="red") +
    labs(x="Island", title=drivers[i], y="effect") +
    theme(text = element_text(size=22), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, colour="black", size=10))
  new.file<-paste("model3a_par_estimates_mu.bIS_", drivers[i],".pdf",sep="")
  pdf(file=new.file)
  print(p)
  dev.off()
}

# SPLIT UP rho.bIS graph
setwd("~/Analyses/_RESULTS/fish-stock")
corr.pairs<-length(reg.corr.combo)
for(i in 1:length(isles))
{
  rho.bIS.plot<-rho.bIS[((i*corr.pairs)-(corr.pairs-1)):(i*corr.pairs),]
  row.names(rho.bIS.plot)<-gsub(x=row.names(rho.bIS.plot), pattern=".*\\[|\\]", replacement="") # pattern=".*\\[| .*\\]" Remove everything before the "[" and everything between the "space" and the "]"
  row.names(rho.bIS.plot)<-gsub(x=row.names(rho.bIS.plot), pattern=isles[i], replacement="")
  p<-ggplot(rho.bIS.plot, aes(x=reg.corr.combo, y=rho.bIS.plot[,3])) +
    geom_errorbar(aes(ymin=rho.bIS.plot[,1], ymax=rho.bIS.plot[,5]), size=1, width=0.1, colour="blue") +
    geom_point(colour="blue") +
    geom_hline(aes(yintercept=0), linetype="dashed", colour="red") +
    labs(x="Island", title=isles[i], y="effect") +
    theme(text = element_text(size=22), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, colour="black", size=8))
  new.file<-paste("model3a_par_estimates_rho.bIS_", isles[i],".pdf",sep="")
  pdf(file=new.file)
  print(p)
  dev.off()
}

# SPLIT UP sigma.bIS graph
setwd("~/Analyses/_RESULTS/fish-stock")
for(i in 1:length(drivers))
{
  sigma.bIS.plot<-sigma.bIS[((i*nisland)-(nisland-1)):(i*nisland),]
  row.names(sigma.bIS.plot)<-gsub(x=row.names(sigma.bIS.plot), pattern=".*\\[| .*\\]", replacement="")
  p<-ggplot(mu.bIS.plot, aes(x=isles, y=sigma.bIS.plot[,3])) +
    geom_errorbar(aes(ymin=sigma.bIS.plot[,1], ymax=sigma.bIS.plot[,5]), size=1, width=0.1, colour="blue") +
    geom_point(colour="blue") +
    geom_hline(aes(yintercept=0), linetype="dashed", colour="red") +
    labs(x="Island", title=drivers[i], y="effect") +
    theme(text = element_text(size=22), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, colour="black", size=10))
  new.file<-paste("model3a_par_estiamtes_sigma.bIS_", drivers[i],".pdf",sep="")
  pdf(file=new.file)
  print(p)
  dev.off()
}

# PLOT parameters BY SECTOR, for ALL SECTORS
# NOTE: Sector names below are hardcoded (need to make this more flexible)
# IF SOME ERROR BARS ARE NOT PLOTTING, WILL NEED TO CHANGE BOUNDS OF YLIM
marxan<-sects # Originally requested by Anne Rosinski for DAR's Marxan Project
for(i in 1:length(marxan)){
  B.plot<-B[grep(paste("\\b",marxan[i], "\\b", sep=""), rownames(B)),] ## \\b places "word boundaries" on the search pattern (e.g., For pattern="Oahu-N", grep will return "Oahu-N", but not "Oahu-NE")
  row.names(B.plot)<-gsub(x=row.names(B.plot), pattern=".*\\[|\\]", replacement="") # pattern=".*\\[| .*\\]" Remove everything before the "[" and the final "]"
  row.names(B.plot)<-gsub(x=row.names(B.plot), pattern=paste(marxan[i], " ", sep=""), replacement="") # Remove the driver
  p<-ggplot(B.plot, aes(x=rownames(B.plot), y=B.plot[,3])) +
    geom_errorbar(aes(ymin=B.plot[,1], ymax=B.plot[,5]), size=1, width=0.1, colour="blue") +
    geom_point(colour="blue") +
    ylim(-2,4) + # -2.25 and 3.75 are good bounds for ALL REEF FISH, -2 and 4 are good bounds for HERBIVORE graphs
    geom_hline(aes(yintercept=0), linetype="dashed", colour="red") +
    coord_flip() +
    labs(x="", title=marxan[i], y="") +
    theme(text = element_text(size=12), axis.text.x=element_text(vjust=0.5, colour="black", size=18), plot.title=element_text(size=24))
  new.file<-paste("model3a_parbySECTOR_estimates_b_", marxan[i],".pdf",sep="")
  pdf(file=new.file)
  print(p)
  dev.off()
}

### EXPORT REGION-level pars - later, combine two analyses (total and herbivorous reef fish) onto one plot
write.csv(mu.bRG, file="par_estimates_mu.bRG_quantiles.csv", quote=FALSE, row.names=TRUE)
### EXPORT ISLAND-level pars
write.csv(mu.bIS, file="par_estimates_mu.bIS_quantiles.csv", quote=FALSE, row.names=TRUE)
### EXPORT sector-level pars
write.csv(coeff.df, file="par_estimates_b_quantiles.csv", quote=FALSE, row.names=TRUE)



###############################################################################################
###############################################################################################
#### EVALUATE MODEL AT THE SECTOR-LEVEL
#### CALCULATE SECTOR-LEVEL AVERAGES OF Y.REPS (MODEL SIMULATED DATA)
#### PARSE through y.reps and y.humansim using SC (SECTOR-LEVEL ID)
###############################################################################################
###############################################################################################

z3a.yrep<-z3a.burnin[,y.rep.cols]

# use 95% CI for model eval at sector-level
quantilesofinterest=c(.025, .25, .5, .75, .975)
allchain.quantiles<-as.data.frame(NA)
for(i in 1:length(z3a.yrep)){
  
  
  z3a.yrep.df<-as.data.frame(z3a.yrep[[i]])
  z3a.yrep.tf<-t(z3a.yrep.df)
  z3a.yrep.tf<-as.data.frame(z3a.yrep.tf)
  z3a.yrep.tf<-cbind(dat.final$SEC_NAME, z3a.yrep.tf)
  names(z3a.yrep.tf)[1]<-"SC"
  
  
  ## THEN, use "SC" to calculate mean biomass aggregated at the sector level at EACH STEP
  mcmc.y.sector.mean<-aggregate(as.matrix(z3a.yrep.tf[,2:ncol(z3a.yrep.tf)]) ~ z3a.yrep.tf$SC, data=z3a.yrep.tf, FUN=mean)
  
  
  # Each row is now a mcmc of sector-level y.reps: use each row to generate sector-level means and quantiles
  y.sector.quantiles<-apply(X=mcmc.y.sector.mean[,-1], MARGIN=1, FUN=quantile, probs=quantilesofinterest)
  colnames(y.sector.quantiles)<-mcmc.y.sector.mean[,1]
  y.sector.quantiles<-t(y.sector.quantiles)
  y.sector.quantiles<-as.data.frame(y.sector.quantiles)
  allchain.quantiles<-as.data.frame(cbind(allchain.quantiles, y.sector.quantiles))
}

## AVERAGE ACROSS THE THREE CHAINS; 

firstquantile<-"2.5%"
#firstquant.cols<-grep(firstquantile, names(allchain.quantiles))
firstquant.cols<-which(names(allchain.quantiles)==firstquantile)
allchain.firstquant<-as.data.frame(apply(allchain.quantiles[,firstquant.cols], MARGIN=1, FUN=mean))
names(allchain.firstquant)<-firstquantile

df.quantiles<-allchain.firstquant

quantilelist<-c("25%", "50%", "75%", "97.5%")
for(i in 1:length(quantilelist)){
  quant<-quantilelist[i]
  #quant.cols<-grep(quant, names(allchain.quantiles))
  quant.cols<-which(names(allchain.quantiles)==quant)
  allchain.quant<-as.data.frame(apply(allchain.quantiles[,quant.cols], MARGIN=1, FUN=mean))
  names(allchain.quant)<-quantilelist[i]
  df.quantiles<-cbind(allchain.quant, df.quantiles)
}

# SECTOR-LEVEL OBSERVED DATA: CALCULATE 95% CONFIDENCE INTERVALS from a normal distribution
y.mean <- aggregate(y, list(dat.final$SEC_NAME), mean)
names(y.mean)<-c("SC", "mean")
y.sd <- aggregate(y, list(dat.final$SEC_NAME), sd)
y.n <- aggregate(y, list(dat.final$SEC_NAME), length)

y.error <- qnorm(0.975)*y.sd[,2]/sqrt(y.n[,2])
y.upper <- y.mean[,2]+y.error
y.lower <- y.mean[,2]-y.error

# PLOT BAR GRAPHS OBSERVED vs MODELED log BIOMASS
dat<-cbind(y.mean, y.upper, y.lower)
dat$SC<-sects
colnames(dat)[2]<-"Observed"


# In graphing output - axis.order will be used below to create gray boxes for modeled biomass
# Modified from Ivor's code for PLoS et al. 2015 final figure
axis.order <- seq(1, dim(df.quantiles)[1], by=1)

row.names(df.quantiles)<-sects
df.quantiles<-cbind(df.quantiles, axis.order)
names(df.quantiles)[1]<-"Upper"
names(df.quantiles)[3]<-"Predicted"
names(df.quantiles)[5]<-"Lower"

dat.graph<-dat
dat.graph<-cbind(df.quantiles, dat.graph)

dat.bt.graph<-dat.graph
dat.bt.graph[,c(1:5,8:10)]<-exp(dat.bt.graph[,c(1:5,8:10)])

##### MODELED biomass will be represented by GRAY BOXES with the following bounds:
pred.b <- aes(xmax = axis.order + 0.45, xmin = axis.order - 0.45, ymax=Upper, ymin=Lower)#

p=ggplot(data=dat.graph, aes(x=SC, y=Predicted)) + 
  geom_point(aes(x=SC, y=Predicted)) +
  geom_rect(pred.b, fill = "gray40", alpha=.6) +
  geom_errorbar(aes(ymin=y.lower, ymax=y.upper), size=1.1, colour="red3", width=0.5) +
  geom_point(aes(x=SC, y=Observed), shape=23, size=1.5, colour="red3", fill="red3") +
  geom_point(aes(x=SC, y=Predicted)) +
  labs(y=expression(paste("log Biomass (g",m^{-2}, ")")), x="") +
  theme_light()+
  theme(text = element_text(size=18), axis.text.x=element_text(vjust=0.5, colour="black", size=18), 
        legend.position="none") +
  coord_flip() 
  #geom_errorbar(aes(ymin=dat.graph$Lower, ymax=dat.graph$Upper, colour=variable), position=position_dodge(width=1))

pdf(file="sectorLevelObservedVsPredicted_logBiomass_independentChains_avgQuantiles.pdf")
print(p)
dev.off()

# PLOT backtransformed values
pred.b <- aes(xmax = axis.order + 0.45, xmin = axis.order - 0.45, ymax=Upper, ymin=Lower)#

p=ggplot(data=dat.bt.graph, aes(x=SC, y=Predicted)) + 
  geom_point(aes(x=SC, y=Predicted)) +
  geom_rect(pred.b, fill = "gray40", alpha=.6) +
  geom_errorbar(aes(ymin=y.lower, ymax=y.upper), size=1.1, colour="red3", width=0.5) +
  geom_point(aes(x=SC, y=Observed), shape=23, colour="red3", fill="red3") +
  geom_point(aes(x=SC, y=Predicted)) +
  labs(y=expression(paste("Biomass (g",m^{-2}, ")")), x="")+
  theme_light()+
  theme(text = element_text(size=18), axis.text.x=element_text(vjust=0.5, colour="black", size=18), 
        legend.position="none") +
  coord_flip() 

pdf(file="sectorLevelObservedVsPredicted_backtransBiomass_independentChains_avgQuantiles.pdf")
print(p)
dev.off()



##########################################################################################
############## 1:1 observed vs predicted plot at the sector-level
############## Still using 95% CI as calculated for both y.mean and z3a.yrep above
##########################################################################################
y.mean$SC<-sects
sector.dat<-cbind(df.quantiles, y.mean[,2])
names(sector.dat)[dim(sector.dat)[2]]<-"y.mean"

# calculate rsquared
r.sq.sector<-cor(sector.dat$Predicted, sector.dat$y.mean) 
r.sq.2.sector<-round(r.sq.sector, digits=2)

# calculate coverage
sect.outside<-rep(NA, dim(sector.dat)[1])
overpredicting<-sector.dat$`2.5%`>y.mean$mean
underpredicting<-sector.dat$`97.5%`<y.mean$mean
sect.outside[overpredicting]<-"OVER"
sect.outside[underpredicting]<-"UNDER"
sect.outside[is.na(sect.outside)]<-"CI OVERLAP"
inside<-sect.outside=="CI OVERLAP"
coverage.sector<-sum(inside)/length(inside)*100 
coverage.2.sector<-round(coverage.sector, digits=2)       

fit<-ggplot(sector.dat, aes(x=sector.dat$y.mean, y=sector.dat$Predicted))+
  geom_point(x=sector.dat$y.mean, y=sector.dat$Predicted)+
  geom_errorbar(aes(ymin=sector.dat$Lower, ymax=sector.dat$Upper))+
  annotate("text", x=min(sector.dat$y.mean), y=max(sector.dat$Predicted), hjust=0, label=paste("R squared = ", r.sq.2.sector, sep=""))+
  annotate("text", x=min(sector.dat$y.mean), y=max(sector.dat$Predicted)*0.75, hjust=0, label=paste("Coverage = ", coverage.2.sector, "%", sep=""))+
  labs(x="observed", y="predicted")+
  geom_abline(aes(intercept=0, slope=1), colour="black")
pdf(file="model3a_FullModel_predictedVsObserved_sectorLevel.pdf")
print(fit)
dev.off()

################################################################################################
################################################################################################
##### GET OTHER SIMULATED DATA OUTPUTS: y.humansim, y.recovery, y.pctrecov, etc. ###############
################################################################################################
y.recovery.cols<-grep("y.recovery", colnames(z3a.burnin[[1]]))
y.pctrecov.cols<-grep("y.pctrecov", colnames(z3a.burnin[[1]]))
y.humansim.cols<-grep("y.humansim", colnames(z3a.burnin[[1]]))



y.humansim.cols<-grep("y.humansim", colnames(z3a.burnin[[1]]))
z3a.yhumansim<-z3a.burnin[,y.humansim.cols]

# Calculate sector-level biomass baselines (i.e., humans minimized)
quantilesofinterest=c(.17, .25, .5, .75, .83)
allhumansim.quantiles<-as.data.frame(NA)

for(i in 1:length(z3a.yhumansim)){
  
  
  z3a.yhumansim.df<-as.data.frame(z3a.yhumansim[[i]])
  z3a.yhumansim.tf<-t(z3a.yhumansim.df)
  z3a.yhumansim.tf<-as.data.frame(z3a.yhumansim.tf)
  z3a.yhumansim.tf<-cbind(dat.final$SEC_NAME, z3a.yhumansim.tf)
  names(z3a.yhumansim.tf)[1]<-"SC"
  
  
  ## THEN, use "SC" to calculate mean biomass aggregated at the sector level at EACH STEP
  mcmc.yhumansim.sector.mean<-aggregate(as.matrix(z3a.yhumansim.tf[,2:ncol(z3a.yhumansim.tf)]) ~ z3a.yhumansim.tf$SC, data=z3a.yhumansim.tf, FUN=mean)
  
  
  # Each row is now a mcmc of sector-level y.reps: use each row to generate sector-level means and quantiles
  yhumansim.sector.quantiles<-apply(X=mcmc.yhumansim.sector.mean[,-1], MARGIN=1, FUN=quantile, probs=quantilesofinterest)
  colnames(yhumansim.sector.quantiles)<-mcmc.yhumansim.sector.mean[,1]
  yhumansim.sector.quantiles<-t(yhumansim.sector.quantiles)
  yhumansim.sector.quantiles<-as.data.frame(yhumansim.sector.quantiles)
  allhumansim.quantiles<-as.data.frame(cbind(allhumansim.quantiles, yhumansim.sector.quantiles))
}


## AVERAGE ACROSS THE THREE CHAINS

quantilenames<-c("17%", "25%", "50%", "75%", "83%")
firstquantile<-quantilenames[1]
#firstquant.cols<-grep(firstquantile, names(allhumansim.quantiles))
firstquant.cols<-which(names(allhumansim.quantiles)==firstquantile)
allhumansim.firstquant<-as.data.frame(apply(allhumansim.quantiles[,firstquant.cols], MARGIN=1, FUN=mean))
names(allhumansim.firstquant)<-firstquantile

df.humansim.quantiles<-allhumansim.firstquant

quantilelist<-quantilenames[-1]
for(i in 1:length(quantilelist)){
  quant<-quantilelist[i]
  #quant.cols<-grep(quant, names(allhumansim.quantiles))
  quant.cols<-which(names(allhumansim.quantiles)==quant)
  allhumansim.quant<-as.data.frame(apply(allhumansim.quantiles[,quant.cols], MARGIN=1, FUN=mean))
  names(allhumansim.quant)[1]<-quantilelist[i]
  df.humansim.quantiles<-cbind(allhumansim.quant, df.humansim.quantiles)
}

names(df.humansim.quantiles)<-paste("minImpact_", names(df.humansim.quantiles), sep="")

#y.rep.cols<-y.rep.cols[!(dat.final$SEC_NAME=="MAI_HANA"|dat.final$SEC_NAME=="MAI_MOLOKINI")]
z3a.yrep<-z3a.burnin[,y.rep.cols]
#dat.final<-dat.final[!(dat.final$SEC_NAME=="MAI_HANA"|dat.final$SEC_NAME=="MAI_MOLOKINI"),]

# RECALCULATE yreps with same quantiles of interest for comparison
# quantilesofinterest=c(.1, .25, .5, .75, .9) #should already be defined above
allchain.quantiles<-as.data.frame(NA)
for(i in 1:length(z3a.yrep)){
  
  
  z3a.yrep.df<-as.data.frame(z3a.yrep[[i]])
  z3a.yrep.tf<-t(z3a.yrep.df)
  z3a.yrep.tf<-as.data.frame(z3a.yrep.tf)
  z3a.yrep.tf<-cbind(dat.final$SEC_NAME, z3a.yrep.tf)
  names(z3a.yrep.tf)[1]<-"SC"
  
  
  ## THEN, use "SC" to calculate mean biomass aggregated at the sector level at EACH STEP
  mcmc.y.sector.mean<-aggregate(as.matrix(z3a.yrep.tf[,2:ncol(z3a.yrep.tf)]) ~ z3a.yrep.tf$SC, data=z3a.yrep.tf, FUN=mean)
  
  
  # Each row is now a mcmc of sector-level y.reps: use each row to generate sector-level means and quantiles
  y.sector.quantiles<-apply(X=mcmc.y.sector.mean[,-1], MARGIN=1, FUN=quantile, probs=quantilesofinterest)
  colnames(y.sector.quantiles)<-mcmc.y.sector.mean[,1]
  y.sector.quantiles<-t(y.sector.quantiles)
  y.sector.quantiles<-as.data.frame(y.sector.quantiles)
  allchain.quantiles<-as.data.frame(cbind(allchain.quantiles, y.sector.quantiles))
}

## AVERAGE ACROSS THE THREE CHAINS; 
#quantilenames<-c("10%", "25%", "50%", "75%", "90%") # already defined above
#firstquantile<-quantilenames[1] # already defined above
#firstquant.cols<-grep(firstquantile, names(allchain.quantiles))
firstquant.cols<-which(names(allchain.quantiles)==firstquantile)
allchain.firstquant<-as.data.frame(apply(allchain.quantiles[,firstquant.cols], MARGIN=1, FUN=mean))
names(allchain.firstquant)<-firstquantile

df.quantiles<-allchain.firstquant

#quantilelist<-quantilenames[-1] # already defined above 
for(i in 1:length(quantilelist)){
  quant<-quantilelist[i]
  #quant.cols<-grep(quant, names(allchain.quantiles))
  quant.cols<-which(names(allchain.quantiles)==quant)
  allchain.quant<-as.data.frame(apply(allchain.quantiles[,quant.cols], MARGIN=1, FUN=mean))
  names(allchain.quant)<-quantilelist[i]
  df.quantiles<-cbind(allchain.quant, df.quantiles)
}

# SET UP DATA FRAME FOR CREATING GRAPHS of MODELED PRESENT DAY vs HUMANSIM logBIOMASS
row.names(df.humansim.quantiles)<-sects
dat<-cbind(row.names(df.humansim.quantiles), df.humansim.quantiles, df.quantiles)
names(dat)[1]<-"SC"
# EXPORT dat for mapping later
write.csv(dat, file="mappingData_minimalImpactEstimates.csv", quote=FALSE, row.names=FALSE)

names(dat)[c(2,6)]<-c("MinimalImpactUpper", "MinimalImpactLower")
names(dat)[4]<-"MinimalImpact"
names(dat)[c(7,11)]<-c("PresentDayUpper", "PresentDayLower")
names(dat)[9]<-"PresentDay"

#### CALCULATE AND EXPORT DEPLETION LEVELS FOR FUTURE REFERENCE
#### FIRST Backtransform # NOTE These are not part of the MCMC
#### For now, just comparing the MEANS of minimal impact and present day biomass
#### To compare their distributions, add calculation to MCMC
dat.deplete<-dat
dat.deplete[,2:11]<-exp(dat[,2:11])

percentDepletion<-100-(dat.deplete$PresentDay/dat.deplete$MinimalImpact)*100
deplete.table<-as.data.frame(cbind(as.character(dat.deplete$SC), percentDepletion))
names(deplete.table)[1]<-"SC"
setwd("~/Analyses/_RESULTS/fish-stock")
write.csv(deplete.table, file="sectorLevel_percentDepletion.csv", quote=FALSE, row.names=FALSE)


########################################################################
########################################################################
########################################################################
########################################################################
# PLOT BAR GRAPHS OBSERVED vs MODELED log BIOMASS
dat.mean.m<-melt(dat[c("SC", "MinimalImpact", "PresentDay")], id.vars="SC")
dat.upper.m<-melt(dat[c("SC", "MinimalImpactUpper", "PresentDayUpper")], id.vars="SC")
names(dat.upper.m)[3]<-"Upper"
dat.lower.m<-melt(dat[c("SC", "MinimalImpactLower", "PresentDayLower")], id.vars="SC")
names(dat.lower.m)[3]<-"Lower"
dat.graph<-cbind(dat.mean.m, dat.lower.m$Lower, dat.upper.m$Upper)
names(dat.graph)[4:5]<-c("Lower", "Upper")

dat.unordered.bt<-dat.graph
dat.unordered.bt[,3:5]<-exp(dat.graph[,3:5])


### PLOT  UNORDERED GRAPH
p=ggplot(data=dat.graph, aes(x=SC, y=value, fill=variable)) + 
  geom_point(aes(x=SC, y=value, colour=variable), position=position_dodge(width=1))+
  labs(x="Sector", y="log Biomass") +
  scale_color_manual(values=c("red", "blue")) +
  theme(text = element_text(size=22), 
        axis.text.x=element_text(angle=90, hjust=0, vjust=0.5, colour="black", size=10),
        legend.position="bottom", legend.title=element_blank()) +
  geom_errorbar(aes(ymin=dat.graph$Lower, ymax=dat.graph$Upper, colour=variable), position=position_dodge(width=1))
setwd("~/Analyses/_RESULTS/fish-stock")
pdf(file="sectorLevelModeledMinimalImpact_logBiomass-unordered_independentChains_avgQuantiles.pdf")
print(p)
dev.off()

## REORDER AXIS from highest to lowest natural (i.e., minimal impact) fish biomass levels 
sc.order<-dat$SC[order(dat$MinimalImpact, decreasing=FALSE)]
sc.order
sc.index<-order(dat$MinimalImpact, decreasing=FALSE) 
# set order from lowest to highest (i.e., increasing bioamss levels) - later, coords_flip() function corrects this to decreasing biomass levels

# LEVELS should be sorted based on sc.index - GRAPHING order is based on order of levels
dat$SC<-factor(dat$SC, levels(dat$SC)[sc.index])
levels(dat$SC)

# In graphing output - axis.order will be used below to create gray boxes for modeled biomass
# Modified from Ivor's code for PLoS et al. 2015 final figure
# NEED TO GIVE each SECTOR a axis.order number corresponding to its placement within levels(dat$SC)
axis.order <- match(dat$SC, levels(dat$SC))
dat<-cbind(dat, axis.order)

dat.bt<-dat
dat.bt[,2:11]<-exp(dat.bt[,2:11])
# EXPORT backtransformed data for later mapping
write.csv(dat.bt, file="mappingData_minimalImpactEstimates_bt.csv", quote=FALSE, row.names = FALSE)

##### MINIMAL IMPACT (natural) biomass will be represented by GRAY BOXES
natlimits <- aes(xmax = axis.order + 0.45, xmin = axis.order - 0.45, ymax =MinimalImpactUpper, ymin=MinimalImpactLower)#

p<-ggplot(data=dat, aes(x=SC, y=MinimalImpact)) + 
  geom_point(aes(x=SC, y=MinimalImpact), colour="white") +
  geom_rect(natlimits, fill = "steelblue4", alpha=.6) +
  geom_errorbar(aes(ymin=PresentDayLower, ymax=PresentDayUpper), size=1.1, colour="black", width=0.5) +
  geom_point(aes(x=SC, y=PresentDay), shape=23, colour="black", fill="black") +
  geom_point(aes(x=SC, y=MinimalImpact), colour="white") +
  labs(y="log Biomass", "Modeled Present Day vs. Minimal Impact log Biomass", x="") +
  theme(text = element_text(size=18), axis.text.x=element_text(vjust=0.5, colour="black", size=18), 
        legend.position="none") +
  coord_flip()
setwd("~/Analyses/_RESULTS/fish-stock")
pdf(file="sectorLevelModeledMinimalImpact_logBiomass_independentChains_avgQuantiles.pdf")
print(p)
dev.off()

#### PLOT BACKTRANSFORMED predictions


p=ggplot(data=dat.unordered.bt, aes(x=SC, y=value, fill=variable)) + 
  geom_point(aes(x=SC, y=value, colour=variable), position=position_dodge(width=1))+
  labs(x="Sector", y="log Biomass") +
  scale_color_manual(values=c("red", "blue")) +
  theme(text = element_text(size=22), 
        axis.text.x=element_text(angle=90, hjust=0, vjust=0.5, colour="black", size=10),
        legend.position="bottom", legend.title=element_blank()) +
  geom_errorbar(aes(ymin=Lower, ymax=Upper, colour=variable), position=position_dodge(width=1))

setwd("~/Analyses/_RESULTS/fish-stock")
pdf(file="sectorLevelModeledMinimalImpact_backtransBiomass-unordered_independentChains_avgQuantiles.pdf")
print(p)
dev.off()

##### Backtransform MINIMAL IMPACT (natural) biomass
natlimits.bt <- aes(xmax = axis.order + 0.45, xmin = axis.order - 0.45, ymax = MinimalImpactUpper, ymin= MinimalImpactLower)

p<-ggplot(data=dat.bt, aes(x=SC, y=MinimalImpact)) + 
  geom_point(aes(x=SC, y=MinimalImpact), colour="white") +
  geom_rect(natlimits.bt, fill = "steelblue4", alpha=.6) +
  geom_errorbar(aes(ymin=PresentDayLower, ymax=PresentDayUpper), colour="black", size=1.1, width=0.5) +
  geom_point(aes(x=SC, y=PresentDay), shape=23, colour="black", fill="black") +
  geom_point(aes(x=SC, y=MinimalImpact), colour="white") +
  labs(y=expression(paste("Biomass (g",m^{-2}, ")")), x="") +  
  theme_light()+
  theme(text = element_text(size=18), axis.text.x=element_text(vjust=0.5, colour="black", size=18), 
        legend.position="none") +
  coord_flip()
setwd("~/Analyses/_RESULTS/fish-stock")
pdf(file="sectorLevelModeledMinimalImpact_backtransBiomass_independentChains_avgQuantiles.pdf")
print(p)
dev.off()



################################################################################################
################################################################################################
#### CALCULATE SECTOR-LEVEL AVERAGES OF y.humansim - y.reps (i.e., yrecovery) ##################
################################################################################################
################################################################################################

z3a.yrecovery<-z3a.burnin[,y.recovery.cols]

quantilesofinterest=c(.17, .25, .5, .75, .83)
allrecovery.quantiles<-as.data.frame(NA)

for(i in 1:length(z3a.yrecovery)){
  
  
  z3a.yrecovery.df<-as.data.frame(z3a.yrecovery[[i]])
  z3a.yrecovery.tf<-t(z3a.yrecovery.df)
  z3a.yrecovery.tf<-as.data.frame(z3a.yrecovery.tf)
  z3a.yrecovery.tf<-cbind(dat.final$SEC_NAME, z3a.yrecovery.tf)
  names(z3a.yrecovery.tf)[1]<-"SC"
  
  ## THEN, use "SC" to calculate mean biomass aggregated at the sector level at EACH STEP
  mcmc.yrecovery.sector.mean<-aggregate(as.matrix(z3a.yrecovery.tf[,2:ncol(z3a.yrecovery.tf)]) ~ z3a.yrecovery.tf$SC, data=z3a.yrecovery.tf, FUN=mean)

  # Each row is now a mcmc of sector-level y.reps: use each row to generate sector-level means and quantiles
  yrecovery.sector.quantiles<-apply(X=mcmc.yrecovery.sector.mean[,-1], MARGIN=1, FUN=quantile, probs=quantilesofinterest)
  colnames(yrecovery.sector.quantiles)<-mcmc.yrecovery.sector.mean[,1]
  yrecovery.sector.quantiles<-t(yrecovery.sector.quantiles)
  yrecovery.sector.quantiles<-as.data.frame(yrecovery.sector.quantiles)
  allrecovery.quantiles<-as.data.frame(cbind(allrecovery.quantiles, yrecovery.sector.quantiles))
}

## AVERAGE ACROSS THE THREE CHAINS

quantilenames<-c("17%", "25%", "50%", "75%", "83%")
firstquantile<-quantilenames[1]
#firstquant.cols<-grep(firstquantile, names(allrecovery.quantiles))
firstquant.cols<-which(names(allrecovery.quantiles)==firstquantile)

allrecovery.firstquant<-as.data.frame(apply(allrecovery.quantiles[,firstquant.cols], MARGIN=1, FUN=mean))
names(allrecovery.firstquant)<-firstquantile

df.recovery.quantiles<-allrecovery.firstquant

quantilelist<-quantilenames[-1]
for(i in 1:length(quantilelist)){
  quant<-quantilelist[i]
  #quant.cols<-grep(quant, names(allrecovery.quantiles))
  quant.cols<-which(names(allrecovery.quantiles)==quant)
  allrecovery.quant<-as.data.frame(apply(allrecovery.quantiles[,quant.cols], MARGIN=1, FUN=mean))
  names(allrecovery.quant)[1]<-quantilelist[i]
  df.recovery.quantiles<-cbind(allrecovery.quant, df.recovery.quantiles)
}

names(df.recovery.quantiles)<-paste("recovery_", names(df.recovery.quantiles), sep="")

# PLOT BAR GRAPHS of BIOMASS RECOVERY POTENTIAL
row.names(df.recovery.quantiles)<-sects
dat<-cbind(row.names(df.recovery.quantiles), df.recovery.quantiles)
names(dat)[1]<-"SC"

# EXPORT dat for mapping later
write.csv(dat, file = "mappingData_recoveryEstimates.csv", quote=FALSE, row.names=FALSE)


## REORDER AXIS from highest to lowest natural (i.e., minimal impact) fish biomass levels 
sc.order<-dat$SC[order(dat$'recovery_50%', decreasing=FALSE)]
sc.order
sc.index<-order(dat$'recovery_50%', decreasing=FALSE) 
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
setwd("~/Analyses/_RESULTS/fish-stock")
pdf(file="sectorLevelRecoveryPotential_independentChains_avgQuantiles.pdf")
print(p)
dev.off()


################################################################################################
################################################################################################
#### CALCULATE SECTOR-LEVEL AVERAGES OF percent recovery (i.e., y.pctrecov) ####################
################################################################################################
################################################################################################

z3a.ypctrecov<-z3a.burnin[,y.pctrecov.cols]

### AS ABOVE, calculate quantiles separately for EACH chain, and then average the quantiles
## ADJUST QUANTILES OF INTEREST and QNORM for y.mean (i.e. 95% vs 80% Confidence Intervals)

quantilesofinterest=c(.17, .25, .5, .75, .83)
allpctrecovery.quantiles<-as.data.frame(NA)

for(i in 1:length(z3a.ypctrecov)){
  
  
  z3a.ypctrecov.df<-as.data.frame(z3a.ypctrecov[[i]])
  z3a.ypctrecov.tf<-t(z3a.ypctrecov.df)
  z3a.ypctrecov.tf<-as.data.frame(z3a.ypctrecov.tf)
  z3a.ypctrecov.tf<-cbind(dat.final$SEC_NAME, z3a.ypctrecov.tf)
  names(z3a.ypctrecov.tf)[1]<-"SC"
  
  
  ## THEN, use "SC" to calculate mean biomass aggregated at the sector level at EACH STEP
  mcmc.ypctrecov.sector.mean<-aggregate(as.matrix(z3a.ypctrecov.tf[,2:ncol(z3a.ypctrecov.tf)]) ~ z3a.ypctrecov.tf$SC, data=z3a.ypctrecov.tf, FUN=mean)
  
  
  # Each row is now a mcmc of sector-level y.reps: use each row to generate sector-level means and quantiles
  ypctrecov.sector.quantiles<-apply(X=mcmc.ypctrecov.sector.mean[,-1], MARGIN=1, FUN=quantile, probs=quantilesofinterest)
  colnames(ypctrecov.sector.quantiles)<-mcmc.ypctrecov.sector.mean[,1]
  ypctrecov.sector.quantiles<-t(ypctrecov.sector.quantiles)
  ypctrecov.sector.quantiles<-as.data.frame(ypctrecov.sector.quantiles)
  allpctrecovery.quantiles<-as.data.frame(cbind(allpctrecovery.quantiles, ypctrecov.sector.quantiles))
}


## AVERAGE ACROSS THE THREE CHAINS

quantilenames<-c("17%", "25%", "50%", "75%", "83%")
firstquantile<-quantilenames[1]
#firstquant.cols<-grep(firstquantile, names(allrecovery.quantiles))
firstquant.cols<-which(names(allpctrecovery.quantiles)==firstquantile)

allpctrecovery.firstquant<-as.data.frame(apply(allpctrecovery.quantiles[,firstquant.cols], MARGIN=1, FUN=mean))
names(allpctrecovery.firstquant)<-firstquantile

df.pctrecovery.quantiles<-allpctrecovery.firstquant

quantilelist<-quantilenames[-1]
for(i in 1:length(quantilelist)){
  quant<-quantilelist[i]
  #quant.cols<-grep(quant, names(allrecovery.quantiles))
  quant.cols<-which(names(allpctrecovery.quantiles)==quant)
  allpctrecovery.quant<-as.data.frame(apply(allpctrecovery.quantiles[,quant.cols], MARGIN=1, FUN=mean))
  names(allpctrecovery.quant)[1]<-quantilelist[i]
  df.pctrecovery.quantiles<-cbind(allpctrecovery.quant, df.pctrecovery.quantiles)
}

names(df.pctrecovery.quantiles)<-paste("pctrecovery_", names(df.pctrecovery.quantiles), sep="")

# PLOT BAR GRAPHS of PERCENT RECOVERY POTENTIAL
row.names(df.pctrecovery.quantiles)<-sects
dat<-cbind(row.names(df.pctrecovery.quantiles), df.pctrecovery.quantiles)
names(dat)[1]<-"SC"
names(dat)[2]<-"Upper"
names(dat)[6]<-"Lower"
names(dat)[4]<-"Mid"

# EXPORT dat for mapping later
write.csv(dat, file = "mappingData_percentRecoveryEstimates.csv", quote=FALSE, row.names=FALSE)

### PLOT  UNORDERED GRAPH
p=ggplot(data=dat, aes(x=SC, y=Mid)) + 
  geom_point(aes(x=SC, y=Mid))+
  labs(x="Sector", y="percent recovery") +
  theme(text = element_text(size=22), 
        axis.text.x=element_text(angle=90, hjust=0, vjust=0.5, colour="black", size=10),
        legend.position="bottom", legend.title=element_blank()) +
  geom_errorbar(aes(ymin=dat$Lower, ymax=dat$Upper))
setwd("~/Analyses/_RESULTS/fish-stock")
pdf(file="sectorLevelPercentRecoveryPotential-unordered_independentChains_avgQuantiles.pdf")
print(p)
dev.off()



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
setwd("~/Analyses/_RESULTS/fish-stock")
pdf(file="sectorLevelPercentRecoveryPotential_independentChains_avgQuantiles.pdf")
print(p)
dev.off()


#SAVE DATA
#rm(z3a.burnin) # Throughout script above, z3a.burnin is parsed into z3a.pars, z3a.sim, z3a.yhumansim, etc - i.e., saving z3a.burnin is unecessary
save.image(file="_bayesianSavedWorkspace_forReDoingOutputs.RData")


