rm(list=ls())
library(reshape) # needed for melt()
library(rjags) 
library(xtable) 
library(coda)
library(gdata)    # needed for drop.levels()
library(car)      # needed for Variance Inflation Factor calculation
library(ggplot2)

##### CHANGE RESPONSE VARIABLE AS NEEDED: "PRIMARY" VS "INSTTotFishMinusSJRTB"

setwd("~/Analyses/fish-stock")
load("~/Analyses/fish-stock/input/TMPwsd_MHI_AllHerbivoresThru2016.RData") # new correct dataframe
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

### MERGE with fish HERBIVORE data
sub.dat<-subset(ocean.dat, select=-c(LATITUDE, LONGITUDE, DATE_, ANALYSIS_SEC, ANALYSIS_STRATA, ANALYSIS_YEAR, SEC_NAME,
                                     METHOD, OBS_YEAR, REGION, REGION_NAME, ISLAND, REEF_ZONE, DEPTH_BIN, HABITAT_CODE,
                                     DEPTH, HARD_CORAL, MA, CCA, SAND, OTHER_BENTHIC, MEAN_SH, SD_SH_DIFF, MAX_HEIGHT,
                                     VISIBILITY, nCounts, nReps))

### All data including post-bleaching 2016 data
common<-c("SITEVISITID", "SITE")
alldat<-merge(mhi, sub.dat, by=common)

### Only the study data - 2012 thru 2016
study.dat<-subset(alldat, OBS_YEAR<2016)

# Join with Humans and Site Data
siteDat<-read.csv("~/Analyses/fish-stock/input/SITE MASTER2016.csv")

#When merging, don't merge by date, latitude, or longitude. 
#Slight differences in the object class for "DATE_" and number of decimal numbers for "LAT" and "LONG"
sub.dat<-subset(study.dat, select=-c(DATE_, LATITUDE, LONGITUDE, ANALYSIS_STRATA, ANALYSIS_YEAR, SEC_NAME))
common<-c("SITE", "SITEVISITID", "REGION", "ISLAND", "REEF_ZONE", "OBS_YEAR", "DEPTH_BIN", "ANALYSIS_SEC")
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
benthic<-subset(benthic, select=-c(DATE_, LATITUDE, LONGITUDE))
common2<-c("SITE", "REGION", "ISLAND", "REEF_ZONE", "OBS_YEAR", "DEPTH_BIN")
dat<-merge(dat, benthic, by=common2)
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

fish<-"PRIMARY"
#fish<-"INSTTotFishMinusSJRTB"


######################################### Select response column
#indicator <- "Site-level Average Length"
indicator <- "Site-level Biomass"
focus <- paste("log",fish,sep="")
#focus <- paste("cuberoot_log",fish,sep="")
response <- paste("log (", fish, " biomass)", sep="")


##########################################TRANSFORMATIONS
# Log Fish: Could not get non-logged data to converge

### FIRST, FOR HERBIVORES ANALYSIS (i.e., fish<-"PRIMARY"), there are four sites with zeroes, remove these
### OTHERWISE they create NA terms (symbolized by "-Inf")
dat<-dat[dat[,fish]>0,]

# now log
dat <- cbind(dat, log(dat[,fish])) # No longer need + 1 since this isn't a zero-inflated model
names(dat)[grep("fish", names(dat))]<-paste("log",fish,sep="")


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
setwd("~/Analyses/fish-stock/RESULTS")

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


# Select all site ID and hierarchical ID information
xa <- c("LATITUDE", "LONGITUDE", "SITE", "SITEVISITID", "REGION", "ISLAND", "REEF_ZONE", "HABITAT_CODE", "OBS_YEAR", "DEPTH_BIN", "SEC_NAME", "ANALYSIS_STRATA", "ANALYSIS_SEC", "METHOD")

dat.scatter<-cbind(dat[xa], dat[focus], dat[covars])
dat.scatter<-na.omit(dat.scatter)

y <- dat.scatter[[focus]]
## Use double brackets to ensure that y is returned as a vector (not a data frame column)

scatter.final<-as.data.frame(cbind(y, dat.scatter[covars], dat.scatter$ISLAND))
names(scatter.final)[length(scatter.final)]<-"ISLAND"

setwd("~/Analyses/fish-stock/RESULTS")


# Graph ONLY COVARS IN FINAL MODEL
xi <- c("Total.Coral", "Total.sqCoral", 
        "SED_Sediment","CCA_Coralline.Alga",
        "DEPTH", "MEAN_SH", "VISIBILITY",
        "MEAN_SST_MEAN",
        "MEAN_Waves_MEAN", "meanWAVESsq",
        "logHUMANS20"
)


finalcovar<-xi

# Select all site ID and hierarchical ID information
xa <- c("LATITUDE", "LONGITUDE", "SITE", "SITEVISITID", "REGION", "ISLAND", "REEF_ZONE", "OBS_YEAR", "DEPTH_BIN", "SEC_NAME", "ANALYSIS_STRATA", "ANALYSIS_SEC", "METHOD", "DATE_")
#xa <- c("LATITUDE", "LONGITUDE", "SITE", "SITEVISITID", "REGION", "ISLAND", "REEF_ZONE", "OBS_YEAR", "DEPTH_BIN", "SEC_NAME", "ANALYSIS_STRATA", "ANALYSIS_SEC", "METHOD")


### REMOVE ALL NA Data
dat.final<-cbind(dat[xa], dat[focus], dat[finalcovar])
dat.final<-na.omit(dat.final)


# NO SCALING OR CENTERING:
# NO INTERACTIONS - no need to plot these
# NOW PLOT: 

loc <- file.path(tempdir(), "stats_dat")
unzip(system.file("extdata", "states_21basic.zip", package = "fiftystater"),
      exdir = loc)
fifty_states_sp <- readOGR(dsn = loc, layer = "states", verbose = FALSE) 
hawaii <- fifty_states_sp[fifty_states_sp$STATE_NAME == "Hawaii", ]

fishcol<-grep(focus, names(dat.final))
names(dat.final)[fishcol]<-"logBiomass"

names(dat.final)[16:26]<-c("Coral", "CoralxCoral", "Sand", "CCA",
           "Depth", "Complexity", "Visibility", "SST", "Waves", "WavesxWaves", "Human Density")


for(i in fishcol:length(dat.final))
{
p<-ggplot(data=dat.final, aes(x=LONGITUDE, y=LATITUDE, color=dat.final[i])) + 
  geom_polygon(data=hawaii, aes(x=long, y=lat, group=group), fill="grey40", colour="grey90") +
  geom_point(alpha=0.2) +
  coord_equal(ratio=1) +
  labs(x="Long", y="Lat", fill=names(dat.final)[i]) +
  theme_light()+
  theme(legend.position="bottom")+
  scale_colour_gradientn(names(dat.final)[i], colours = c("red", "blue"))  
  #scale_fill_distiller(palette="OrRd", direction=1)

mapname<-paste("mapdat_", names(dat.final)[i], ".pdf", sep="")
pdf(mapname)
print(p)
dev.off()
}



