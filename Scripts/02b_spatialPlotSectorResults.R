# MAP RESULTS TO SECTOR MAP
# Following: http://spatioanalytics.com/

################################################################
################################################################
################################################################ 
#### start by just plotting just the shape file
#### METHOD 1: Combine google map + shapefile
rm(list=ls())
library(ggplot2)
library(sp)
library(rgdal)
library(rgeos)
library(ggthemes)
library(ggmap)

# Get base map
library(RColorBrewer)
colors <- brewer.pal(9, "BuGn")
setwd("~/Analyses/fish-stock/input/shapefiles_Sectors")
mapCenter<-geocode("21.184738, -157.180487") # LAT/LONG of western Molokai taken from GoogleMaps
hawaiiMap<-get_map(c(lon=mapCenter$lon, lat=mapCenter$lat), zoom=7, maptype="terrain", source="google")


# Get shapefile
library(rgdal)
setwd("~/Analyses/fish-stock/input/shapefiles_Sectors")
####library(maptools)
####sectorShape <- readShapePoly("MHI_Sectors.shp") # recommended that we use readOGR from rgdal package instead of readShapePoly from maptools package
sectorShape<-readOGR(".","MHI_Sectors")
#sectorShape<-sectorShape



sectorShape<-spTransform(sectorShape, CRS("+proj=longlat +datum=WGS84")) #specify mapping projection #this particular projection is used by both GoogleMaps and CRED maps
sectorDf<-fortify(sectorShape)

# Combine:
sectorMap<-ggmap(hawaiiMap)+
  geom_polygon(aes(x=long, y=lat, group=group), 
               data=sectorDf, color=colors[9], fill=colors[6], alpha=0.5)+
  labs(x="Longitude", y="Latitude")


setwd("~/Analyses/fish-stock/RESULTS")
mapfilename<-paste("justSectors.pdf", sep="")
pdf(file=mapfilename)
print(sectorMap)
dev.off()

################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
# METHOD 2: Plot the shapefile with labels
################################################################
rm(list=ls())
library(ggplot2)
library(sp)
library(rgdal)
library(rgeos)
library(ggthemes)
library(gridExtra)


# Get shapefile
setwd("~/Analyses/fish-stock/input/shapefiles_Sectors")
#library(maptools)
#sectorShape <- readShapePoly("MHI_Sectors.shp") # recommended that we use readOGR from rgdal package instead of readShapePoly from maptools package
sectorShape<-readOGR(".","MHI_Sectors")
sectorShape<-spTransform(sectorShape, CRS("+proj=longlat +datum=WGS84"))

map.sects<-c("Hawaii-Hamakua", "Hawaii-Puna", "Hawaii-Kona",
             "Kauai-East", "Kauai-Na Pali",
             "Lanai-North", "Lanai-South",
             "Maui-Hana", "Maui-Kahului", "Maui-Kihei", "Maui-Lahaina", "Maui-Northeast", "Maui-Northwest", "Maui-Southeast", 
             "Molokai-Northwest", "Molokai-Pali", "Molokai-South", "Molokai-West",
             "Niihau-East", "Niihau-Lehua", "Niihau-West",
             "Oahu-East", "Oahu-Kaena", "Oahu-Northeast", "Oahu-Northwest", "Oahu-South",
             "Hawaii-Southeast")
# PREPARE SHAPEFILE FOR PLOTTING IN ggplot2
# add an extra column "id" to the data slot that match the polygons associated with this data
# Do this before merging anything else with the data slot as merge will change the row order
sectorShape@data$id<-rownames(sectorShape@data) 
sectorShape@data$SEC_NAME<-map.sects
#sectorShape<-spTransform(sectorShape, CRS("+proj=longlat +datum=WGS84")) #specify mapping projection #this particular projection is used by both GoogleMaps and CRED maps
#sectorDf<-fortify(sectorShape)

# Read in figure1INPUT.csv
seclabs<-read.csv("~/Analyses/fish-stock/input/figure1Input.csv")
sectorShape@data<-merge(x=sectorShape@data, y=seclabs, by="SEC_NAME", all.x=TRUE)
sectorPoints<-fortify(sectorShape)
sectorDf<-merge(sectorPoints, sectorShape@data, by="id")

#ggSectors<-ggplot(data=sectorDf, aes(x=long, y=lat))+
#geom_polygon(aes(x=long, y=lat, group=group), data=sectorDf, fill="white")+
ggSectors<-ggplot(data=sectorDf)+
  coord_equal()+ 
  geom_path(color="grey20", aes(x=long, y=lat, group=group))+
  theme_light()+
  theme(text = element_text(size=22),
        legend.position="none",
        axis.text=element_text(size=18))+
  labs(x="Longitude", y="Latitude")+
  geom_text(data=seclabs, aes(x=labelLong, y=labelLat, label=label), size=5, color="black")


#setwd("~/Analyses/fish-stock/RESULTS")
#pdf(file="justSectors.pdf")
#print(ggSectors)
#dev.off()
# white colors below are the non-MauiNui islands from levels(sectorDf$island)
ggMauiNui<-ggplot(data=sectorDf, aes(color=island))+
  scale_colour_manual(values=c("white", "white", "darkorange", "green4", "royalblue1", "white", "white"))+
  coord_equal()+ 
  coord_map(xlim=c(-157.5, -155.8), ylim=c(20.4, 21.4))+
  geom_path(aes(x=long, y=lat, group=group))+
  theme_light()+
  theme(legend.position="none",
        axis.text=element_blank())+
  labs(x="", y="")+
  geom_text(data=seclabs, size=5, aes(x=labelLong, y=labelLat, label=inset))
  
# ORIGINAL
#ggMauiNui<-ggplot(data=sectorDf)+
#  coord_equal()+ 
#  coord_map(xlim=c(-157.5, -155.8), ylim=c(20.4, 21.4))+
#  geom_path(color="grey20", aes(x=long, y=lat, group=group))+
#  theme_light()+
#  theme(legend.position="none",
#        axis.text=element_text(size=8))+
#  labs(x="", y="")+
#  geom_text(data=seclabs, aes(x=labelLong, y=labelLat, label=inset), color="black", size=8)


#setwd("~/Analyses/fish-stock/RESULTS")
#pdf(file="justMauiNui-color.pdf")
#print(ggMauiNui)
#dev.off()

## Following http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/#insert-a-graphical-element-inside-a-ggplot
#ggSectors_grob<-ggplotGrob(ggSectors)
ggMauiNui_grob<-ggplotGrob(ggMauiNui)

setwd("~/Analyses/RESULTS/fish-stock")
pdf(file="Figure1.pdf")
ggSectors + 
  annotation_custom(grob=ggMauiNui_grob,
                    xmin=-160.7, xmax=-157.2,
                    ymin=18.6, ymax=21)
dev.off()



################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
## MAPPING minimum impact and recovery potential RESULTS (color ramp)
## if you want to manipulate the polygons then need to prepare the shapefile data first
## FOLLOWING: http://mazamascience.com/WorkingWithData/?p=1494

# Get base map
#library(RColorBrewer)
#colors <- brewer.pal(9, "BuGn")
#setwd("~/Analyses/fish-stock/input/shapefiles_Sectors")
#mapCenter<-geocode("21.184738, -157.180487") # LAT/LONG of western Molokai taken from GoogleMaps
#hawaiiMap<-get_map(c(lon=mapCenter$lon, lat=mapCenter$lat), zoom=7, maptype="terrain", source="google")

rm(list=ls())
library(ggplot2)
library(sp)
library(rgdal)
library(rgeos)
library(ggthemes)

## SETTINGS:
mapinput<-"mappingData_recoveryEstimates-Herbivores.csv"
fishtitle<-"Herbivores" #Total Reef Fish  OR   #Herbivores
legendtitle<-"Absolute Recovery Potential"      #"Percent Recovery Potential"  OR #"Biomass Baseline"  OR #Absolute Recovery Potential
# name of variable column in input file 
# manually change in excel to avoid 'minImpact_50%' - weird symbols like %
variablename<-"Mid"
mapoutput<-paste("absRecoveryMap-Herbivores.pdf", sep="")



# Get shapefile
setwd("~/Analyses/fish-stock/input/shapefiles_Sectors")
#library(maptools)
#sectorShape <- readShapePoly("MHI_Sectors.shp") # recommended that we use readOGR from rgdal package instead of readShapePoly from maptools package
sectorShape<-readOGR(".","MHI_Sectors")
sectorShape<-spTransform(sectorShape, CRS("+proj=longlat +datum=WGS84"))



# Get mapping data
setwd("~/Analyses/fish-stock/input")
mapdat<-read.csv(mapinput)
# ADD NA to Maui-Hana polygon - not reporting this sector in RESULTS
#mapdat[mapdat$SC=="Maui-Hana",]<-NA

# CHANGE SEC_NAME column in sectorShape@data to match sects in Analysis
# ORDER of map.sects should match order of sectorShape@data$SEC_NAME: confirmed by plotting "sectorMap_colorCodedOBJECTID.pdf"
# USE THIS TO CONFIRM SECTOR NAMES
map.sects<-c("Hawaii-Hamakua", "Hawaii-Puna", "Hawaii-Kona",
             "Kauai-East", "Kauai-Na Pali",
             "Lanai-North", "Lanai-South",
             "Maui-Hana", "Maui-Kahului", "Maui-Kihei", "Maui-Lahaina", "Maui-Northeast", "Maui-Northwest", "Maui-Southeast", 
             "Molokai-Northwest", "Molokai-Pali", "Molokai-South", "Molokai-West",
             "Niihau-East", "Niihau-Lehua", "Niihau-West",
             "Oahu-East", "Oahu-Kaena", "Oahu-Northeast", "Oahu-Northwest", "Oahu-South",
             "Hawaii-Southeast")
# PREPARE SHAPEFILE FOR PLOTTING IN ggplot2
# add an extra column "id" to the data slot that match the polygons associated with this data
# Do this before merging anything else with the data slot as merge will change the row order
sectorShape@data$id<-rownames(sectorShape@data) 
sectorShape@data$SEC_NAME<-map.sects


## Add mapping data - merge based on SEC_NAME
names(mapdat)[1]<-"SEC_NAME"
sectorShape@data<-merge(x=sectorShape@data, y=mapdat, by="SEC_NAME", all.x=TRUE)




sectorPoints<-fortify(sectorShape) # fortify transforms this into a data.frame;
sectorDf<-merge(sectorPoints, sectorShape@data, by="id") #now merge by "id"


#### FINALLY, get base map
#### OPTION 1: not ideal - base map projection is slightly off - looks weird when overlaying sector shapefile
#loc <- file.path(tempdir(), "stats_dat")
#unzip(system.file("extdata", "states_21basic.zip", package = "fiftystater"),
#      exdir = loc)
#fifty_states_sp <- readOGR(dsn = loc, layer = "states", verbose = FALSE) 
#fifty_states_sp<-spTransform(fifty_states_sp, CRS("+proj=longlat +datum=WGS84"))
#hawaii <- fifty_states_sp[fifty_states_sp$STATE_NAME == "Hawaii", ]
#ggSectors<-ggplot(data=sectorDf, aes(x=long, y=lat, group=group))+
#  geom_polygon(data=hawaii, aes(x=long, y=lat, group=group), fill="grey40", colour="grey90") +
#  geom_polygon(aes(fill=MinimalImpact), color=NA)+
#  geom_path(color="black")+
#  scale_fill_distiller(palette="YlOrRd")+
#  coord_equal()+ 
#  theme(legend.position="bottom", 
#    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#    axis.ticks.y = element_blank(), axis.text.y = element_blank(),
#    axis.ticks.x = element_blank(), axis.text.x = element_blank())+
#  labs(fill="Minimal Impact Biomass", x="", y="")


#### OPTION 2: Google Maps base map
#mapCenter<-geocode("21.184738, -157.180487") # LAT/LONG of western Molokai taken from GoogleMaps
#hawaii<-get_map(c(lon=mapCenter$lon, lat=mapCenter$lat), zoom=7, maptype="terrain", source="google")
## OPTION 2B: get stamen watercolor map
#hawaii<-get_map(c(lon=mapCenter$lon, lat=mapCenter$lat), zoom=7, maptype="watercolor", source="stamen")
#hawaii<-spTransform(hawaii, CRS("+proj=longlat +datum=WGS84"))
#hawaiiMap<-ggmap(hawaii)

#ggSectors<-hawaiiMap+
#  geom_polygon(aes(x=long, y=lat, group=group, fill=MinimalImpact), data=sectorDf, color=NA)+
#  scale_fill_distiller(palette="YlOrRd")+
#  coord_equal()+
#  geom_path(color="black")+
#  theme(legend.position="bottom", 
#        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
#        axis.ticks.x = element_blank(), axis.text.x = element_blank())+
#  labs(fill="Minimal Impact Biomass", x="", y="")


#### OPTION 3: NO BASE MAP
#http://ggplot2.tidyverse.org/reference/scale_brewer.html
#scale_fill_distiller(palette="Blues")+
#scale_fill_distiller(palette="YlOrRd", direction=1)+
#scale_colour_gradient2(low="red", mid="yellow", high="green")+
#scale_fill_distiller(palette="PuBuGn", direction=1, na.value="white")+

# COLOR SCHEMES for DIFFERENT RESULTS:
#USE FOR BASELINE BIOMASS: scale_fill_distiller(palette="Blues", direction=1, na.value="white")+
#USE FOR PERCENT RECOVERY POTENTIAL: scale_fill_gradient(low="yellow", high="darkgreen", na.value="white")+
#USE FOR (absolute) RECOVERY POTENTIAL: scale_fill_distiller(palette="Purples", direction=1, na.value="white")+
ggSectors<-ggplot(data=sectorDf, aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill=sectorDf[variablename]), color=NA, size=10)+
  scale_fill_distiller(palette="Purples", direction=1, na.value="white")+
  coord_equal()+ 
  geom_path(color="grey20")+
  theme_light()+
  theme(text = element_text(size=22), 
        legend.position="bottom", 
        axis.text.x=element_text(size=18), 
        axis.text.y=element_text(size=18), 
        legend.text=element_text(size=8),
        legend.title=element_text(size=14))+
  labs(fill=legendtitle, x="Longitude", y="Latitude", title=fishtitle)

setwd("~/Analyses/RESULTS/fish-stock")
pdf(file=mapoutput)
print(ggSectors)
dev.off()


#OBJECTID SEC_NAME(shapefile) SEC_NAME(analysis)
#1 HAW_HAMAKUA Hawaii-Hamakua
#2 PUNA Hawaii-Puna
#3 KONA Hawaii-Kona
#4 EAST Kauai-East
#5 NAPALI Kauai-Napali
#6 NORTH Lanai-North
#7 SOUTH Lanai-South
#8 HANA Maui-Hana
#9 KAHULUI Maui-Kahului
#10 KIHEI Maui-Kihei
#11 LAHAINA Maui-Lahaina
#12 NE Maui-NE
#13 NW Maui-NW
#14 SE Maui-SE???? (NOT IN ANALYSIS)
#15 NW Molokai-NW
#16 PALI Molokai-Pali
#17 SOUTH Molokai-S
#18 WEST Molokai-W
#19 EAST Niihau-E
#20 LEHUA Niihau-Lehua
#21 WEST Niihau-West
#22 EAST Oahu-East
#23 KAENA Oahu-Kaena
#24 NE Oahu-NE
#25 NORTH Oahu-NW
#26 SOUTH Oahu-S
#27 SE Hawaii-SE


