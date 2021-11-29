library(httr)
library(jsonlite)
library(dplyr, quietly=T)
library(devtools)
library (stringr)
#install_github('NEONScience/NEON-geolocation/geoNEON', dependencies=TRUE)
library(geoNEON)

setwd("//marthathegreat/SAL/NEON/NEON_Biodiversity/Data/makeup")

# Request wvps data using the GET function & the API call

req <- GET("http://data.neonscience.org/api/v0/products/DP1.10098.001")
req

#View Requested data 
req.content <- content(req, as="parsed")
req.content

# Get data availabilityt list for the product 
req.text <- content(req, as="text")
avail <- fromJSON(req.text, simplifyDataFrame=T, flatten=T)
avail

#Get wvps data avaliabitlity list
wvps.urls <- unlist(avail$data$siteCodes$availableDataUrls)
wvps.urls
#write.csv(wvps.urls,'./wvps_availability.csv')

#Get the data availability for a specific site based on the avaliability list 
wvps <- GET(wvps.urls[grep("SERC/2017-11", wvps.urls)])
wvps.files <- fromJSON(content(wvps, as="text"))
wvps.files$data$files

#Get the data from "vst_mappingandtagging" 
wvps.mapandtag <- read.delim(wvps.files$data$files$url
                             [intersect(grep("vst_mappingandtagging", 
                                             wvps.files$data$files$name),
                                        grep("basic", 
                                             wvps.files$data$files$name))], 
                             sep=",")
wvps.mapandtag <-data.frame(wvps.mapandtag)
wvps.mapandtag<-def.calc.geo.os(wvps.mapandtag,"vst_mappingandtagging")


head(wvps.mapandtag)

#Get the data from "vst_apparentindividual"
wvps.individual <- read.delim(wvps.files$data$files$url
                              [intersect(grep("vst_apparentindividual", 
                                              wvps.files$data$files$name),
                                         grep("basic", 
                                              wvps.files$data$files$name))], 
                              sep=",")
wvps.individual <-data.frame(wvps.individual)
#wvps.individual <- dplyr::select(wvps.individual, -c(uid))
head(wvps.individual)



##keep only the alive trees Alive = "Live", "Live, other damage", "Live, disease damaged", "Live, insect damaged", "Live, Physically damaged"
#delete all of the followingrows: Dea, no longer qualifies, lost, removed, standing dead, dead, broken bole
wvps.individual <- dplyr::filter(wvps.individual, plantStatus == "Live" | plantStatus == "Live, other damage" | plantStatus == "Live, disease damaged"| plantStatus == "Live, insect damaged"| plantStatus == "Live, physically damaged")

wvps.join <- merge(wvps.individual, wvps.mapandtag, by = "individualID",all=FALSE)
wvps.join <- unique(wvps.join)

## Create year and month column 
wvps.join$year <- substr(wvps.join$date.x, start = 1, stop = 4)
wvps.join$month <- substr(wvps.join$date.x,start = 6, stop = 7 )

####Diversity indices####

##calculate at the site scale

#alpha diversity = number species in each plot
#gamma diversity = number of species found in all plots across the site
#beta diversity = gamma/(mean alpha)
sitePlotAlphaDiv <- wvps.join %>%
  select(siteID.x, plotID.x, year, taxonID) %>%
  unique() %>%
  group_by(siteID.x, year, plotID.x) %>%
  summarise(plotSpeciesRichness = n()) %>%
  summarise(meanSiteAlphaDiversity = mean(plotSpeciesRichness)) %>%
  ungroup()

siteGammaDiv <- wvps.join %>%
  select(siteID.x, year, taxonID) %>%
  unique() %>%
  group_by(siteID.x, year) %>%
  summarise(siteGammaDiversity = n())%>%
  ungroup()

#combine for site beta diversity
siteDiversity <- dplyr::full_join(sitePlotAlphaDiv, siteGammaDiv, by = c("siteID.x", "year")) 
siteDiversity$siteGammaDiversity <- as.numeric(siteDiversity$siteGammaDiversity)
siteDiversity$siteBetaDiversity <- siteDiversity$siteGammaDiversity/siteDiversity$meanSiteAlphaDiversity
#write the file to working directory

write.table(siteDiversity, "./SERC_wvps_siteDiversity_2017_11.csv", sep = ',', col.names = TRUE, row.names = F, quote = T, na = '')

##calculate at the plot scale
#plot apha diversity
plotAlphaDiv <- wvps.join %>%
  select(siteID.x, plotID.x, subplotID.y, year, taxonID) %>%
  unique() %>%
  group_by(siteID.x, year, plotID.x, subplotID.y) %>%
  summarise(subplotRichness = n()) %>%
  ungroup %>%
  group_by(siteID.x, year, plotID.x) %>%
  summarise(plotAlphaDiversity = mean(subplotRichness)) %>%
  ungroup()


#plot gamma diversity
plotGammaDiv <- wvps.join %>%
  select(siteID.x, plotID.x, year, taxonID) %>%
  unique() %>%
  group_by(siteID.x, year,plotID.x) %>%
  summarise(plotGammaDiversity = n())

#combine for plot gamma diversity
plotDiversity <- dplyr::full_join(plotAlphaDiv, plotGammaDiv, by = c("siteID.x", "year", "plotID.x"))
plotDiversity$plotGammaDiversity <- as.numeric(plotDiversity$plotGammaDiversity)
plotDiversity$plotBetaDiversity <- plotDiversity$plotGammaDiversity/plotDiversity$plotAlphaDiversity

#write the file to working directory
write.table(plotDiversity, "./SERC_wvps_plotDiversity_2017_11.csv",  
            sep = ',', col.names = TRUE, row.names = F, quote = T, na = '')



### calculate wvps_sppRichness 
plotSpeciesRichness <- wvps.join %>%
  select(siteID.x,plotID.x,year,taxonID) %>%
  unique()%>%
  group_by(siteID.x,year,plotID.x) %>%
  summarise(plotSpeciesRichness = n())%>%
  ungroup()


## calculate grandtotal of each taxonId per plot
plotGrandtotal<- wvps.join %>%
  select(siteID.x,plotID.x,year,taxonID) %>%
  group_by(year,plotID.x,taxonID) %>%
  add_count(taxonID)%>%
  unique() %>%
  group_by(plotID.x)%>%
  add_tally(n, name = "Grandtotal")%>%
  unique()%>%
  ungroup()

SimpsonDiversity_full <- dplyr::full_join(plotGrandtotal, plotSpeciesRichness, by = c("siteID.x", "year", "plotID.x"))
str(SimpsonDiversity_full)

SimpsonDiversity_full <- SimpsonDiversity_full %>%
  mutate(square = (SimpsonDiversity_full$n/SimpsonDiversity_full$Grandtotal)^2)

SimpsonDiversity_full <- SimpsonDiversity_full %>%
  group_by(plotID.x) %>%
  add_tally(square, name = "D") %>%
  ungroup()

SimpsonDiversity_full <- SimpsonDiversity_full %>%
  mutate(SimpsonD = 1-SimpsonDiversity_full$D)


### Site SimpsonDiversity 
SimpsonDiversity <- dplyr::select(SimpsonDiversity_full, -c(taxonID,n,square))
SimpsonDiversity <- unique(SimpsonDiversity)

wvps.join <- dplyr::inner_join(SimpsonDiversity, wvps.join, by = "plotID.x")

write.table(SimpsonDiversity_full, "./SERC_simpsonDiversity_plot_2017_11.csv", sep = ',', col.names = TRUE, row.names = F, quote = T, na = '')

write.table(SimpsonDiversity, "./SERC_wvps_simpsonDiversity_site_2017_11.csv", sep = ',', col.names = TRUE, row.names = F, quote = T, na = '')
write.table(wvps.join, "./SERC_full_2017_11.csv", sep = ',', col.names = TRUE, row.names = F, quote = T, na = '')
#####################################################################################

# 
# req.loc <- GET("http://data.neonscience.org/api/v0/locations/SERC_001.basePlot.div")
# SERC001 <- fromJSON(content(req.loc, as="text"))
# SERC001


## Here, as the wvps data doesnt have any geospatial information, we will join based on the subplotID with pppc data
## Need to generate the pppc unified form to automized, - download from the data portal 
# 
# #Here, we built an summary table that collecting all study area NEON site so that we can merge wvps and pppc based on the plotID or subplotID 
# # we will use just the 10m and 100m excel sheet
# total_pppc <- read.csv(file="G:/NEON Biodviersity Mapping/NEON Availability List/NEON_presence-cover-plant/stackedFiles/div_07m2Data100m2Data.csv")
# total_pppc <- data.frame(total_pppc)
# total_pppc.loc <-  def.extr.geo.os(total_pppc)
# total_pppc.loc <- dplyr::select(total_pppc.loc, uid,adjEasting,adjNorthing,plotID,decimalLatitude,decimalLongitude)


##cleanout wvps database
wvps.join <-dplyr::select(wvps.join,individualID,SimpsonD, namedLocation.x, plotID.y,date.x,eventID.x,siteID.x.x, subplotID.y,growthForm,plantStatus,stemDiameter,measurementHeight,height,subplotID.y,nestedSubplotID,pointID,stemDistance,stemAzimuth,taxonID,scientificName,taxonRank,year.y,month,adjNorthing,adjEasting,api.elevation,adjDecimalLatitude,adjDecimalLongitude,adjCoordinateUncertainty)


wvps.join$adjNorthing <-as.numeric(as.character(wvps.join$adjNorthing))
wvps.join$adjEasting <-as.numeric(as.character(wvps.join$adjEasting))
wvps.join$adjCoordinateUncertainty <-as.numeric(as.character(wvps.join$adjCoordinateUncertainty))
wvps.join$adjDecimalLatitude <-as.numeric(wvps.join$adjDecimalLatitude)
wvps.join$adjDecimalLongitude <-as.numeric(as.character(wvps.join$adjDecimalLongitude))

str(wvps.join)





symbols(wvps.join$adjEasting, wvps.join$adjNorthing, 
        circles=wvps.join$adjCoordinateUncertainty, 
        xlab="Easting", ylab="Northing", tck=0.01, inches=F)


#print out the Northing and Easting boundaries of all plots in NEON sites fg
maxnorth<-max(wvps.join$adjNorthing,na.rm=TRUE)
maxlatitude <-max(wvps.join$adjDecimalLatitude,na.rm=TRUE)+0.01
print(paste0("Max northing: ", maxnorth))
print(paste0("Max latitude: ", maxlatitude))

maxeast<-max(wvps.join$adjEasting,na.rm=TRUE)
maxlongtitude <-max(wvps.join$adjDecimalLongitude,na.rm=TRUE)+0.01
print(paste0("Max easting: ", maxeast))
print(paste0("Max latitude: ", maxlongtitude))

minnorth<-min(wvps.join$adjNorthing,na.rm=TRUE)
minlatitude <-min(wvps.join$adjDecimalLatitude,na.rm=TRUE)-0.01
print(paste0("Min northing: ", minnorth))
print(paste0("Min latitude: ", minlatitude))

mineast<-min(wvps.join$adjEasting,na.rm=TRUE)
minlongtitude <-min(wvps.join$adjDecimalLongitude,na.rm=TRUE)-0.01
print(paste0("Min easting: ", mineast))
print(paste0("Min latitude: ", minlongtitude))

############################
library(RgoogleMaps)
library(sp)
library(ggplot2)
library(ggmap)
library(neonUtilities)
myLocation <- c(minlongtitude, minlatitude, maxlongtitude, maxlatitude)
myMap <- get_map(location=myLocation, source = "google",maptype = "satellite",zoom=12)
ggmap(myMap)
# 
# xy <- wvps.join[,c(24,25)]
# xy <- na.omit(xy)
# xy <-data.frame(xy)
# print(xy)

new_wvps <- na.omit(wvps.join)
new_wvps <-data.frame(new_wvps)
new_wvps$adjDecimalLatitude <- as.double(new_wvps$adjDecimalLatitude)
new_wvps$adjDecimalLongitude <- as.double(new_wvps$adjDecimalLongitude)
print(new_wvps)
str(new_wvps)
write.table(new_wvps, "./SERC_wvps_full_2017_11.csv", sep = ',', col.names = TRUE, row.names = F, quote = T, na = '')

ggmap(myMap)+geom_point(aes(x = new_wvps$adjDecimalLongitude, y = new_wvps$adjDecimalLatitude), data = new_wvps,
                        alpha = .5, color="darkred", size = 2)
# byTileAOP(dpID="DP3.30026.001", site="SERC", year="2016",easting=new_wvps$adjEasting,northing=new_wvps$adjNorthing, buffer=200,check.size = FALSE, savepath = './2017_11')
 byTileAOP(dpID="DP3.30006.001", site="SERC", year="2017",easting=new_wvps$adjEasting,northing=new_wvps$adjNorthing, buffer=200,check.size = FALSE, savepath = './2017_12')
# byTileAOP(dpID="DP3.30024.001", site="SERC", year="2016",easting=new_wvps$adjEasting,northing=new_wvps$adjNorthing, buffer=200,check.size = FALSE, savepath = './2017_11')
# byTileAOP(dpID="DP3.30015.001", site="SERC", year="2016",easting=new_wvps$adjEasting,northing=new_wvps$adjNorthing, buffer=200,check.size = FALSE, savepath = './2017_11')
# byTileAOP(dpID="DP3.30012.001", site="SERC", year="2016",easting=new_wvps$adjEasting,northing=new_wvps$adjNorthing, buffer=200,check.size = FALSE, savepath = './2017_11')
# byTileAOP(dpID="DP3.30026.001", site="SERC", year="2016",easting=new_wvps$adjEasting,northing=new_wvps$adjNorthing, buffer=200,check.size = FALSE, savepath = './2017_11')
# byTileAOP(dpID="DP3.30016.001", site="SERC", year="2016",easting=new_wvps$adjEasting,northing=new_wvps$adjNorthing, buffer=200,check.size = FALSE, savepath = './2017_11')
# byTileAOP(dpID="DP3.30025.001", site="SERC", year="2016",easting=new_wvps$adjEasting,northing=new_wvps$adjNorthing, buffer=200,check.size = FALSE, savepath = './2017_11')
byTileAOP(dpID="DP3.30006.001", site="SERC", year="2016",easting=new_wvps$adjEasting,northing=new_wvps$adjNorthing, buffer=200,check.size = FALSE, savepath = './2017_11_SERC')
