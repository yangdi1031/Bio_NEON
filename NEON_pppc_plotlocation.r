library(httr)
library(jsonlite)
library(dplyr, quietly=T)
library(devtools)
library (stringr)
#install_github('NEONScience/NEON-geolocation/geoNEON', dependencies=TRUE)
library(geoNEON)

setwd(dir = "G:/NEON Biodviersity Mapping/Test")

# Request PPPC data using the GET function & the API call

req <- GET("http://data.neonscience.org/api/v0/products/DP1.10058.001")
req

#View Requested data 
req.content <- content(req, as="parsed")
req.content

# Get data availabilityt list for the product 
req.text <- content(req, as="text")
avail <- fromJSON(req.text, simplifyDataFrame=T, flatten=T)
avail

#Get pppc data avaliabitlity list
pppc.urls <- unlist(avail$data$siteCodes$availableDataUrls)
pppc.urls
#write.csv(pppc.urls,'./pppc_availability.csv')

#Get the data availability for a specific site based on the avaliability list 
pppc <- GET(pppc.urls[grep("GRSM/2017-04", pppc.urls)])
pppc.files <- fromJSON(content(pppc, as="text"))
pppc.files$data$files



# Get botht files of 1m2 data and 10m-100m2 data 
pppc.1m <- read.delim(pppc.files$data$files$url
                      [intersect(grep("div_1m2Data", 
                                      pppc.files$data$files$name),
                                 grep("basic", 
                                      pppc.files$data$files$name))], 
                      sep=",")
pppc.1m <-data.frame(pppc.1m)
head(pppc.1m)
# pppc1.1m <- read.delim(pppc1.files$data$files$url
#                       [intersect(grep("div_1m2Data", 
#                                       pppc1.files$data$files$name),
#                                  grep("basic", 
#                                       pppc1.files$data$files$name))], 
#                       sep=",")
# pppc1.1m <-data.frame(pppc1.1m)

#pppc.1m <-rbind.data.frame(pppc.1m,pppc1.1m)

#Keep only the plant data 
pppc.1m<- dplyr::filter(pppc.1m, divDataType == "plantSpecies")

#remove uniqueID field
pppc.1m <- dplyr::select(pppc.1m, -c(uid))
head(pppc.1m)

pppc.10m_100m2 <- read.delim(pppc.files$data$files$url
                       [intersect(grep("div_10m2Data100m2Data", 
                                       pppc.files$data$files$name),
                                  grep("basic", 
                                       pppc.files$data$files$name))], 
                       sep=",")
#pppc.10m_100m2 <- data.frame(pppc.10m_100m2)
#remove uniqueID field
pppc.10m_100m2 <- dplyr::select(pppc.10m_100m2, -c(uid))
head(pppc.10m_100m2)


pppc.100m2 = pppc.10m_100m2[which(nchar(as.character(pppc.10m_100m2$subplotID))<3), ]
pppc.10m2 = pppc.10m_100m2[which(nchar(as.character(pppc.10m_100m2$subplotID))>2), ]


#rename 1m2 for adding to 10m2 becaues 1m2 subplots nested in 10m2 data but depending on year not always processed that way:
data_10m2Build <- pppc.1m
# rename 1m2 subplots for easy combine later
data_10m2Build$subplotID[data_10m2Build$subplotID == "31.1.1"] <- "31.1.10"
data_10m2Build$subplotID[data_10m2Build$subplotID == "31.4.1"] <- "31.4.10"
data_10m2Build$subplotID[data_10m2Build$subplotID == "32.2.1"] <- "32.2.10"
data_10m2Build$subplotID[data_10m2Build$subplotID == "32.4.1"] <- "32.4.10"  
data_10m2Build$subplotID[data_10m2Build$subplotID == "40.1.1"] <- "40.1.10"
data_10m2Build$subplotID[data_10m2Build$subplotID == "40.3.1"] <- "40.3.10"
data_10m2Build$subplotID[data_10m2Build$subplotID == "41.1.1"] <- "41.1.10"
data_10m2Build$subplotID[data_10m2Build$subplotID == "41.4.1"] <- "41.4.10"

head(data_10m2Build)

#data preparation, add a field that is unique to the 10m and 100m2 data
data_10m2Build$additionalSpecies <- NA
#change the data type
data_10m2Build$additionalSpecies <- as.character(data_10m2Build$additionalSpecies)

#select the fields that are common to the 10m2 data
data_10m2Build <- dplyr::select(data_10m2Build, namedLocation, domainID,	siteID,	decimalLatitude,	decimalLongitude,	geodeticDatum, nlcdClass,	coordinateUncertainty,	elevation,	elevationUncertainty,
                                plotID,	subplotID,	endDate,	boutNumber,	samplingProtocolVersion,	targetTaxaPresent,	taxonID,	scientificName,	taxonRank,	family,	nativeStatusCode,
                                identificationQualifier,	taxonIDRemarks,	morphospeciesID,	morphospeciesIDRemarks,	identificationReferences,	additionalSpecies,	remarks,	measuredBy,	recordedBy)  

#combine what was the nested 1m2 subplot data with the 10m2 data to get the complete list of species in each 10m2 subplot 
pppc.10m2 <- rbind(pppc.10m2, data_10m2Build)


data_100m2Build <- pppc.10m2

data_100m2Build$subplotID[data_100m2Build$subplotID == "31.1.10"] <- 31
data_100m2Build$subplotID[data_100m2Build$subplotID == "31.4.10"] <- 31
data_100m2Build$subplotID[data_100m2Build$subplotID == "32.2.10"] <- 32
data_100m2Build$subplotID[data_100m2Build$subplotID == "32.4.10"] <- 32
data_100m2Build$subplotID[data_100m2Build$subplotID == "40.1.10"] <- 40
data_100m2Build$subplotID[data_100m2Build$subplotID == "40.3.10"] <- 40
data_100m2Build$subplotID[data_100m2Build$subplotID == "41.1.10"] <- 41
data_100m2Build$subplotID[data_100m2Build$subplotID == "41.4.10"] <- 41

pppc.100m2 <- rbind(pppc.100m2, data_100m2Build)

#remove duplicates at all scales
pppc.1m <- unique(pppc.1m)
pppc.10m2 <- unique(pppc.10m2)
pppc.100m2 <- unique(pppc.100m2)

#create year column
pppc.1m$year <- substr(pppc.1m$endDate, start = 1, stop = 4)
pppc.10m2$year <- substr(pppc.10m2$endDate, start = 1, stop = 4)
pppc.100m2$year <- substr(pppc.100m2$endDate, start = 1, stop = 4)

####Diversity indices####

##calculate at the site scale

#alpha diversity = number species in each plot
#gamma diversity = number of species found in all plots across the site
#beta diversity = gamma/(mean alpha)

#site alpha diversity
sitePlotAlphaDiv <- pppc.10m2 %>%
  select(siteID, plotID, year, boutNumber, taxonID) %>%
  unique() %>%
  group_by(siteID, year, boutNumber, plotID) %>%
  summarise(plotSpeciesRichness = n()) %>%
  summarise(meanSiteAlphaDiversity = mean(plotSpeciesRichness)) %>%
  ungroup()

#site gamma diversity 
siteGammaDiv <- pppc.10m2 %>%
  select(siteID, year, boutNumber, taxonID) %>%
  unique() %>%
  group_by(siteID, year, boutNumber) %>%
  summarise(siteGammaDiversity = n())%>%
  ungroup()

#combine for site beta diversity
siteDiversity <- dplyr::full_join(sitePlotAlphaDiv, siteGammaDiv, by = c("siteID", "year", "boutNumber")) 
siteDiversity$siteGammaDiversity <- as.numeric(siteDiversity$siteGammaDiversity)
siteDiversity$siteBetaDiversity <- siteDiversity$siteGammaDiversity/siteDiversity$meanSiteAlphaDiversity

#write the file to working directory
write.table(siteDiversity, "./siteDiversity.csv", sep = ',', col.names = TRUE, row.names = F, quote = T, na = '')


##calculate at the plot scale
#plot apha diversity
plotAlphaDiv <- pppc.10m2 %>%
  select(siteID, plotID, subplotID, year, boutNumber, taxonID) %>%
  unique() %>%
  group_by(siteID, year, boutNumber, plotID, subplotID) %>%
  summarise(subplotRichness = n()) %>%
  ungroup %>%
  group_by(siteID, year, boutNumber, plotID) %>%
  summarise(plotAlphaDiversity = mean(subplotRichness)) %>%
  ungroup()

#plot gamma diversity
plotGammaDiv <- pppc.10m2 %>%
  select(siteID, plotID, year, boutNumber, taxonID) %>%
  unique() %>%
  group_by(siteID, year, boutNumber, plotID) %>%
  summarise(plotGammaDiversity = n())

#combine for plot gamma diversity
plotDiversity <- dplyr::full_join(plotAlphaDiv, plotGammaDiv, by = c("siteID", "year", "boutNumber", "plotID"))
plotDiversity$plotGammaDiversity <- as.numeric(plotDiversity$plotGammaDiversity)
plotDiversity$plotBetaDiversity <- plotDiversity$plotGammaDiversity/plotDiversity$plotAlphaDiversity

#write the file to working directory
write.table(plotDiversity, "./plotDiversity.csv",  
            sep = ',', col.names = TRUE, row.names = F, quote = T, na = '')

##################################################################################

# Cluster by scienfitic name
cluster <- pppc.1m %>%
  group_by(scientificName)%>%
  summarize(total=sum(percentCover))

cluster <- cluster[order(cluster$total, decreasing=T),]
barplot(cluster$total, names.arg=cluster$scientificName, 
        ylab="Total", cex.names=0.5, las=3)
head(pppc.10m$namedLocation)
# 
# req.loc <- GET("http://data.neonscience.org/api/v0/locations/HARV_001.basePlot.div")
# harv001 <- fromJSON(content(req.loc, as="text"))
# harv001
library(geoNEON)

# extract the spatial data
pppc.1m.loc <- def.extr.geo.os(pppc.1m)
pppc.1m.loc <-data.frame(pppc.1m.loc)

symbols(pppc.1m.loc$api.easting, pppc.1m.loc$api.northing, 
        circles=pppc.1m.loc$coordinateUncertainty, 
        xlab="Easting", ylab="Northing", tck=0.01, inches=F)


pppc.1m.pt <- def.calc.geo.os(pppc.1m.loc, "div_1m2Data")


symbols(pppc.1m.pt$adjEasting, pppc.1m.pt$adjNorthing, 
        circles=pppc.1m.pt$adjCoordinateUncertainty, 
        xlab="Easting", ylab="Northing", tck=0.01, inches=F)


#print out the Northing and Easting boundaries of all plots in NEON sites fg
maxnorth<-max(pppc.1m.loc$api.northing)
maxlatitude <-max(pppc.1m.loc$decimalLatitude)+0.01
print(paste0("Max northing: ", maxnorth))
print(paste0("Max latitude: ", maxlatitude))

maxeast<-max(pppc.1m.loc$api.easting)
maxlongtitude <-max(pppc.1m.loc$decimalLongitude)+0.01
print(paste0("Max easting: ", maxeast))
print(paste0("Max latitude: ", maxlongtitude))

minnorth<-min(pppc.1m.loc$api.northing)
minlatitude <-min(pppc.1m.loc$decimalLatitude)-0.01
print(paste0("Min northing: ", minnorth))
print(paste0("Min latitude: ", minlatitude))

mineast<-min(pppc.1m.loc$api.easting)
minlongtitude <-min(pppc.1m.loc$decimalLongitude)-0.01
print(paste0("Min easting: ", mineast))
print(paste0("Min latitude: ", minlongtitude))

############################
library(RgoogleMaps)
library(sp)
library(ggplot2)
library(ggmap)
library(neonUtilities)
myLocation <- c(minlongtitude, minlatitude, maxlongtitude, maxlatitude)
myMap <- get_map(location=myLocation, source = "google",maptype = "satellite",zoom=11)

# 
# 
# 
xy <- pppc.1m.pt[,c(5,6)]
xy <-data.frame(xy)

spdf <- SpatialPointsDataFrame(coords = xy, pppc.1m.pt,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


ggmap(myMap)+geom_point(aes(x = xy$decimalLongitude, y = xy$decimalLatitude), data = xy,
                        alpha = .5, color="darkred", size = 2)


#register_google(key = "AIzaSyB7VCRxS1MiIM2FQggh05ii489L9pxdRRA ", write = TRUE)
# 
# byTileAOP(dpID="DP3.30026.001", site="GRSM", year="2017",easting=c(271365,286394),northing=c(3949672,3953628), buffer=100,check.size = FALSE)
# byTileAOP(dpID="DP3.30024.001", site="GRSM", year="2017",easting=c(271365,286394),northing=c(3949672,3953628), buffer=100,check.size = FALSE)
# byTileAOP(dpID="DP3.30015.001", site="GRSM", year="2017",easting=c(271365,286394),northing=c(3949672,3953628), buffer=100,check.size = FALSE)
# byTileAOP(dpID="DP3.30010.001", site="GRSM", year="2017",easting=c(271365,286394),northing=c(3949672,3953628), buffer=100,check.size = FALSE)
byTileAOP(dpID="DP3.30026.001", site="GRSM", year="2017",easting= pppc.1m.pt$api.easting,northing= pppc.1m.pt$api.northing, buffer=100,check.size = FALSE)
byTileAOP(dpID="DP3.30006.001", site="GRSM", year="2017",easting= pppc.1m.pt$api.easting,northing= pppc.1m.pt$api.northing, buffer=100,check.size = FALSE)
byTileAOP(dpID="DP3.30024.001", site="GRSM", year="2017",easting= pppc.1m.pt$api.easting,northing= pppc.1m.pt$api.northing, buffer=100,check.size = FALSE)
byTileAOP(dpID="DP3.30015.001", site="GRSM", year="2017",easting= pppc.1m.pt$api.easting,northing= pppc.1m.pt$api.northing, buffer=100,check.size = FALSE)
byTileAOP(dpID="DP3.30010.001", site="GRSM", year="2017",easting= pppc.1m.pt$api.easting,northing= pppc.1m.pt$api.northing, buffer=100,check.size = FALSE)

byFileAOP(dpID="DP3.30010.001", site="GRSM", year="2017",easting= pppc.1m.pt$api.easting,northing= pppc.1m.pt$api.northing, buffer=100,check.size = FALSE)

#data.plusSpatial <- def.extr.geo.os(pppc.1m.loc)






