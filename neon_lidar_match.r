library(httr)
library(jsonlite)
library(dplyr, quietly=T)
library(devtools)
library (stringr)
#install_github('NEONScience/NEON-geolocation/geoNEON', dependencies=TRUE)
library(geoNEON)
setwd("G:/NEON Biodviersity Mapping/Test/2016")
folder <- "G:/NEON_Biodviersity_Mapping/Test/2016/DP3.30015.001/2016/FullSite/D07/2016_GRSM_2/L3/DiscreteLidar/CanopyHeightModelGtif"
file_list <- list.files(path=folder)
file_list <- data.frame(file_list)

#this is for the dsm
file_list$LiDARfilename <- substr(file_list$file_list, start = 13, stop = 26)



req <- GET("http://data.neonscience.org/api/v0/products/DP1.30003.001")
req

#View Requested data 
req.content <- content(req, as="parsed")
req.content

# Get data availabilityt list for the product 
req.text <- content(req, as="text")
avail <- fromJSON(req.text, simplifyDataFrame=T, flatten=T)
avail

#Get LiDAR data avaliabitlity list
LiDAR.urls <- unlist(avail$data$siteCodes$availableDataUrls)
LiDAR.urls

LiDAR <- GET(LiDAR.urls[grep("GRSM/2016-06", LiDAR.urls)])
LiDAR.files <- fromJSON(content(LiDAR, as="text"))
LiDAR.files$data$files

LiDAR.files <-data.frame(LiDAR.files)

LiDAR.files$format <- substr(LiDAR.files$data.files.name, nchar(LiDAR.files$data.files.name)-2,nchar(LiDAR.files$data.files.name))
LiDAR.files <- dplyr::filter(LiDAR.files, format == "laz")

# LiDAR.files.classified <- LiDAR.files %>%
#   filter(str_detect(data.files.name, "classified_point_cloud"))

LiDAR.files$LiDARfilename <- substr(LiDAR.files$data.files.name, start = 13,stop =26)

LiDAR.files.new <- dplyr::inner_join(LiDAR.files, file_list, by = "LiDARfilename") 

#to download the images, we used the the package named "downloader" to download files 

library(downloader)

for (i in 1:nrow(LiDAR.files.new)){
  
  print(LiDAR.files.new$data.files.name[i])
  download(LiDAR.files.new$data.files.url[i],paste(LiDAR.files.new$data.files.name[i],".laz", sep=""),mode="wb")
  
}



         