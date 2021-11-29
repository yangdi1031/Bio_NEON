# Load `raster` and `rhdf5` packages and read NIS data into R
library(raster)
# install.packages("BiocManager")
# BiocManager::install("rhdf5")
library(rhdf5)
library(rgdal)
library(stringr)
library(entropy)
library(TSA)

#setwd(dir = "//marthathegreat/SAL/NEON/NEON_Biodiversity/Data/Derived_Data/Hyperspectral/GRSM/GRSM_2017")
#setwd(dir = "D:/GRSM_2018")
## change the data path
setwd(dir = "D:/2017_GRSM_3/L1/Spectrometer/ReflectanceH5/2017101216")
file_list<-list.files(pattern="*.h5")

## count the number of files in the folder
n_file<-length(file_list)

## number 16 is n_file, used the number here because if the n_file is too large, there will be a memory issue
for(i in 1:2){

f <- file_list[i]
print(f)
h5ls(f,all=T) 
### for different NEON sites, replace all "GRSM" with the other sites name, such as "HARV" for Harvard Forests
spInfo <- h5readAttributes(f,"/GRSM/Reflectance/Metadata/Coordinate_System/Map_Info")
reflInfo <- h5readAttributes(f,"/GRSM/Reflectance/Reflectance_Data")
wavelengths<- h5read(f,"/GRSM/Reflectance/Metadata/Spectral_Data/Wavelength")
nRows <- reflInfo$Dimensions[1]
nCols <- reflInfo$Dimensions[2]
nBands <- reflInfo$Dimensions[3]
myCRS <- h5read(f,"/GRSM/Reflectance/Metadata/Coordinate_System/Proj4")
print(myCRS)
b<-list()
filter_bands <- c(c(1:283),c(322:419))
filter_bands <-data.frame(filter_bands)

x<-stack()
#for (j in 1:4)  {
for (j in 1:nrow(filter_bands))  {
  print(j)
  b<-list()
  b[[j]] <- list()
  b[[j]] <- h5read(f,"/GRSM/Reflectance/Reflectance_Data",index=list(j,1:nCols,1:nRows))
  b[[j]] <- b[[j]][1,,]

  ### remember to transpose the image

  b[[j]] <-t(b[[j]])

  h5readAttributes(f,"/GRSM/Reflectance/Reflectance_Data")
  
  #Check the myNoDataValue to make sure the noDatavalue and here it shows to be -9999
  # myNoDataValue <- as.numeric(reflInfo$`Data_Ignore_Value`)
  # myNoDataValue
 # hist(b[[j]],breaks=40,col="darkmagenta")
  myNoDataValue <- -9999
  b[[j]][b[[j]] == myNoDataValue] <- 0

  mapInfo<-h5read(f,"/GRSM/Reflectance/Metadata/Coordinate_System/Map_Info")
  mapInfo<-unlist(strsplit(mapInfo, ","))

  myCRS <- h5read(f,"/GRSM/Reflectance/Metadata/Coordinate_System/Proj4")
  ##change projection: HARV -> 18N. More info here: https://www.neonscience.org/field-sites/about-field-sites 
  b[[j]] <-raster(b[[j]],crs = "+proj=utm +zone=17N +ellps=WGS84 +datum=WGS84" )

  res <-reflInfo$Spatial_Resolution_X_Y
  xMin <- as.numeric(mapInfo[4])
  #grab the top corner coordinate (yMax)
  yMax <- as.numeric(mapInfo[5])
  xMax <- (xMin + (ncol(b[[j]]))*1)
  yMin <- (yMax - (nrow(b[[j]]))*1)

  rasExt <- extent(xMin,xMax,yMin,yMax)
  extent(b[[j]]) <- rasExt

  name <-paste("band_",j,".tif",sep="")
  #image(b[[j]])

  x <-stack(x,b[[j]])
  #writeRaster(b[[j]], filename=name, format="GTiff",overwrite=TRUE) 
  
  h5closeAll()
  
}


##change the filename if is not BRDF corrected
writeRaster(x,paste('Reflectance_',str_sub(file_list[i],1,nchar(file_list[i])-3),'.tif', sep=' '),format="GTiff",overwrite=T)

r_mean <- calc(x, mean)
writeRaster(r_mean,paste('Mean_',str_sub(file_list[i],1,nchar(file_list[i])-3),'.tif', sep=' '),format="GTiff")
plot(r_mean)
r_coefficient <- calc(x,cv)
plot(r_coefficient)
writeRaster(r_coefficient,paste('CV_',str_sub(file_list[i],1,nchar(file_list[i])-3),'.tif', sep=' '),format="GTiff")
r_median <- calc(x, median)
writeRaster(r_median,paste('Median_',str_sub(file_list[i],1,nchar(file_list[i])-3),'.tif', sep=' '),format="GTiff")
r_sd <- calc(x, sd)
writeRaster(r_sd,paste('SD_',str_sub(file_list[i],1,nchar(file_list[i])-3),'.tif', sep=' '),format="GTiff")
r_max <-calc(x,max)
writeRaster(r_max,paste('Max_',str_sub(file_list[i],1,nchar(file_list[i])-3),'.tif', sep=' '),format="GTiff")

ts_entropy <- function(x) {
  ##		x is assumed to already be a timeseries object:
  Ps <- spec(x, log="no", plot=FALSE)
  Ps_spec <- Ps$spec
  Pk <- Ps_spec / sum(Ps_spec)
  Hsn_x <- -1 * sum(Pk * log(Pk)) / (log(length(Pk)+0.0000000001))
  as.numeric(Hsn_x)
  ##  Hsn_x[is.na(Hsn_x)]<-0
  # x_null <- sample(x)
  # 
  # Ps <- spec(x_null, log="no", plot=FALSE)
  # Ps_spec <- Ps$spec
  # Pk <- Ps_spec / sum(Ps_spec)
  #  Hsn_null <- -1 * sum(Pk * log(Pk)) / log(length(Pk))
  ##	The following would be the start to performing the bootstrap for
  ##		confidence interval generation, but for now we'll just return the
  ##		normalized spectral entropy (Hsn) from the series and call it good.
  return(Hsn_x)
}

# tile_cols <- 2
# tile_rows <- 4
# 
# nrows_for_subset <- round(f_brick@nrows/tile_rows)
# ncols_for_subset <- round(f_brick@ncols/tile_cols)
# 
# subset_extent_1 <- extent(f_brick, 1, nrows_for_subset, 1, ncols_for_subset)
# subset_extent_2 <- extent(f_brick, 1, nrows_for_subset, ncols_for_subset+1, f_brick@ncols)
# subset_extent_3 <- extent(f_brick, nrows_for_subset+1, 2*nrows_for_subset, 1, ncols_for_subset)
# subset_extent_4 <- extent(f_brick, nrows_for_subset+1, 2*nrows_for_subset, ncols_for_subset+1, f_brick@ncols)
# subset_extent_5 <- extent(f_brick, 2*nrows_for_subset+1, 3*nrows_for_subset, 1, ncols_for_subset)
# subset_extent_6 <- extent(f_brick, 2*nrows_for_subset+1, 3*nrows_for_subset, ncols_for_subset+1, f_brick@ncols)
# subset_extent_7 <- extent(f_brick, 3*nrows_for_subset+1, f_brick@nrows, 1, ncols_for_subset)
# subset_extent_8 <- extent(f_brick, 3*nrows_for_subset+1, f_brick@nrows, ncols_for_subset+1, f_brick@ncols)
# 
# raster_subset <- crop(f_brick, subset_extent_3)
# raster_subset <-brick(raster_subset)
#plot(raster_subset)
#is.na(f_brick)==0
#plot(f_brick)
#calculate the spectral entropy 
r_entropy <- calc(x, entropy)
#calculate the spectral normalized entropy
r_normalised_entropy <- calc(x, ts_entropy)
writeRaster(r_entropy,paste('Entropy_',str_sub(file_list[i],1,nchar(file_list[i])-3),'.tif', sep=' '),format="GTiff")
writeRaster(r_normalised_entropy,paste('N_Entropy_',str_sub(file_list[i],1,nchar(file_list[i])-3),'.tif', sep=' '),format="GTiff")
plot(r_normalised_entropy)
}

