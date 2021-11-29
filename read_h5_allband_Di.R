# Load `raster` and `rhdf5` packages and read NIS data into R
library(raster)
library(rhdf5)
library(rgdal)

#setwd(dir = "//marthathegreat/SAL/NEON/NEON_Biodiversity/Data/Derived_Data/Hyperspectral/GRSM/GRSM_2017")
#setwd(dir = "D:/2018/FullSite/D07/2018_GRSM_4/L1/Spectrometer/ReflectanceH5/2018061612")
#setwd(dir = "//petaLibrary.arcc.uwyo.edu/commons/remotesensing/NEON/BRDF_Corrected/GRSM_2018")
setwd(dir = 'R:/NEON/BRDF_Corrected/GRSM_2018')
f <-'./NEON_D07_GRSM_DP1_20180616_141237_reflectance_BRDF.h5'
h5ls(f,all=T) 
spInfo <- h5readAttributes(f,"/GRSM/Reflectance/Metadata/Coordinate_System/Map_Info")
reflInfo <- h5readAttributes(f,"/GRSM/Reflectance/Reflectance_Data")
wavelengths<- h5read(f,"/GRSM/Reflectance/Metadata/Spectral_Data/Wavelength")
nRows <- reflInfo$Dimensions[1]
nCols <- reflInfo$Dimensions[2]
nBands <- reflInfo$Dimensions[3]
b <- list()

myCRS <- h5read(f,"/GRSM/Reflectance/Metadata/Coordinate_System/Proj4")
print(myCRS)

filter_bands <- c(c(1:283),c(322:419))
filter_bands <-data.frame(filter_bands)


for (i in 1:nrow(filter_bands))  {

b[[i]] <- h5read(f,"/GRSM/Reflectance/Reflectance_Data",index=list(i,1:nCols,1:nRows))

b[[i]] <- b[[i]][1,,]

### remember to transpose the image 

b[[i]] <-t(b[[i]])

h5readAttributes(f,"/GRSM/Reflectance/Reflectance_Data")

hist(b[[i]],breaks=40,col="darkmagenta")
myNoDataValue <- as.numeric(reflInfo$`Data_Ignore_Value`)
myNoDataValue
b[[i]][b[[i]] == myNoDataValue] <- NA

mapInfo<-h5read(f,"/GRSM/Reflectance/Metadata/Coordinate_System/Map_Info")
mapInfo<-unlist(strsplit(mapInfo, ","))

myCRS <- h5read(f,"/GRSM/Reflectance/Metadata/Coordinate_System/Proj4")


b[[i]] <-raster(b[[i]],crs = "+proj=utm +zone=18N +ellps=WGS84 +datum=WGS84" )

# cv(b[[i]], na.rm =TRUE)
# 
# sd(b[[i]], na.rm =TRUE)


res <-reflInfo$Spatial_Resolution_X_Y
res
xMin <- as.numeric(mapInfo[4]) 
#grab the top corner coordinate (yMax)
yMax <- as.numeric(mapInfo[5])


xMax <- (xMin + (ncol(b[[i]]))*1)
yMin <- (yMax - (nrow(b[[i]]))*1) 

rasExt <- extent(xMin,xMax,yMin,yMax)
extent(b[[i]]) <- rasExt

name <-paste("band_",i,".tif",sep="")
#writeRaster(b[[i]], filename=name, format="GTiff",overwrite=TRUE)

#It's always good practice to close the H5 connection before moving on!
#close the H5 file
h5closeAll()
}

b <- stack(b)
writeRaster(b,filename="NEON_D07_GRSM_DP1_20180616_141237_reflectance",format="GTiff",overwrite=T)
# 
# 
# f_brick <- brick(b)
# r_mean <- calc(f_brick, mean)
# writeRaster(r_mean,filename="NEON_D02_GRSM_DP3_365000_4305000_reflectance_noise_removal_mean",format="GTiff",overwrite=T)
# r_coefficient <- calc(f_brick,cv)
# writeRaster(r_coefficient,filename="NEON_D02_GRSM_DP3_365000_4305000_reflectance_noise_removal_cv",format="GTiff",overwrite=T)
# r_median <- calc(f_brick, median)
# writeRaster(r_median,filename="NEON_D02_GRSM_DP3_365000_4305000_reflectance_noise_removal_median",format="GTiff",overwrite=T)
# r_sd <- calc(f_brick, sd)
# writeRaster(r_sd,filename="NEON_D02_GRSM_DP3_365000_4305000_reflectance_noise_removal_stdev",format="GTiff",overwrite=T)
# r_max <-calc(f_brick,max)
# writeRaster(r_max,filename="NEON_D02_GRSM_DP3_365000_4305000_reflectance_noise_removal_max",format="GTiff",overwrite=T)
# r_var <-calc(f_brick,var)
# writeRaster(r_var,filename="NEON_D02_GRSM_DP3_365000_4305000_reflectance_noise_removal_variance",format="GTiff",overwrite=T)
# image(r_var)
# 
# # Define a function to calculate the normalized spectral entropy for a 
# # ##		time series (as defined from the values extracted for each
# # ##		pixel:
# # 
# # ##	As adapted from Zaccarelli (2013), variable names relate to terms
# # ##		in the described Appendix and model:
# 
# library(entropy)
# library(TSA)
# ts_entropy <- function(x) {
#   ##		x is assumed to already be a timeseries object:
#   Ps <- spec(x, log="no", plot=FALSE)
#   Ps_spec <- Ps$spec
#   Pk <- Ps_spec / sum(Ps_spec)
#   Hsn_x <- -1 * sum(Pk * log(Pk)) / (log(length(Pk))+0.0000000000000001)
#   Hsn_x[is.na(Hsn_x)]<-0
#   ##	The following would be the start to performing the bootstrap for 
#   ##		confidence interval generation, but for now we'll just return the
#   ##		normalized spectral entropy (Hsn) from the series and call it good.
#   return(Hsn_x)
# }
# 
# r_entropy <-calc(f_brick, ts_entropy, forceapply=TRUE)
# image(r_entropy)
# writeRaster(r_entropy,filename="NEON_D02_GRSM_DP3_365000_4305000_reflectance_noise_removal_entropy",format="GTiff",overwrite=T)
