
#set the personal library
.libPaths("E:/Mes_Docs/WorkingDIR/MyLibrary")

library(raster)

# All the 13 predictors have been prepared using QGIS

# Load the DEM

dem = raster("F:/Mes_Docs/Souss/Floods/Data/Rasters/Dem.tif")

# Resample all the predictors to the extent of the dem raster 

f = list.files("F:/Mes_Docs/Souss/Floods/Data/Rasters", pattern = ".tif$", full.names = T)
fn <- gsub("\\.tif$", ".tif", f)

for (i in 1:length(f)) {
  r <- raster(f[i])
  resamp <- resample(r, dem, resample = 'bilinear')
  writeRaster(resamp , filename = fn[i], overwrite = TRUE)
}
