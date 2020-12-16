setwd("F:/Mes_Docs/WorkingDIR")

.libPaths("F:/Mes_Docs/WorkingDIR/Library")


# Load packages
library(rgdal)        
library(raster)
library(plyr)         
library(dplyr)        
library(RStoolbox)     
library(RColorBrewer) 
library(sp)           
library(doParallel)   
library(e1071)       
library(pROC)
library(CAST)
library(ggplot2)           
library(caret)        
library(openxlsx)
library(pROC)
library(ROCR)

library(viridis)
library(latticeExtra)
library(gridExtra)
library(grid)
library(mapview)

palfunc <- function (n, alpha = 1, begin = 0, end = 1, direction = 1) 
{
  colors <- rev(brewer.pal(11, "RdYlGn"))
  if (direction < 0) colors <- rev(colors)
  colorRampPalette(colors, alpha = alpha)(n)
}


######################### Prediction Rasters
# List of rasters

setwd("F:/Mes_Docs/Souss/Floods/Data/Cartes")

f = list.files(, pattern = ".tif$", full.names = T)

# Load rasters

ras = lapply(f,raster)

# Stack rasters

Rasters = stack(ras)

names(Rasters) = c("KNN","KNN_SE","KNN_TR","KNN_TR_SE",
                   "NNET","NNET_SE","NNET_TR","NNET_TR_SE",
                   "RF" ,"RF_SE","RF_TR","RF_TR_SE",
                   "XGB","XGB_SE","XGB_TR","XGB_TR_SE")


############# Load the Shapefile of the watershed

setwd("F:/Mes_Docs/Souss/Floods/Data/Shapefile")

Souss = shapefile("BV_Souss.shp")


########################### Plot and save maps

png("Floods_maps.png",
    width=17,height=22,units="cm",res = 600)
spplot(Rasters,maxpixels=50000,col.regions=palfunc,cuts=80,
       ylab.right=expression ("Floods occurrence probability"),
       par.settings = list(layout.widths = list(axis.key.padding = 0,
                                                ylab.right = 2))) + 
  as.layer(spplot(Souss,col="black",col.regions="transparent",lwd=2))

  # as.layer(spplot(RH,col="blue",col.regions="transparent",lwd=2)) + 
  # layer(sp.text(coordinates(Douars), txt = Douars$NOM, pos = 1))

dev.off()


############ Reclass maps

setwd("F:/Mes_Docs/Souss/Floods/Data/Cartes/rcl")

f = list.files(, pattern = ".tif$", full.names = T)

Ras = list()
for (i in 1:length(f)){
  R = raster(f[i])
  R = as.factor(R)
  rat <- levels(R)[[1]]
  rat [["Classes"]] <- c("Very low (0-0.2)","Low (0.2-0.4)","Moderate (0.4-0.6)",
                         "High (0.6-0.8)","Very high (0.8-1.0)")
  levels(R) <- rat
  Ras[[i]] = R
}

R = stack(Ras)

names(R) = c("KNN","KNN_SE","KNN_TR","KNN_TR_SE",
                   "NNET","NNET_SE","NNET_TR","NNET_TR_SE",
                   "RF" ,"RF_SE","RF_TR","RF_TR_SE",
                   "XGB","XGB_SE","XGB_TR","XGB_TR_SE")

# Plot and save maps

png("Floods_rcl_maps.png",
    width=17,height=22,units="cm",res = 600)
rasterVis::levelplot(R, col.regions=palfunc,
                     par.settings=list(axis.line=list(col="transparent")),
                     scales = list(relation = "free", y = list(draw = FALSE),x = list(draw = FALSE))) + 
  as.layer(spplot(Souss,col="black",col.regions="transparent",lwd=2)) 

dev.off()

