
.libPaths("F:/Mes_Docs/WorkingDIR/Library")

# Load packages

library(rgdal)        
library(raster)       
library(plyr)         
library(dplyr)        
library(RStoolbox)    
library(RColorBrewer) 
library(ggplot2)      
library(sp)          
library(caret)        
library(doParallel)   
library(openxlsx)
library(pROC)
library(tidyverse)


# List of variables

f = list.files("F:/Mes_Docs/Souss/Floods/Data/Rasters", pattern = ".tif$", full.names = T)

#Load variables

ras = lapply(f,raster)

# Stack variables

st = stack(ras)


# Load flood points
fp = shapefile("F:/Mes_Docs/Souss/Floods/Data/Shapefile/Flood_points.shp")

#Extract values to flood points

ext = extract(st,fp)

ext2 = unlist(ext)

#Convert to dataframe

ext2 = as.data.frame(ext2)

# Add ID to flood points

fp@data$ID <- seq.int(nrow(fp@data))

# Add ID to extracted dat

ext2 <- rowid_to_column(ext2, "ID")

# Merge flood points and extracted data

extm <- merge(ext2, fp,by.x="ID", by.y="ID")


# Data partition

set.seed(100)
trainids <- createDataPartition(extm$Flood, list=FALSE,p=0.7)
trainDat <- extm[trainids,]
testDat <- extm[-trainids,]



####################################################################################
#Modeling without transformation of categorical variables and variables selection
######################################################################################


# Transforming floods occurrences codes 1 and 0 to yes and no

trainDat = trainDat[,-c(1,16:17)]
trainDat$Floods <- ifelse(trainDat$Floods == 1, "yes","no")
trainDat$Floods <- as.factor(trainDat$Floods)

testDat = testDat[,-c(1,16:17)]
testDat$Floods <- ifelse(testDat$Floods == 1, "yes","no")
testDat$Floods <- as.factor(testDat$Floods)

# Center numerical variables

maxss <- apply(trainDat[,c(2:6,10:11,13)], 2, max)
minss <- apply(trainDat[,c(2:6,10:11,13)], 2, min)
numvar <- as.data.frame(scale(trainDat[,c(2:6,10:11,13)], center = minss, scale = maxss - minss))
trainDat <- data.frame(cbind(trainDat[,14], numvar, trainDat[,c(1,7:9,12)]))
names(trainDat)[1] = "Floods"

maxss <- apply(testDat[,c(2:6,10:11,13)], 2, max)
minss <- apply(testDat[,c(2:6,10:11,13)], 2, min)
numvar <- as.data.frame(scale(testDat[,c(2:6,10:11,13)], center = minss, scale = maxss - minss))
testDat<- data.frame(cbind(testDat[,14], numvar, testDat[,c(1,7:9,12)]))
names(testDat)[1] = "Floods"

# Save train and test data

setwd("F:/Mes_Docs/Souss/Floods/Data")

write.xlsx(trainDat,"Train_Data.xlsx")
write.xlsx(testDat,"Test_Data.xlsx")

# Convert categorical variables to factor

trainDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
  lapply(trainDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.factor)

testDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
  lapply(testDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.factor)


# Train rf model

#Random search#####
 
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        search = 'random')    

set.seed(1)
rf_random <- train(Floods~., 
                   data=trainDat,
                   method = 'rf',
                   metric = 'Accuracy',
                   trControl = control,
                   importance = TRUE)

save(rf_random, file = "fit_rf.RData")


# Final model

All_incidents <- merge(trainDat, testDat, all=TRUE) 

set.seed(849)
fit.rfAll<- train(Floods~., 
                  data=All_incidents,
                  method = "rf",
                  metric = "Accuracy",
                  trControl = control,
                  importance = TRUE)

save(fit.rfAll, file = "fit_rfAll.Rdata")


fit.rfAll = get(load(file = "F:/Mes_Docs/Souss/Floods/Data/fit_rfAll.Rdata"))

#stacked rasters

Rasters=st

# Convert rasters to dataframe with x-y -----------------------

df = as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)

# Scale the numeric variables --------------------------------------

maxss <- apply(df[,c(2:6,10:11,13)], 2, max) 
minss <- apply(df[,c(2:6,10:11,13)], 2, min)
df_scaled <- as.data.frame(scale(df[,c(2:6,10:11,13)], center = minss, scale = maxss - minss)) 

# Add back the (x,y) and to categorical variables

df_scaled <- data.frame(cbind(df[,c(14,15)], df_scaled, df[,c(1,7:9,12)]))


# Omit na

df_scaled = df_scaled[complete.cases(df_scaled),]

# Convert categorical variables to factor

df_scaled[,c("Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
  lapply(df_scaled[,c("Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.factor)

str(df_scaled)

save(df_scaled, file = "df_scaled_OV.RData")
write.xlsx("df_scaled.xlsx")

# PRODUCE PROBABILITY MAP

p <-as.data.frame(predict(fit.rfAll, df_scaled[,-c(1,2)], type = "prob"))

df$Levels_yes<-p$yes
df$Levels_no<-p$no

x<-SpatialPointsDataFrame(as.data.frame(df)[, c("x", "y")], data = df)
r_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
r_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])

# Load DEM

dem = raster("F:/Mes_Docs/Souss/Floods/Data/Rasters/Dem.tif")

# Assign the DEM coordinates to the prediction raster

proj4string(r_yes)=CRS(projection(dem))

proj4string(r_no)=CRS(projection(dem))

# Save rasters

writeRaster(r_yes,filename="Prediction_floods_rf.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_no,filename="Prediction_non_floods_rf.tif", format="GTiff", overwrite=TRUE) 

# Plot Maps

# Palette function

palfunc <- function (n, alpha = 1, begin = 0, end = 1, direction = 1) 
{
  colors <- rev(brewer.pal(11, "RdYlGn"))
  if (direction < 0) colors <- rev(colors)
  colorRampPalette(colors, alpha = alpha)(n)
}

palfunc2 <- function (n, alpha = 1, begin = 0, end = 1, direction = 1) 
{
  colors <- brewer.pal(11, "RdYlGn")
  if (direction < 0) colors <- rev(colors)
  colorRampPalette(colors, alpha = alpha)(n)
}

# Save plots 

jpeg("Floods_SM_RF.jpg", width = 800, height = 500)
spplot(r_yes, main="Floods Susceptibility Mapping using RF",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_RF.jpg", width = 800, height = 500)
spplot(r_no, main="Non Floods RF",col.regions=palfunc2)
dev.off()




#Run XGBoost function ------------------------------------------------

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        search = 'random') 

cl <- makePSOCKcluster(15)
registerDoParallel(cl)
clusterEvalQ(cl, .libPaths("F:/Mes_Docs/WorkingDIR/Library"))


set.seed(5)

fit.xgb_train <- train(Floods~., 
                       data=trainDat,
                       method = "xgbTree",
                       metric= "Accuracy",
                       preProc = c("center", "scale"), 
                       trControl = control)


save(fit.xgb_train,file = "fit_xgb.RData")

stopCluster(cl)


#Train xgbTree model USING aLL dependent data

set.seed(849)
fit.xgbAll<- train(Floods~., 
                   data=All_incidents,
                   method = "xgbTree",
                   metric= "Accuracy",
                   preProc = c("center", "scale"), 
                   trControl = myControl,
                   tuneLength = 10,
                   importance = TRUE)

save(fit.xgbAll, file = "fit_xgbAll.Rdata")

# Prediction

p<-as.data.frame(predict(fit.xgbAll, df_scaled[,-c(1,2)], type = "prob"))

df$Levels_yes<-p$yes
df$Levels_no<-p$no

x<-SpatialPointsDataFrame(as.data.frame(df)[, c("x", "y")], data = df)
r_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_yes)=CRS(projection(dem))

r_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_no)=CRS(projection(dem))


# Save prediction

writeRaster(r_yes,filename="Prediction_floods_xgb.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_no,filename="Prediction_non_floods_xgb.tif", format="GTiff", overwrite=TRUE) 


# Plot and save Maps

jpeg("Floods_SM_XGB.jpg", width = 800, height = 500)
spplot(r_yes, main="Floods Susceptibility Mapping using XGB",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_XGB.jpg", width = 800, height = 500)
spplot(r_no, main="Non Floods XGB",col.regions=palfunc2)
dev.off()

############################



#Run KNN function ------------------------------------------------

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)

set.seed(1)
knn = train(Floods~., 
                    data=trainDat,
                    method = "knn",
                    trControl = control)

save(knn, file = "fit_knn.Rdata")


#Train KNN model USING aLL dependent data

set.seed(849)
fit.KNNAll<- train(Floods~., 
                   data=All_incidents,
                   method = "knn",
                   trControl = control)

save(fit.KNNAll, file = "fit_knnAll.Rdata")

# Prediction 

p<-as.data.frame(predict(fit.KNNAll, df_scaled[,-c(1,2)], type = "prob"))
summary(p)

df$Levels_yes<-p$yes
df$Levels_no<-p$no

x<-SpatialPointsDataFrame(as.data.frame(df)[, c("x", "y")], data = df)

r_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_yes)=CRS(projection(dem))

r_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_no)=CRS(projection(dem))

# Save prediction rasters 

writeRaster(r_yes,filename="Prediction_floods_knn.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_no,filename="Prediction_non_floods_knn.tif", format="GTiff", overwrite=TRUE) 


# Plot and save Maps

jpeg("Floods_SM_KNN.jpg", width = 800, height = 500)
spplot(r_yes, main="Floods Susceptibility Mapping using KNN",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_KNN.jpg", width = 800, height = 500)
spplot(r_no, main="Non Floods KNN",col.regions=palfunc2)
dev.off()


######################

#Run nnet function ------------------------------------------------

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)

set.seed(1)
nnet_default = train(Floods~., 
                     data=trainDat,
                     method = "nnet",
                     trControl = control)

save(nnet_default,file = "fit_nnet.RData")

All_incidents <- merge(trainDat, testDat, all=TRUE)

#Train nnet model USING aLL dependent data

set.seed(849)
fit.nnetAll<- train(Floods~., 
                    data=All_incidents,
                    method = "nnet",
                    trControl = control)

save(fit.nnetAll,file = "fit_nnetAll.RData")

# Prediction

p<-as.data.frame(predict(fit.nnetAll, df_scaled[,-c(1,2)], type = "prob"))

df$Levels_yes<-p$yes
df$Levels_no<-p$no

x<-SpatialPointsDataFrame(as.data.frame(df)[, c("x", "y")], data = df)
r_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_yes)=CRS(projection(dem))

r_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_no)=CRS(projection(dem))

# Save prediction rasters 

writeRaster(r_yes,filename="Prediction_floods_nnet.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_no,filename="Prediction_non_floods_nnet.tif", format="GTiff", overwrite=TRUE) 


# Plot and save Maps

jpeg("Floods_SM_nnet.jpg", width = 800, height = 500)
spplot(r_yes, main="Floods Susceptibility Mapping using nnet",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_nnet.jpg", width = 800, height = 500)
spplot(r_no, main="Non Floods nnet",col.regions=palfunc2)
dev.off()
