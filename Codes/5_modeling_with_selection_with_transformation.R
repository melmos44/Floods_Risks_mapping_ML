
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
library(CAST)

setwd("F:/Mes_Docs/Souss/Floods/Data")


####################################################################################
#Modeling with transformation of categorical variables and variables selection
####################################################################################

# Load transformed train and test data

trainDat = read.xlsx("Train_Data_tr.xlsx")
testDat = read.xlsx("Test_Data_tr.xlsx")

trainDat$Floods = as.factor(trainDat$Floods)
testDat$Floods = as.factor(testDat$Floods)

###########

# Modeling ---------------------------------------------------------

# Train rf model

# Variables selection for rf

cl <- makePSOCKcluster(15)
registerDoParallel(cl)
clusterEvalQ(cl, .libPaths("F:/Mes_Docs/WorkingDIR/Library"))

train_control <- trainControl(method = 'cv', number = 10, returnResamp = 'all', 
                              classProbs = TRUE)

ffs_rf <- ffs(trainDat,trainDat$Floods,
              method="rf",
              trControl = train_control,
              tuneLength=3)

save(ffs_rf, file = "F:/Mes_Docs/Souss/Floods/Data/ffs_tr_rf.Rdata")

stopCluster(cl)

# Selected variables: Dem,Drainage_density,a2

trainDat = trainDat[,c("Floods","Dem", "Drainage_density","a2")]
testDat = testDat[,c("Floods","Dem", "Drainage_density","a2")]


# Define the control

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

save(rf_random, file = "fit_rf_tr_select.RData")


# Final model

All_incidents <- merge(trainDat, testDat, all=TRUE)


set.seed(849)
fit.rfAll<- train(Floods~., 
                  data=All_incidents,
                  method = "rf",
                  metric = "Accuracy",
                  trControl = control,
                  importance = TRUE)

save(fit.rfAll, file = "fit_rfAll_tr_select.Rdata")


# Load transformed rasters dataframe

df_scaled = get(load(file = "df_scaled_TV.RData"))


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

writeRaster(r_yes,filename="Prediction_floods_rf_tr_select.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_no,filename="Prediction_non_floods_rf_tr_select.tif", format="GTiff", overwrite=TRUE) 

# Plot Maps

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

jpeg("Floods_SM_RF_tr_select.jpg", width = 800, height = 500)
spplot(r_yes, main="Floods Susceptibility Mapping using RF",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_RF_tr_select.jpg", width = 800, height = 500)
spplot(r_no, main="Non Floods RF",col.regions=palfunc2)
dev.off()




#Run XGBoost function ------------------------------------------------

trainDat = read.xlsx("Train_Data_tr.xlsx")
testDat = read.xlsx("Test_Data_tr.xlsx")

trainDat$Floods = as.factor(trainDat$Floods)
testDat$Floods = as.factor(testDat$Floods)


# Modeling ---------------------------------------------------------

# Variables selection for XGBoost

cl <- makePSOCKcluster(15)
registerDoParallel(cl)
clusterEvalQ(cl, .libPaths("F:/Mes_Docs/WorkingDIR/Library"))

train_control <- trainControl(method = 'cv', number = 10, returnResamp = 'all', 
                              classProbs = TRUE)

ffs_xgb <- ffs(predictors = trainDat[,2:14], 
               response = trainDat$Floods,
               method = "xgbTree",
               trControl = train_control)

save(ffs_xgb, file = "F:/Mes_Docs/Souss/Floods/Data/ffs_tr_xgb.Rdata")

stopCluster(cl)


# Selected variables: Dem,Drainage_density,Distance_to_rivers

trainDat = trainDat[,c("Floods","Dem","Drainage_density","Distance_to_rivers")]
testDat = testDat[,c("Floods","Dem","Drainage_density","Distance_to_rivers")]


#Tunning prameters

myControl <- trainControl(method="repeatedcv", 
                          number=10, 
                          repeats=5,
                          returnResamp='all', 
                          allowParallel=TRUE)

# Step 5 modeling
set.seed(849)

fit.xgb_train<- train(Floods~., 
                      data=trainDat,
                      method = "xgbTree",
                      metric= "Accuracy",
                      preProc = c("center", "scale"), 
                      trControl = myControl,
                      tuneLength = 10)

save(fit.xgb_train, file = "fit_xgb_tr_select.Rdata")



All_incidents <- merge(trainDat, testDat, all=TRUE)

#Train xgbTree model USING aLL dependent data

set.seed(849)
fit.xgbAll<- train(Floods~., 
                   data=All_incidents,
                   method = "xgbTree",
                   metric= "Accuracy",
                   preProc = c("center", "scale"), 
                   trControl = myControl,
                   importance = TRUE)


save(fit.xgbAll, file = "fit_xgbAll_tr_select.Rdata")

# PRODUCE PROBABILITY MAP

p <-as.data.frame(predict(fit.rfAll, df_scaled[,-c(1,2)], type = "prob"))

df$Levels_yes<-p$yes
df$Levels_no<-p$no

x<-SpatialPointsDataFrame(as.data.frame(df)[, c("x", "y")], data = df)
r_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
r_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])

proj4string(r_yes)=CRS(projection(dem))

proj4string(r_no)=CRS(projection(dem))

# Save rasters

writeRaster(r_yes,filename="Prediction_floods_xgb_tr_select.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_no,filename="Prediction_non_floods_xgb_tr_select.tif", format="GTiff", overwrite=TRUE) 

# Plot Maps

jpeg("Floods_SM_XGB_tr_select.jpg", width = 800, height = 500)
spplot(r_yes, main="Floods Susceptibility Mapping using XGB",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_XGB_tr_select.jpg", width = 800, height = 500)
spplot(r_no, main="Non Floods XGB",col.regions=palfunc2)
dev.off()

############################



#Run KNN function ------------------------------------------------

trainDat = read.xlsx("Train_Data_tr.xlsx")
testDat = read.xlsx("Test_Data_tr.xlsx")

trainDat$Floods = as.factor(trainDat$Floods)
testDat$Floods = as.factor(testDat$Floods)


# Variables selection for KNN

cl <- makePSOCKcluster(15)
registerDoParallel(cl)
clusterEvalQ(cl, .libPaths("F:/Mes_Docs/WorkingDIR/Library"))

train_control <- trainControl(method = 'cv', number = 10, returnResamp = 'all', 
                              classProbs = TRUE)

ffs_knn <- ffs(predictors = trainDat[,2:14], 
               response = trainDat$Floods,
               method = "knn",
               trControl = train_control)

save(ffs_knn, file = "F:/Mes_Docs/Souss/Floods/Data/ffs_tr_knn.Rdata")

stopCluster(cl)


# Selected variables: Dem,Distance_to_rivers,Drainage_density

trainDat = trainDat[,c("Floods","Dem", "Distance_to_rivers","Drainage_density")]
testDat = testDat[,c("Floods","Dem", "Distance_to_rivers","Drainage_density")]


control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)

set.seed(1)
knn_default = train(Floods~., 
                    data=trainDat,
                    method = "knn",
                    trControl = control)

save(knn_default, file = "fit_knn_tr_select.Rdata")


All_incidents <- merge(trainDat, testDat, all=TRUE)

#Train KNN model USING aLL dependent data

set.seed(849)
fit.KNNAll<- train(Floods~., 
                   data=All_incidents,
                   method = "knn",
                   trControl = control)


save(fit.KNNAll, file = "fit_knnAll_tr_select.Rdata")


# PRODUCE PROBABILITY MAP

p <-as.data.frame(predict(fit.rfAll, df_scaled[,-c(1,2)], type = "prob"))

df$Levels_yes<-p$yes
df$Levels_no<-p$no

x<-SpatialPointsDataFrame(as.data.frame(df)[, c("x", "y")], data = df)
r_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
r_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])

proj4string(r_yes)=CRS(projection(dem))

proj4string(r_no)=CRS(projection(dem))

# Save rasters

writeRaster(r_yes,filename="Prediction_floods_knn_tr_select.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_no,filename="Prediction_non_floods_knn_tr_select.tif", format="GTiff", overwrite=TRUE) 

# Plot Maps

jpeg("Floods_SM_KNN_tr_select.jpg", width = 800, height = 500)
spplot(r_yes, main="Floods Susceptibility Mapping using KNN",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_KNN_tr_select.jpg", width = 800, height = 500)
spplot(r_no, main="Non Floods KNN",col.regions=palfunc2)
dev.off()





######################
#Run NNET function ------------------------------------------------

trainDat = read.xlsx("Train_Data_tr.xlsx")
testDat = read.xlsx("Test_Data_tr.xlsx")

trainDat$Floods = as.factor(trainDat$Floods)
testDat$Floods = as.factor(testDat$Floods)


# Variables selection for NNET

cl <- makePSOCKcluster(15)
registerDoParallel(cl)
clusterEvalQ(cl, .libPaths("F:/Mes_Docs/WorkingDIR/Library"))

train_control <- trainControl(method = 'cv', number = 10, returnResamp = 'all', 
                              classProbs = TRUE)

ffs_nnet <- ffs(predictors = trainDat[,2:14], 
                response = trainDat$Floods,
                method = "nnet",
                trControl = train_control)

save(ffs_nnet, file = "F:/Mes_Docs/Souss/Floods/Data/ffs_tr_nnet.Rdata")

stopCluster(cl)

# Selected variables: Dem,Distance_to_rivers,a1

trainDat = trainDat[,c("Floods","Dem", "Distance_to_rivers","a1")]
testDat = testDat[,c("Floods","Dem", "Distance_to_rivers","a1")]


#Run nnet function ------------------------------------------------

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)

set.seed(1)
nnet_default = train(Floods~., 
                     data=trainDat,
                     method = "nnet",
                     trControl = control)

save(nnet_default,file = "fit_nnet_tr_select.RData")

All_incidents <- merge(trainDat, testDat, all=TRUE)

#Train nnet model USING aLL dependent data

set.seed(849)
fit.nnetAll<- train(Floods~., 
                    data=All_incidents,
                    method = "nnet",
                    trControl = control)

save(fit.nnetAll,file = "fit_nnetAll_tr_select.RData")

Rasters.df_N_scaled = get(load(file = "Rasters.df_N_scaled_TV.RData"))

# PRODUCE PROBABILITY MAP

p <-as.data.frame(predict(fit.rfAll, df_scaled[,-c(1,2)], type = "prob"))

df$Levels_yes<-p$yes
df$Levels_no<-p$no

x<-SpatialPointsDataFrame(as.data.frame(df)[, c("x", "y")], data = df)
r_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
r_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])

proj4string(r_yes)=CRS(projection(dem))

proj4string(r_no)=CRS(projection(dem))

# Save rasters

writeRaster(r_yes,filename="Prediction_floods_nnet_tr_select.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_no,filename="Prediction_non_floods_nnet_tr_select.tif", format="GTiff", overwrite=TRUE) 


# Plot Maps

jpeg("Floods_SM_nnet_tr_select.jpg", width = 800, height = 500)
spplot(r_yes, main="Floods Susceptibility Mapping using nnet",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_nnet_tr_select.jpg", width = 800, height = 500)
spplot(r_no, main="Non Floods nnet",col.regions=palfunc2)
dev.off()
