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


########################################################################################
# Modeling with transformation of categorical variables and without variables selection
#######################################################################################

# Load training and testing data

trainDat = read.xlsx("Train_Data.xlsx")
testDat = read.xlsx("Test_Data.xlsx")

# Convert categorical variables to numerical

trainDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
  lapply(trainDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.numeric)

testDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
  lapply(testDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.numeric)


######### Categorical variables transformation

# Dealing with training data

# Aspect setting

AspectTr<-cut(trainDat$Aspect, seq(0,9,1), right=FALSE, labels=c("a0","a1","a2","a3","a4","a5","a6","a7","a8"))
table(AspectTr)
class(AspectTr)

flags = data.frame(Reduce(cbind,lapply(levels(AspectTr),function(x){(AspectTr == x)*1})
))
names(flags) = levels(AspectTr)
trainDat = cbind(trainDat, flags)

# Remove the original Aspect
trainDat <- trainDat[,-c(10,15)]


# Flow_Direction setting

Flow_DirectionTr<-cut(trainDat$Flow_Direction, seq(0,9,1), right=FALSE, labels=c("fd0","fd1","fd2","fd3","fd4","fd5","fd6","fd7","fd8"))
table(Flow_DirectionTr)
class(Flow_DirectionTr)


flags = data.frame(Reduce(cbind,lapply(levels(Flow_DirectionTr),function(x){(Flow_DirectionTr == x)*1})
))
names(flags) = levels(Flow_DirectionTr)
trainDat = cbind(trainDat, flags)

# Remove the original Flow_Direction
trainDat <- trainDat[,-c(10,22)]


# Geology setting

GeologyTr<-cut(trainDat$Geology, seq(0,8,1), right=FALSE, labels=c("g0","g1","g2","g3","g4","g5","g6","g7"))
table(GeologyTr)
class(GeologyTr)


flags = data.frame(Reduce(cbind,lapply(levels(GeologyTr),function(x){(GeologyTr == x)*1})
))
names(flags) = levels(GeologyTr)
trainDat = cbind(trainDat, flags)

# Remove the original Geology
trainDat <- trainDat[,-c(10,29)]


# Landuse setting

LanduseTr<-cut(trainDat$Landuse, seq(0,6,1), right=FALSE, labels=c("l0","l1","l2","l3","l4","l5"))
table(LanduseTr)
class(LanduseTr)


flags = data.frame(Reduce(cbind,lapply(levels(LanduseTr),function(x){(LanduseTr == x)*1})
))
names(flags) = levels(LanduseTr)
trainDat = cbind(trainDat, flags)

# Remove the original Landuse
trainDat <- trainDat[,-c(10,35)]


# Soil_type setting

Soil_typeTr<-cut(trainDat$Soil_type, seq(0,7,1), right=FALSE, labels=c("st0","st1","st2","st3","st4","st5","st6"))
table(Soil_typeTr)
class(Soil_typeTr)


flags = data.frame(Reduce(cbind,lapply(levels(Soil_typeTr),function(x){(Soil_typeTr == x)*1})
))
names(flags) = levels(Soil_typeTr)
trainDat = cbind(trainDat, flags)

# Remove the original Soil_type
trainDat <- trainDat[,-c(10,39)]


# Dealing with testing data


# Aspect setting

AspectTr<-cut(testDat$Aspect, seq(0,9,1), right=FALSE, labels=c("a0","a1","a2","a3","a4","a5","a6","a7","a8"))
table(AspectTr)
class(AspectTr)


flags = data.frame(Reduce(cbind,lapply(levels(AspectTr),function(x){(AspectTr == x)*1})
))
names(flags) = levels(AspectTr)
testDat = cbind(testDat, flags)

# Remove the original Aspect
testDat <- testDat[,-c(10,15)]


# Flow_Direction setting

Flow_DirectionTr<-cut(testDat$Flow_Direction, seq(0,9,1), right=FALSE, labels=c("fd0","fd1","fd2","fd3","fd4","fd5","fd6","fd7","fd8"))
table(Flow_DirectionTr)
class(Flow_DirectionTr)


flags = data.frame(Reduce(cbind,lapply(levels(Flow_DirectionTr),function(x){(Flow_DirectionTr == x)*1})
))
names(flags) = levels(Flow_DirectionTr)
testDat = cbind(testDat, flags)

# Remove the original Flow_Direction
testDat <- testDat[,-c(10,22)]


# Geology setting

GeologyTr<-cut(testDat$Geology, seq(0,8,1), right=FALSE, labels=c("g0","g1","g2","g3","g4","g5","g6","g7"))
table(GeologyTr)
class(GeologyTr)


flags = data.frame(Reduce(cbind,lapply(levels(GeologyTr),function(x){(GeologyTr == x)*1})
))
names(flags) = levels(GeologyTr)
testDat = cbind(testDat, flags)

# Remove the original Geology
testDat <- testDat[,-c(10,29)]


# Landuse setting

LanduseTr<-cut(testDat$Landuse, seq(0,6,1), right=FALSE, labels=c("l0","l1","l2","l3","l4","l5"))
table(LanduseTr)
class(LanduseTr)


flags = data.frame(Reduce(cbind,lapply(levels(LanduseTr),function(x){(LanduseTr == x)*1})
))
names(flags) = levels(LanduseTr)
testDat = cbind(testDat, flags)

# Remove the original Landuse
testDat <- testDat[,-c(10,35)]


# Soil_type setting

Soil_typeTr<-cut(testDat$Soil_type, seq(0,7,1), right=FALSE, labels=c("st0","st1","st2","st3","st4","st5","st6"))
table(Soil_typeTr)
class(Soil_typeTr)


flags = data.frame(Reduce(cbind,lapply(levels(Soil_typeTr),function(x){(Soil_typeTr == x)*1})
))
names(flags) = levels(Soil_typeTr)
testDat = cbind(testDat, flags)

# Remove the original Soil_type
testDat <- testDat[,-c(10,39)]


# Save transformed train and test data

write.xlsx(scaled_t,"Train_Data_tr.xlsx")
write.xlsx(scaled_tst,"Test_Data_tr.xlsx")


# Modeling ---------------------------------------------------------

# Train rf model

# Define the control
trControl <- trainControl(method='repeatedcv', 
                          repeats=3,
                          number = 10,
                          search = "grid")

set.seed(1)
rf_random <- train(Training~., 
                   data=trainDat,
                   method = 'rf',
                   metric = 'Accuracy',
                   trControl = control,
                   importance = TRUE)

save(rf_random, file = "fit_rf_tr.RData")


# Final model

All_incidents <- merge(trainDat, testDat, all=TRUE)

set.seed(849)
fit.rfAll<- train(Training~., 
                  data=All_incidents,
                  method = "rf",
                  metric = "Accuracy",
                  trControl = control,
                  importance = TRUE)

save(fit.rfAll, file = "fit_rfAll_tr.Rdata")

fit.rfAll = get(load(file = "F:/Mes_Docs/Souss/Floods/Data/fit_rfAll_tr.Rdata"))


# Load rasters dataframe

df_scaled = get(load(file = "df_scaled_OV.RData"))

# Transform categorical data 

# convert to numerical 

df_scaled[,c("Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
  lapply(df_scaled[,c("Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.numeric)

# Aspect setting

AspectTr<-cut(df_scaled$Aspect, seq(0,9,1), right=FALSE, labels=c("a0","a1","a2","a3","a4","a5","a6","a7","a8"))
table(AspectTr) 
class(AspectTr) 


flags = data.frame(Reduce(cbind,lapply(levels(AspectTr),function(x){(AspectTr == x)*1})
))
names(flags) = levels(AspectTr)
df_scaled = cbind(df_scaled, flags) 

# Remove the original Aspect 
df_scaled <- df_scaled[,-c(11,16)] 


# Flow_Direction setting

Flow_DirectionTr<-cut(df_scaled$Flow_Direction, seq(0,9,1), right=FALSE, labels=c("fd0","fd1","fd2","fd3","fd4","fd5","fd6","fd7","fd8"))
table(Flow_DirectionTr) 
class(Flow_DirectionTr) 


flags = data.frame(Reduce(cbind,lapply(levels(Flow_DirectionTr),function(x){(Flow_DirectionTr == x)*1})
))
names(flags) = levels(Flow_DirectionTr)
df_scaled = cbind(df_scaled, flags) 

# Remove the original Flow_Direction 
df_scaled <- df_scaled[,-c(11,23)] 


# Geology setting

GeologyTr<-cut(df_scaled$Geology, seq(0,8,1), right=FALSE, labels=c("g0","g1","g2","g3","g4","g5","g6","g7"))
table(GeologyTr) 
class(GeologyTr) 


flags = data.frame(Reduce(cbind,lapply(levels(GeologyTr),function(x){(GeologyTr == x)*1})
))
names(flags) = levels(GeologyTr)
df_scaled = cbind(df_scaled, flags)

# Remove the original Geology 
df_scaled <- df_scaled[,-c(11,30)]


# Landuse setting

LanduseTr<-cut(df_scaled$Landuse, seq(0,6,1), right=FALSE, labels=c("l0","l1","l2","l3","l4","l5"))
table(LanduseTr) 
class(LanduseTr) 


flags = data.frame(Reduce(cbind,lapply(levels(LanduseTr),function(x){(LanduseTr == x)*1})
))
names(flags) = levels(LanduseTr)
df_scaled = cbind(df_scaled, flags) 

# Remove the original Landuse 
df_scaled <- df_scaled[,-c(11,36)] 


# Soil_type setting

Soil_typeTr<-cut(df_scaled$Soil_type, seq(0,7,1), right=FALSE, labels=c("st0","st1","st2","st3","st4","st5","st6"))
table(Soil_typeTr) 
class(Soil_typeTr) 


flags = data.frame(Reduce(cbind,lapply(levels(Soil_typeTr),function(x){(Soil_typeTr == x)*1})
))
names(flags) = levels(Soil_typeTr)
df_scaled = cbind(df_scaled, flags)

# Remove the original Soil_type 
df_scaled <- df_scaled[,-c(11,40)]


# Omit na

df_scaled = df_scaled[complete.cases(df_scaled),]

str(df_scaled)

save(df_scaled, file = "df_scaled_TV.RData")


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

writeRaster(r_yes,filename="Prediction_floods_rf_tr.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_no,filename="Prediction_non_floods_rf_tr.tif", format="GTiff", overwrite=TRUE) 

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

jpeg("Floods_SM_RF_tr.jpg", width = 800, height = 500)
spplot(r_yes, main="Floods Susceptibility Mapping using RF",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_RF_tr.jpg", width = 800, height = 500)
spplot(r_no, main="Non Floods RF",col.regions=palfunc2)
dev.off()




#Run XGBoost function ------------------------------------------------

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        search = 'random') 

set.seed(5)

fit.xgb_train <- train(Floods~., 
                       data=trainDat,
                       method = "xgbTree",
                       metric= "Accuracy",
                       preProc = c("center", "scale"), 
                       trControl = control)


save(fit.xgb_train, file = "fit_xgb_tr.Rdata")


#Train xgbTree model USING aLL dependent data

set.seed(849)
fit.xgbAll<- train(Training~., 
                   data=All_incidents,
                   method = "xgbTree",
                   metric= "Accuracy",
                   preProc = c("center", "scale"), 
                   trControl = myControl,
                   tuneLength = 10,
                   importance = TRUE)

save(fit.xgbAll, file = "fit_xgbAll_tr.Rdata")



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

writeRaster(r_yes,filename="Prediction_floods_xgb_tr.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_no,filename="Prediction_non_floods_xgb_tr.tif", format="GTiff", overwrite=TRUE) 

# Plot Maps

jpeg("Floods_SM_XGB_tr.jpg", width = 800, height = 500)
spplot(r_yes, main="Floods Susceptibility Mapping using XGB",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_XGB_tr.jpg", width = 800, height = 500)
spplot(r_no, main="Non Floods XGB",col.regions=palfunc2)
dev.off()

############################



#Run KNN function ------------------------------------------------

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)

set.seed(1)
knn_default = train(Training~., 
                    data=scaled_t,
                    method = "knn",
                    trControl = control)

save(knn_default, file = "fit_knn_tr.Rdata")

#Train KNN model USING aLL dependent data

set.seed(849)
fit.KNNAll<- train(Training~., 
                   data=All_incidents,
                   method = "knn",
                   trControl = control)

save(fit.KNNAll, file = "fit_knnAll_tr.Rdata")


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

writeRaster(r_yes,filename="Prediction_floods_knn_tr.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_no,filename="Prediction_non_floods_knn_tr.tif", format="GTiff", overwrite=TRUE) 

# Plot Maps

jpeg("Floods_SM_KNN_tr.jpg", width = 800, height = 500)
spplot(r_yes, main="Floods Susceptibility Mapping using KNN",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_KNN_tr.jpg", width = 800, height = 500)
spplot(r_no, main="Non Floods KNN",col.regions=palfunc2)
dev.off()


######################

trainDat = read.xlsx("Train_Data_tr.xlsx")
testDat = read.xlsx("Test_Data_tr.xlsx")

names(trainDat)[1] = "Floods"
names(testDat)[1] = "Floods"

trainDat$Floods = as.factor(trainDat$Floods)
testDat$Floods = as.factor(testDat$Floods)

#Run nnet function ------------------------------------------------

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)

set.seed(1)
nnet_default = train(Floods~., 
                     data=trainDat,
                     method = "nnet",
                     trControl = control)

save(nnet_default,file = "fit_nnet_tr.RData")

All_incidents <- merge(trainDat, testDat, all=TRUE)

#Train nnet model USING aLL dependent data

set.seed(849)
fit.nnetAll<- train(Floods~., 
                    data=All_incidents,
                    method = "nnet",
                    trControl = control)

save(fit.nnetAll,file = "fit_nnetAll_tr.RData")

df_scaled = get(load(file = "df_scaled_TV.RData"))

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

writeRaster(r_yes,filename="Prediction_floods_nnet_tr.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_no,filename="Prediction_non_floods_nnet_tr.tif", format="GTiff", overwrite=TRUE) 


# Plot Maps


jpeg("Floods_SM_nnet_tr.jpg", width = 800, height = 500)
spplot(r_yes, main="Floods Susceptibility Mapping using nnet",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_nnet_tr.jpg", width = 800, height = 500)
spplot(r_no, main="Non Floods nnet",col.regions=palfunc2)
dev.off()
