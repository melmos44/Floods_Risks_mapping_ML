
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
library(data.table)


###################################################################################

##### Load test Data for each model

# Test data without transformation

setwd("F:/Mes_Docs/Souss/Floods/Data")

testDat = read.xlsx("Test_data.xlsx")
testDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
  lapply(testDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.factor)

testDat_rf = testDat
testDat_xgb = testDat
testDat_knn = testDat
testDat_nnet = testDat

testDat = testDat[,c("Floods","Distance_to_rivers", "Soil_type")]
testDat_rf_se = testDat


testDat = read.xlsx("Test_data.xlsx")
testDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
  lapply(testDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.factor)
testDat = testDat[,c("Floods","Dem","Rainfall","Distance_to_rivers")]
testDat_xgb_se = testDat

testDat = read.xlsx("Test_data.xlsx")
testDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
  lapply(testDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.factor)
testDat = testDat[,c("Floods","Dem", "Distance_to_rivers","Drainage_density")]
testDat_knn_se = testDat


testDat = read.xlsx("Test_data.xlsx")
testDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
  lapply(testDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.factor)
testDat = testDat[,c("Floods","Dem", "Distance_to_rivers")]
testDat_nnet_se = testDat

# Test data with transformation

testDat = read.xlsx("Test_data_tr.xlsx")

testDat_rf_tr = testDat
testDat_xgb_tr = testDat
testDat_knn_tr = testDat
testDat_nnet_tr = testDat


testDat = testDat[,c("Floods","Dem", "Drainage_density","a2")]
testDat_rf_tr_se = testDat

testDat = read.xlsx("Test_data_tr.xlsx")
testDat = testDat[,c("Floods","Dem","Drainage_density","Distance_to_rivers")]
testDat_xgb_tr_se = testDat

testDat = read.xlsx("Test_data_tr.xlsx")
testDat = testDat[,c("Floods","Dem", "Distance_to_rivers","Drainage_density")]
testDat_knn_tr_se = testDat

testDat = read.xlsx("Test_data_tr.xlsx")
testDat = testDat[,c("Floods","Dem", "Distance_to_rivers","a1")]
testDat_nnet_tr_se = testDat


###################################################################################
setwd("F:/Mes_Docs/Souss/Floods/Data/Models")

# Load Models

rf = get(load(file = "fit_rf.RData"))
rf_tr = get(load(file = "fit_rf_tr.RData"))
rf_se = get(load(file = "fit_rf_select.RData"))
rf_tr_se = get(load(file = "fit_rf_tr_select.RData"))

xgb = get(load(file = "fit_xgb.RData"))
xgb_tr = get(load(file = "fit_xgb_tr.RData"))
xgb_se = get(load(file = "fit_xgb_select.RData"))
xgb_tr_se = get(load(file = "fit_xgb_tr_select.RData"))


knn = get(load(file = "fit_knn.RData"))
knn_tr = get(load(file = "fit_knn_tr.RData"))
knn_se = get(load(file = "fit_knn_select.RData"))
knn_tr_se = get(load(file = "fit_knn_tr_select.RData"))

nnet = get(load(file = "fit_nnet.RData"))
nnet_tr = get(load(file = "fit_nnet_tr.RData"))
nnet_se = get(load(file = "fit_nnet_select.RData"))
nnet_tr_se = get(load(file = "fit_nnet_tr_select.RData"))


###################################################################################
# Test data prediction

p_rf <- predict(rf, testDat_rf, type = "raw")
p_rf_tr <- predict(rf_tr, testDat_rf_tr, type = "raw")
p_rf_se <- predict(rf_se, testDat_rf_se, type = "raw")
p_rf_tr_se <- predict(rf_tr_se, testDat_rf_tr_se, type = "raw")


p_xgb <- predict(xgb, testDat_xgb, type = "raw")
p_xgb_tr <- predict(xgb_tr, testDat_xgb_tr, type = "raw")
p_xgb_se <- predict(xgb_se, testDat_xgb_se, type = "raw")
p_xgb_tr_se <- predict(xgb_tr_se, testDat_xgb_tr_se, type = "raw")


p_knn <- predict(knn, testDat_knn, type = "raw")
p_knn_tr <- predict(knn_tr, testDat_knn_tr, type = "raw")
p_knn_se <- predict(knn_se, testDat_knn_se, type = "raw")
p_knn_tr_se <- predict(knn_tr_se, testDat_knn_tr_se, type = "raw")

p_nnet <- predict(nnet, testDat_nnet, type = "raw")
p_nnet_tr <- predict(nnet_tr, testDat_nnet_tr, type = "raw")
p_nnet_se <- predict(nnet_se, testDat_nnet_se, type = "raw")
p_nnet_tr_se <- predict(nnet_tr_se, testDat_nnet_tr_se, type = "raw")

###################################################################################
# confusion Matrix

confusionMatrix(p_rf, as.factor(testDat_rf$Floods))
confusionMatrix(p_rf_tr, as.factor(testDat_rf_tr$Floods))
confusionMatrix(p_rf_se, as.factor(testDat_rf_se$Floods))
confusionMatrix(p_rf_tr_se, as.factor(testDat_rf_tr_se$Floods))

confusionMatrix(p_xgb, as.factor(testDat_xgb$Floods))
confusionMatrix(p_xgb_tr, as.factor(testDat_xgb_tr$Floods))
confusionMatrix(p_xgb_se, as.factor(testDat_xgb_se$Floods))
confusionMatrix(p_xgb_tr_se, as.factor(testDat_xgb_tr_se$Floods))

confusionMatrix(p_knn, as.factor(testDat_knn$Floods))
confusionMatrix(p_knn_tr, as.factor(testDat_knn_tr$Floods))
confusionMatrix(p_knn_se, as.factor(testDat_knn_se$Floods))
confusionMatrix(p_knn_tr_se, as.factor(testDat_knn_tr_se$Floods))

confusionMatrix(p_nnet, as.factor(testDat_nnet$Floods))
confusionMatrix(p_nnet_tr, as.factor(testDat_nnet_tr$Floods))
confusionMatrix(p_nnet_se, as.factor(testDat_nnet_se$Floods))
confusionMatrix(p_nnet_tr_se, as.factor(testDat_nnet_tr_se$Floods))

###################################################################################
# ##  ROC 

pred_rf <- as.data.frame(predict(rf, testDat_rf, type = "prob"))
pred_rf$predict <- names(pred_rf)[1:2][apply(pred_rf[,1:2], 1, which.max)]
pred_rf$observed <- as.factor(testDat_rf$Floods)
roc_rf <- roc(ifelse(pred_rf$observed=="yes","no-yes","yes"), as.numeric(pred_rf$yes))


pred_rf_tr <- as.data.frame(predict(rf_tr, testDat_rf_tr, type = "prob"))
pred_rf_tr$predict <- names(pred_rf_tr)[1:2][apply(pred_rf_tr[,1:2], 1, which.max)]
pred_rf_tr$observed <- as.factor(testDat_rf_tr$Floods)
roc_rf_tr <- roc(ifelse(pred_rf_tr$observed=="yes","no-yes","yes"), as.numeric(pred_rf_tr$yes))


pred_rf_se <- as.data.frame(predict(rf_se, testDat_rf_se, type = "prob"))
pred_rf_se$predict <- names(pred_rf_se)[1:2][apply(pred_rf_se[,1:2], 1, which.max)]
pred_rf_se$observed <- as.factor(testDat_rf_se$Floods)
roc_rf_se <- roc(ifelse(pred_rf_se$observed=="yes","no-yes","yes"), as.numeric(pred_rf_se$yes))

pred_rf_tr_se <- as.data.frame(predict(rf_tr_se, testDat_rf_tr_se, type = "prob"))
pred_rf_tr_se$predict <- names(pred_rf_tr_se)[1:2][apply(pred_rf_tr_se[,1:2], 1, which.max)]
pred_rf_tr_se$observed <- as.factor(testDat_rf_tr_se$Floods)
roc_rf_tr_se <- roc(ifelse(pred_rf_tr_se$observed=="yes","no-yes","yes"), as.numeric(pred_rf_tr_se$yes))


pred_xgb <- as.data.frame(predict(xgb, testDat_xgb, type = "prob"))
pred_xgb$predict <- names(pred_xgb)[1:2][apply(pred_xgb[,1:2], 1, which.max)]
pred_xgb$observed <- as.factor(testDat_xgb$Floods)
roc_xgb <- roc(ifelse(pred_xgb$observed=="yes","no-yes","yes"), as.numeric(pred_xgb$yes))


pred_xgb_tr <- as.data.frame(predict(xgb_tr, testDat_xgb_tr, type = "prob"))
pred_xgb_tr$predict <- names(pred_xgb_tr)[1:2][apply(pred_xgb_tr[,1:2], 1, which.max)]
pred_xgb_tr$observed <- as.factor(testDat_xgb_tr$Floods)
roc_xgb_tr <- roc(ifelse(pred_xgb_tr$observed=="yes","no-yes","yes"), as.numeric(pred_xgb_tr$yes))


pred_xgb_se <- as.data.frame(predict(xgb_se, testDat_xgb_se, type = "prob"))
pred_xgb_se$predict <- names(pred_xgb_se)[1:2][apply(pred_xgb_se[,1:2], 1, which.max)]
pred_xgb_se$observed <- as.factor(testDat_xgb_se$Floods)
roc_xgb_se <- roc(ifelse(pred_xgb_se$observed=="yes","no-yes","yes"), as.numeric(pred_xgb_se$yes))

pred_xgb_tr_se <- as.data.frame(predict(xgb_tr_se, testDat_xgb_tr_se, type = "prob"))
pred_xgb_tr_se$predict <- names(pred_xgb_tr_se)[1:2][apply(pred_xgb_tr_se[,1:2], 1, which.max)]
pred_xgb_tr_se$observed <- as.factor(testDat_xgb_tr_se$Floods)
roc_xgb_tr_se <- roc(ifelse(pred_xgb_tr_se$observed=="yes","no-yes","yes"), as.numeric(pred_xgb_tr_se$yes))


pred_knn <- as.data.frame(predict(knn, testDat_knn, type = "prob"))
pred_knn$predict <- names(pred_knn)[1:2][apply(pred_knn[,1:2], 1, which.max)]
pred_knn$observed <- as.factor(testDat_knn$Floods)
roc_knn <- roc(ifelse(pred_knn$observed=="yes","no-yes","yes"), as.numeric(pred_knn$yes))


pred_knn_tr <- as.data.frame(predict(knn_tr, testDat_knn_tr, type = "prob"))
pred_knn_tr$predict <- names(pred_knn_tr)[1:2][apply(pred_knn_tr[,1:2], 1, which.max)]
pred_knn_tr$observed <- as.factor(testDat_knn_tr$Floods)
roc_knn_tr <- roc(ifelse(pred_knn_tr$observed=="yes","no-yes","yes"), as.numeric(pred_knn_tr$yes))


pred_knn_se <- as.data.frame(predict(knn_se, testDat_knn_se, type = "prob"))
pred_knn_se$predict <- names(pred_knn_se)[1:2][apply(pred_knn_se[,1:2], 1, which.max)]
pred_knn_se$observed <- as.factor(testDat_knn_se$Floods)
roc_knn_se <- roc(ifelse(pred_knn_se$observed=="yes","no-yes","yes"), as.numeric(pred_knn_se$yes))

pred_knn_tr_se <- as.data.frame(predict(knn_tr_se, testDat_knn_tr_se, type = "prob"))
pred_knn_tr_se$predict <- names(pred_knn_tr_se)[1:2][apply(pred_knn_tr_se[,1:2], 1, which.max)]
pred_knn_tr_se$observed <- as.factor(testDat_knn_tr_se$Floods)
roc_knn_tr_se <- roc(ifelse(pred_knn_tr_se$observed=="yes","no-yes","yes"), as.numeric(pred_knn_tr_se$yes))


pred_nnet <- as.data.frame(predict(nnet, testDat_nnet, type = "prob"))
pred_nnet$predict <- names(pred_nnet)[1:2][apply(pred_nnet[,1:2], 1, which.max)]
pred_nnet$observed <- as.factor(testDat_nnet$Floods)
roc_nnet <- roc(ifelse(pred_nnet$observed=="yes","no-yes","yes"), as.numeric(pred_nnet$yes))


pred_nnet_tr <- as.data.frame(predict(nnet_tr, testDat_nnet_tr, type = "prob"))
pred_nnet_tr$predict <- names(pred_nnet_tr)[1:2][apply(pred_nnet_tr[,1:2], 1, which.max)]
pred_nnet_tr$observed <- as.factor(testDat_nnet_tr$Floods)
roc_nnet_tr <- roc(ifelse(pred_nnet_tr$observed=="yes","no-yes","yes"), as.numeric(pred_nnet_tr$yes))


pred_nnet_se <- as.data.frame(predict(nnet_se, testDat_nnet_se, type = "prob"))
pred_nnet_se$predict <- names(pred_nnet_se)[1:2][apply(pred_nnet_se[,1:2], 1, which.max)]
pred_nnet_se$observed <- as.factor(testDat_nnet_se$Floods)
roc_nnet_se <- roc(ifelse(pred_nnet_se$observed=="yes","no-yes","yes"), as.numeric(pred_nnet_se$yes))

pred_nnet_tr_se <- as.data.frame(predict(nnet_tr_se, testDat_nnet_tr_se, type = "prob"))
pred_nnet_tr_se$predict <- names(pred_nnet_tr_se)[1:2][apply(pred_nnet_tr_se[,1:2], 1, which.max)]
pred_nnet_tr_se$observed <- as.factor(testDat_nnet_tr_se$Floods)
roc_nnet_tr_se <- roc(ifelse(pred_nnet_tr_se$observed=="yes","no-yes","yes"), as.numeric(pred_nnet_tr_se$yes))


###################################################################################
# auc

AUC_rf = roc_rf$auc
AUC_rf_tr = roc_rf_tr$auc
AUC_rf_se = roc_rf_se$auc
AUC_rf_tr_se = roc_rf_tr_se$auc

AUC_xgb = roc_xgb$auc
AUC_xgb_tr = roc_xgb_tr$auc
AUC_xgb_se = roc_xgb_se$auc
AUC_xgb_tr_se = roc_xgb_tr_se$auc


AUC_knn = roc_knn$auc
AUC_knn_tr = roc_knn_tr$auc
AUC_knn_se = roc_knn_se$auc
AUC_knn_tr_se = roc_knn_tr_se$auc

AUC_nnet = roc_nnet$auc
AUC_nnet_tr = roc_nnet_tr$auc
AUC_nnet_se = roc_nnet_se$auc
AUC_nnet_tr_se = roc_nnet_tr_se$auc


AUC = list(RF=AUC_rf,RF_TR=AUC_rf_tr,RF_SE=AUC_rf_se,RF_TR_SE=AUC_rf_tr_se,
           XGB=AUC_xgb,XGB_TR=AUC_xgb_tr,XGB_SE=AUC_xgb_se,XGB_TR_SE=AUC_xgb_tr_se,
           KNN=AUC_knn,KNN_TR=AUC_knn_tr,KNN_SE=AUC_knn_se,KNN_TR_SE=AUC_knn_tr_se,
           NNET=AUC_nnet,NNET_TR=AUC_nnet_tr,NNET_SE=AUC_nnet_se,NNET_TR_SE=AUC_nnet_tr_se)

AUC = as.data.frame(unlist(AUC))

AUC = setDT(AUC, keep.rownames = TRUE)[]

names(AUC) = c("Models","AUC")

AUC$AUC = round(AUC$AUC,digits=3)

AUC = as.data.frame(AUC)

write.xlsx(AUC,"AUC.xlsx")

rf = AUC %>% slice(1:4)
xgb = AUC %>% slice(5:8)
knn = AUC %>% slice(9:12)
nnet = AUC %>% slice(13:16)

h1 = ggplot(knn, aes(x=Models, y=AUC)) + 
  geom_bar(stat = "identity",width=0.2)

h1 = ggplot(data=knn, aes(x=Models, y=AUC)) +
  geom_bar(stat="identity", color="blue", fill="white", width=0.5)+
  geom_text(aes(label=AUC), vjust=-0.3, size=3.5)+
  theme_minimal()

h2 = ggplot(data=nnet, aes(x=Models, y=AUC)) +
  geom_bar(stat="identity", color="blue", fill="white", width=0.5)+
  geom_text(aes(label=AUC), vjust=-0.3, size=3.5)+
  theme_minimal()

h3 = ggplot(data=rf, aes(x=Models, y=AUC)) +
  geom_bar(stat="identity", color="blue", fill="white", width=0.5)+
  geom_text(aes(label=AUC), vjust=-0.3, size=3.5)+
  theme_minimal()

h4 = ggplot(data=xgb, aes(x=Models, y=AUC)) +
  geom_bar(stat="identity", color="blue", fill="white", width=0.5)+
  geom_text(aes(label=AUC), vjust=-0.3, size=3.5)+
  theme_minimal()

grid.arrange(h1, h2,h3,h4, ncol=2)

png("AUC_models.png",
    width=17,height=22,units="cm",res = 600)
grid.arrange(h1, h2,h3,h4, ncol=2)
dev.off()

AUC2 = AUC

AUC2$Models = gsub("RF.*","RF",AUC2$Models)
AUC2$Models = gsub("XGB.*","XGB",AUC2$Models)
AUC2$Models = gsub("KNN.*","KNN",AUC2$Models)
AUC2$Models = gsub("NNET.*","NNET",AUC2$Models)

AUC3 = AUC2 %>% group_by(Models) %>% summarise(AUC=mean(AUC))

ggplot(AUC3) + 
  geom_col(aes(x = forcats::fct_reorder(Models, AUC), 
               y = AUC, fill = Models),width=0.5) +
  coord_flip() +
  scale_fill_manual(values = c("green4", "red", "cyan4","blue")) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank()) +
  #ggtitle("Variable importance of the models") +
  ylab("AUC") +
  xlab("")

qplot(Models, AUC, data=AUC2, geom=c("boxplot", "jitter"),
      fill=Models, xlab="Models", ylab="AUC")

setwd("F:/Mes_Docs/Souss/Floods/Data/Results")
jpeg("AUC_models.jpg", width = 800, height = 500)
qplot(Models, AUC, data=AUC2, geom=c("boxplot", "jitter"),
      fill=Models, xlab="Models", ylab="AUC")
dev.off()

###################################################################################

# ROC plot

list_roc = list(RF=roc_rf, RF_TR=roc_rf_tr, RF_SE=roc_rf_se, RF_TR_SE = roc_rf_tr_se,
                XGB=roc_xgb, XGB_TR=roc_xgb_tr, XGB_SE=roc_xgb_se, XGB_TR_SE = roc_xgb_tr_se,
                KNN=roc_knn, KNN_TR=roc_knn_tr, KNN_SE=roc_knn_se, KNN_TR_SE = roc_knn_tr_se,
                NNET=roc_nnet, NNET_TR=roc_nnet_tr, NNET_SE=roc_nnet_se, NNET_TR_SE = roc_nnet_tr_se)

list_roc3 = list(rf=roc_rf, rf_tr=roc_rf_tr, rf_se=roc_rf_se, rf_tr_se = roc_rf_tr_se)

list_roc4 = list(xgb=roc_xgb, xgb_tr=roc_xgb_tr, xgb_se=roc_xgb_se, xgb_tr_se = roc_xgb_tr_se)

list_roc1 = list(knn=roc_knn, knn_tr=roc_knn_tr, knn_se=roc_knn_se, knn_tr_se = roc_knn_tr_se)

list_roc2 = list(nnet=roc_nnet, nnet_tr=roc_nnet_tr, nnet_se=roc_nnet_se, nnet_tr_se = roc_nnet_tr_se)


p1 = ggroc(list_roc1)+facet_grid(.~name) + theme(legend.position="none") + theme(axis.text.x = element_blank())
p2 = ggroc(list_roc2)+facet_grid(.~name) + theme(legend.position="none")+ theme(axis.text.x = element_blank())
p3 = ggroc(list_roc3)+facet_grid(.~name) + theme(legend.position="none")+ theme(axis.text.x = element_blank())
p4 = ggroc(list_roc4)+facet_grid(.~name) + theme(legend.position="none")+ theme(axis.text.x = element_blank())

require(gridExtra)
grid.arrange(p1, p2,p3,p4, ncol=2)


png("ROC_models.png",
    width=17,height=22,units="cm",res = 600)
grid.arrange(p1, p2,p3,p4, ncol=2)
dev.off()


ggrocs <- function(rocs, breaks = seq(0,1,0.1), legendTitel = "Legend") {
  if (length(rocs) == 0) {
    stop("No ROC objects available in param rocs.")
  } else {
    require(plyr)
    # Store all sensitivities and specifivities in a data frame
    # which an be used in ggplot
    RocVals <- plyr::ldply(names(rocs), function(rocName) {
      if(class(rocs[[rocName]]) != "roc") {
        stop("Please provide roc object from pROC package")
      }
      data.frame(
        fpr = rev(rocs[[rocName]]$specificities),
        tpr = rev(rocs[[rocName]]$sensitivities),
        names = rep(rocName, length(rocs[[rocName]]$sensitivities)),
        stringAsFactors = T
      )
    })
    
    rocPlot <- ggplot(RocVals, aes(x = fpr, y = tpr, colour = names), size=2) +
      geom_segment(aes(x = 0, y = 1, xend = 1,yend = 0), alpha = 0.5, colour = "gray") + 
      geom_line(size=1) + 
      scale_x_reverse(name = "False Positive Rate (1 - Specificity)",limits = c(1,0), breaks = breaks) + 
      scale_y_continuous(name = "True Positive Rate (Sensitivity)", limits = c(0,1), breaks = breaks) +
      theme_bw() + 
      coord_equal() +
      annotate("text",x = 0.22, y = 0.76, vjust = 0, hjust = 0,size = 2.1,label= "AUC(RF) = 0.967") + 
      annotate("text",x = 0.22, y = 0.71, vjust = 0, hjust = 0,size = 2.1,label= "AUC(RF_TR) = 0.967") + 
      annotate("text",x = 0.22, y = 0.66, vjust = 0, hjust = 0,size = 2.1,label= "AUC(RF_SE) = 0.930") + 
      annotate("text",x = 0.22, y = 0.61, vjust = 0, hjust = 0,size = 2.1,label= "AUC(RF_TR_SE) = 0.981") + 
      annotate("text",x = 0.22, y = 0.56, vjust = 0, hjust = 0,size = 2.1,label= "AUC(XGB) = 0.959") + 
      annotate("text",x = 0.22, y = 0.51, vjust = 0, hjust = 0,size = 2.1,label= "AUC(XGB_TR) = 0.941") + 
      annotate("text",x = 0.22, y = 0.46, vjust = 0, hjust = 0,size = 2.1,label= "AUC(XGB_SE) = 0.972") + 
      annotate("text",x = 0.22, y = 0.41, vjust = 0, hjust = 0,size = 2.1,label= "AUC(XGB)_TR_SE = 0.944") + 
      annotate("text",x = 0.22, y = 0.36, vjust = 0, hjust = 0,size = 2.1,label= "AUC(KNN) = 0.946") + 
      annotate("text",x = 0.22, y = 0.31, vjust = 0, hjust = 0,size = 2.1,label= "AUC(KNN_TR) = 0.951") + 
      annotate("text",x = 0.22, y = 0.26, vjust = 0, hjust = 0,size = 2.1,label= "AUC(KNN_SE) = 0.986") + 
      annotate("text",x = 0.22, y = 0.21, vjust = 0, hjust = 0,size = 2.1,label= "AUC(KNN_TR_SE) = 0.986") + 
      annotate("text",x = 0.22, y = 0.16, vjust = 0, hjust = 0,size = 2.1,label= "AUC(NNET) = 0.933") + 
      annotate("text",x = 0.22, y = 0.11, vjust = 0, hjust = 0,size = 2.1,label= "AUC(NNET_TR) = 0.959") + 
      annotate("text",x = 0.22, y = 0.06, vjust = 0, hjust = 0,size = 2.1,label= "AUC(NNET_SE) = 0.954") + 
      annotate("text",x = 0.22, y = 0.01, vjust = 0, hjust = 0,size = 2.1,label= "AUC(NNET_TR_SE) = 0.954") +
      guides(colour = guide_legend(legendTitel)) +
      theme(axis.ticks = element_line(color = "grey80"))
    
    rocPlot
  }
}


ggrocs <- function(rocs, breaks = seq(0,1,0.1), legendTitel = "Legend") {
  if (length(rocs) == 0) {
    stop("No ROC objects available in param rocs.")
  } else {
    require(plyr)
    # Store all sensitivities and specifivities in a data frame
    # which an be used in ggplot
    RocVals <- plyr::ldply(names(rocs), function(rocName) {
      if(class(rocs[[rocName]]) != "roc") {
        stop("Please provide roc object from pROC package")
      }
      data.frame(
        fpr = rev(rocs[[rocName]]$specificities),
        tpr = rev(rocs[[rocName]]$sensitivities),
        names = rep(rocName, length(rocs[[rocName]]$sensitivities)),
        stringAsFactors = T
      )
    })
    
    rocPlot <- ggplot(RocVals, aes(x = fpr, y = tpr, colour = names), size=2) +
      geom_segment(aes(x = 0, y = 1, xend = 1,yend = 0), alpha = 0.5, colour = "gray") + 
      geom_line(size=1) + 
      scale_x_reverse(name = "False Positive Rate (1 - Specificity)",limits = c(1,0), breaks = breaks) + 
      scale_y_continuous(name = "True Positive Rate (Sensitivity)", limits = c(0,1), breaks = breaks) +
      theme_bw() + 
      coord_equal() +
      annotate("text",x = 0.175, y = 0.2, vjust = 0, hjust = 0,size = 3,label= "AUC(RF) = 0.967") +
      annotate("text",x = 0.175, y = 0.15, vjust = 0, hjust = 0,size = 3,label= "AUC(XGB) = 0.959") + 
      annotate("text",x = 0.175, y = 0.1, vjust = 0, hjust = 0,size = 3,label= "AUC(KNN) = 0.946") + 
      annotate("text",x = 0.175, y = 0.05, vjust = 0, hjust = 0,size = 3,label= "AUC(NNET) = 0.933") +
      guides(colour = guide_legend(legendTitel)) +
      theme(axis.ticks = element_line(color = "grey80"))
    
    rocPlot
  }
}

par(mfrow=c(2,2))
ggrocs(list_roc5)

jpeg("ROC_models2.png", width =1200, height = 900)
ggrocs(list_roc5)
dev.off()

png("ROC_models.png",
    width=17,height=22,units="cm",res = 600)
ggrocs(list_roc)
dev.off()

###################################################################################
##############################################################################

# Box and dot plots of Accuracy

results <- resamples(list(RF=rf, RF_TR=rf_tr, RF_SE=rf_se, RF_TR_SE=rf_tr_se,
                           XGB=xgb, XGB_TR=xgb_tr, XGB_SE=xgb_se, XGB_TR_SE=xgb_tr_se,
                          KNN=knn,KNN_TR=knn_tr, KNN_SE=knn_se, KNN_TR_SE=knn_tr_se,
                          NNET=nnet, NNET_TR=nnet_tr, NNET_SE=nnet_se, NNET_TR_SE = nnet_tr_se))

# summarize differences between modes
summary(results)


# box and whisker plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)

jpeg("Bwplot_models.png", width = 800, height = 500)
bwplot(results, scales=scales)
dev.off()


# dot plots of accuracy
scales <- list(x=list(relation="free"), y=list(relation="free"))
dotplot(results, scales=scales)

jpeg("Dotplot_models.png", width = 800, height = 500)
dotplot(results, scales=scales)
dev.off()
###################################################################################

varImp(rf)
varImp(knn)
varImp(nnet)
varImp(xgb)

df = as.data.frame(varimp)

ggplot(df) + 
  geom_col(aes(x = forcats::fct_reorder(Variables, Importance), 
               y = Importance, fill = Models)) +
  coord_flip() +
  scale_fill_manual(values = c("grey", "black", "green4","blue4")) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank()) +
  #ggtitle("Variable importance of the models") +
  ylab("Variable importance") +
  xlab("")


png("Varimp.png",
    width=17,height=22,units="cm",res = 600)

ggplot(df) + 
  geom_col(aes(x = forcats::fct_reorder(Variables, Importance), 
               y = Importance, fill = Models)) +
  coord_flip() +
  scale_fill_manual(values = c("grey", "black", "green4","blue4")) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank()) +
  #ggtitle("Variable importance of the models") +
  ylab("Variable importance") +
  xlab("")

dev.off()



