# UTS
# Variabel target = V7 (status inflamation)  
data <- read.csv("C:/Users/ACER/Documents/Panii/MATKUL SD/PPT/Kodingan R/DMKM/diagnosis.data", header = FALSE, fileEncoding = "utf-16", sep = ";")
library(caret)
library(adabag)
library(e1071)
library(dplyr)
View(data)
str(data)
data1 <- select(data, V1, V2, V3, V4, V5, V6, V7)
indexes=createDataPartition(data1$V7, p=0.9, list = F)
datatraining1 <- data1[indexes,]
datatesting1<-data1[-indexes,]
print(paste("Jumlah data training  adalah ", nrow(datatraining1)))
print(paste("Jumlah data testing adalah ", nrow(datatesting1)))
#Model
model1<- boosting(V7~., data = datatraining1, boos = T, mfinal = 100)
prediksi1<-predict(model1, datatesting1)
confusionMatrix(prediksi1$confusion)
prediksi1Semua<-predict(model1, data1)
confusionMatrix(prediksi1Semua$confusion)
data2 <- select(data, V1, V2, V3, V4, V5, V6, V8)
indexes2=createDataPartition(data2$V8, p=0.9, list = F)
datatraining2 <- data2[indexes2,]
datatesting2<-data2[-indexes2,]
print(paste("Jumlah data training  adalah ", nrow(datatraining2)))
print(paste("Jumlah data testing adalah ", nrow(datatesting2)))
#Model
model2<- boosting(V8~., data = datatraining2, boos = T, mfinal = 100)
confusionMatrix(prediksi2$confusion)
prediksi2Semua<-predict(model2, data2)
confusionMatrix(prediksi2Semua$confusion)
