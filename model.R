library(C50)
library(caret)

data_ha <- read.csv("/data/heart.csv")

#cek struktur data
str(data_ha)

View(data_ha)

#mengecek data kosong
is.na(data_ha)
sum(is.na(data_ha))

#Rubah struktur data
data_ha$sex <- as.factor(data_ha$sex)
data_ha$cp <- as.factor(data_ha$cp)
data_ha$fbs <- as.factor(data_ha$fbs)
data_ha$restecg <- as.factor(data_ha$restecg)
data_ha$exng <- as.factor(data_ha$exng)
data_ha$output <- as.factor(data_ha$output)

#str(data_ha)

#Drop variabel
library(dplyr)

data_ha <- data_ha %>%
  select( -c(slp, caa, thall))


str(data_ha)

##Model
model_c50 <- C5.0(output ~ ., data_ha, trials=10)
summary(model_c50)
#Kehandalan model / Prediksi
nilai_predict <- predict(model_c50, data_ha[-11])
nilai_aktual <- data_ha$output
confusionMatrix(nilai_predict, nilai_aktual)

##menyimpan model
saveRDS(model_c50, "Deploy/data/model_c50.RDS")

model_c50 <- readRDS("data/model_c50.RDS")


data_baru <- data.frame(
  nama = "pasien B",
  age = 10,
  sex = factor(0),
  cp  = factor(0),
  trtbps =  167,
  chol = 203,
  fbs = factor(1),
  restecg = factor(0),
  thalachh = 189,
  exng = factor(1),
  oldpeak = 1.3
)

if(predict(model_c50, data_baru) == 0){
  print(paste(data_baru[1,1], "beresiko rendah menderita penyakit jantung"))
} else{
  print(paste(data_baru[1,1], "beresiko tinggi menderita penyakit jantung"))  
}






