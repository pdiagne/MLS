# Clear environment and console
remove(list = ls())
shell("cls")

#Load Libraries
library(openxlsx)
library(readxl)
library(rapportools)
library(data.table)
library(gtools)
library(stringr)
library(tidyr)
library(reshape)
library(dplyr)
library(purrr)
library(tidyverse)
library(plotly)
library(webshot)
library(svDialogs)
library(ggvis)
library(factoextra)
library(pracma)
library(xgboost)
library(caret)

start <- Sys.time()

dir <- ''
setwd(dir)

# Kaggle Football Data Challenge
  # https://www.kaggle.com/c/football-data-challenge/

c <- 644 # length(unique(train$ID)) = 644
models <- vector(mode = "list", length = c)
nrounds = 100
max.depth = 2

# Load Training Data
d <- read.csv("train.csv")

# XGBoost Classifier Training - In Sample Forecast
for (j in 1:c) { 
 
  # Filter Training Data
  train <- d
  train$ID <- paste(train$HomeTeam,train$AwayTeam, sep = '-')
  id <- unique(train$ID)
  ind <- train$ID == id[j]
  train <- train[ind,-c(1:4)]
  
  # Replace missing data with column median
  for(i in 1:ncol(train)){
    if (length(train[is.na(train[,i]), i]) > 0) {
      train[is.na(train[,i]), i] <- median(train[,i], na.rm = TRUE)
    } 
    if (length(train[is.na(train[,i]), i]) > 0 && nrow(train) == 1) {
      train[is.na(train[,i]), i] <- 1
    }
  }
  
  # Feature Engineering Training Data
  
  # Log Transform
  train$B365H_log <- log(train$B365H)
  train$B365D_log <- log(train$B365D)
  train$B365A_log <- log(train$B365A)
  train$BWH_log <- log(train$BWH)
  train$BWD_log <- log(train$BWD)
  train$BWA_log <- log(train$BWA)
  train$IWH_log <- log(train$IWH)
  train$IWD_log <- log(train$IWD)
  train$IWA_log <- log(train$IWA)
  train$LBH_log <- log(train$LBH)
  train$LBD_log <- log(train$LBD)
  train$LBA_log <- log(train$LBA)
  train$WHH_log <- log(train$WHH)
  train$WHD_log <- log(train$WHD)
  train$WHA_log <- log(train$WHA)
  train$VCH_log <- log(train$VCH)
  train$VCD_log <- log(train$VCD)
  train$VCA_log <- log(train$VCA)
  
  # Box-Cox Transform
  lambda = 2
  train$B365H_bc <- (train$B365H^lambda - 1)/lambda
  train$B365D_bc <- (train$B365D^lambda - 1)/lambda
  train$B365A_bc <- (train$B365A^lambda - 1)/lambda
  train$BWH_bc <- (train$BWH^lambda - 1)/lambda
  train$BWD_bc <- (train$BWD^lambda - 1)/lambda
  train$BWA_bc <- (train$BWA^lambda - 1)/lambda
  train$IWH_bc <- (train$IWH^lambda - 1)/lambda
  train$IWD_bc <- (train$IWD^lambda - 1)/lambda
  train$IWA_bc <- (train$IWA^lambda - 1)/lambda
  train$LBH_bc <- (train$LBH^lambda - 1)/lambda
  train$LBD_bc <- (train$LBD^lambda - 1)/lambda
  train$LBA_bc <- (train$LBA^lambda - 1)/lambda
  train$WHH_bc <- (train$WHH^lambda - 1)/lambda
  train$WHD_bc <- (train$WHD^lambda - 1)/lambda
  train$WHA_bc <- (train$WHA^lambda - 1)/lambda
  train$VCH_bc <- (train$VCH^lambda - 1)/lambda
  train$VCD_bc <- (train$VCD^lambda - 1)/lambda
  train$VCA_bc <- (train$VCA^lambda - 1)/lambda
  
  # Reciprocal Transform
  train$B365H_rec <- 1/train$B365H
  train$B365D_rec <- 1/train$B365D
  train$B365A_rec <- 1/train$B365A
  train$BWH_rec <- 1/train$BWH
  train$BWD_rec <- 1/train$BWD
  train$BWA_rec <- 1/train$BWA
  train$IWH_rec <- 1/train$IWH
  train$IWD_rec <- 1/train$IWD
  train$IWA_rec <- 1/train$IWA
  train$LBH_rec <- 1/train$LBH
  train$LBD_rec <- 1/train$LBD
  train$LBA_rec <- 1/train$LBA
  train$WHH_rec <- 1/train$WHH
  train$WHD_rec <- 1/train$WHD
  train$WHA_rec <- 1/train$WHA
  train$VCH_rec <- 1/train$VCH
  train$VCD_rec <- 1/train$VCD
  train$VCA_rec <- 1/train$VCA
  
  train_x = data.matrix(train[,-1])
  train_y = train[,1]
  
  # PCA Dimension Reduction
    # https://www.datacamp.com/community/tutorials/pca-analysis-r
  if (nrow(train_x) > 1) {
    train_x.pca <- prcomp(train_x, center = FALSE, scale = FALSE) # summary(train_x.pca)
    pc.use <- 1 #length(which(train_x.pca$sdev > 1)) 
    trunc <- train_x.pca$x[,1:pc.use] %*% t(train_x.pca$rotation[,1:pc.use])
    
    # Add the center (and re-scale) back to data
    if(train_x.pca$scale != FALSE){
      trunc <- scale(trunc, center = FALSE , scale=1/train_x.pca$scale)
    }
    if(train_x.pca$center != FALSE){
      trunc <- scale(trunc, center = -1 * train_x.pca$center, scale=FALSE)
    }
    train_x <- trunc
  }

  # https://www.datatechnotes.com/2018/03/classification-with-xgboost-model-in-r.html
  xgb_train = xgb.DMatrix(data=train_x, label=train_y)
  xgbc = xgboost(data=xgb_train, max.depth=max.depth, nrounds=nrounds)
  models[[j]] <- xgbc
}

#Feature Engineering Prediction Data
p <- read.csv("test.csv")
p_id <- paste(p$HomeTeam,p$AwayTeam, sep = '-')
pred <- p[,-c(1:4)]
p$ID_m <- paste(p$HomeTeam,p$AwayTeam, sep = '-')

# Replace missing data with column median
for(i in 1:ncol(pred)){
  if (length(pred[is.na(pred[,i]), i]) > 0) {
    pred[is.na(pred[,i]), i] <- median(pred[,i], na.rm = TRUE)
  } 
  if (length(pred[is.na(pred[,i]), i]) > 0 && nrow(pred) == 1) {
    pred[is.na(pred[,i]), i] <- 1
  }
}

# Log Transform
pred$B365H_log <- log(pred$B365H)
pred$B365D_log <- log(pred$B365D)
pred$B365A_log <- log(pred$B365A)
pred$BWH_log <- log(pred$BWH)
pred$BWD_log <- log(pred$BWD)
pred$BWA_log <- log(pred$BWA)
pred$IWH_log <- log(pred$IWH)
pred$IWD_log <- log(pred$IWD)
pred$IWA_log <- log(pred$IWA)
pred$LBH_log <- log(pred$LBH)
pred$LBD_log <- log(pred$LBD)
pred$LBA_log <- log(pred$LBA)
pred$WHH_log <- log(pred$WHH)
pred$WHD_log <- log(pred$WHD)
pred$WHA_log <- log(pred$WHA)
pred$VCH_log <- log(pred$VCH)
pred$VCD_log <- log(pred$VCD)
pred$VCA_log <- log(pred$VCA)

# Box-Cox Transform
lambda = 2
pred$B365H_bc <- (pred$B365H^lambda - 1)/lambda
pred$B365D_bc <- (pred$B365D^lambda - 1)/lambda
pred$B365A_bc <- (pred$B365A^lambda - 1)/lambda
pred$BWH_bc <- (pred$BWH^lambda - 1)/lambda
pred$BWD_bc <- (pred$BWD^lambda - 1)/lambda
pred$BWA_bc <- (pred$BWA^lambda - 1)/lambda
pred$IWH_bc <- (pred$IWH^lambda - 1)/lambda
pred$IWD_bc <- (pred$IWD^lambda - 1)/lambda
pred$IWA_bc <- (pred$IWA^lambda - 1)/lambda
pred$LBH_bc <- (pred$LBH^lambda - 1)/lambda
pred$LBD_bc <- (pred$LBD^lambda - 1)/lambda
pred$LBA_bc <- (pred$LBA^lambda - 1)/lambda
pred$WHH_bc <- (pred$WHH^lambda - 1)/lambda
pred$WHD_bc <- (pred$WHD^lambda - 1)/lambda
pred$WHA_bc <- (pred$WHA^lambda - 1)/lambda
pred$VCH_bc <- (pred$VCH^lambda - 1)/lambda
pred$VCD_bc <- (pred$VCD^lambda - 1)/lambda
pred$VCA_bc <- (pred$VCA^lambda - 1)/lambda

# Reciprocal Transform
pred$B365H_rec <- 1/pred$B365H
pred$B365D_rec <- 1/pred$B365D
pred$B365A_rec <- 1/pred$B365A
pred$BWH_rec <- 1/pred$BWH
pred$BWD_rec <- 1/pred$BWD
pred$BWA_rec <- 1/pred$BWA
pred$IWH_rec <- 1/pred$IWH
pred$IWD_rec <- 1/pred$IWD
pred$IWA_rec <- 1/pred$IWA
pred$LBH_rec <- 1/pred$LBH
pred$LBD_rec <- 1/pred$LBD
pred$LBA_rec <- 1/pred$LBA
pred$WHH_rec <- 1/pred$WHH
pred$WHD_rec <- 1/pred$WHD
pred$WHA_rec <- 1/pred$WHA
pred$VCH_rec <- 1/pred$VCH
pred$VCD_rec <- 1/pred$VCD
pred$VCA_rec <- 1/pred$VCA

# XGBoost Classifier Prediction - In Sample 
p$FTR_pred <- ''
for (i in 1:nrow(pred)) {
  ind <- which(p$ID_m[i] == id)
  if (length(ind) > 0) { # Some Home/Away Team matches in the test set are not in the training set 
    pred_out = predict(models[[ind]], data.matrix(pred[i,]))
    p$FTR_pred[i] = as.character(as.factor((levels(train_y))[round(pred_out)]))
  }
}


# XGBoost Classifier Training - Out of Sample Forecast

# Filter Training Data
train <- d
train$ID <- paste(train$HomeTeam,train$AwayTeam, sep = '-')
id <- unique(train$ID)
train <- train[,-c(1:4)]

# Replace missing data with column median
for(i in 1:ncol(train)){
  if (length(train[is.na(train[,i]), i]) > 0) {
    train[is.na(train[,i]), i] <- median(train[,i], na.rm = TRUE)
  } 
  if (length(train[is.na(train[,i]), i]) > 0 && nrow(train) == 1) {
    train[is.na(train[,i]), i] <- 1
  }
}

# Feature Engineering Training Data

# Log Transform
train$B365H_log <- log(train$B365H)
train$B365D_log <- log(train$B365D)
train$B365A_log <- log(train$B365A)
train$BWH_log <- log(train$BWH)
train$BWD_log <- log(train$BWD)
train$BWA_log <- log(train$BWA)
train$IWH_log <- log(train$IWH)
train$IWD_log <- log(train$IWD)
train$IWA_log <- log(train$IWA)
train$LBH_log <- log(train$LBH)
train$LBD_log <- log(train$LBD)
train$LBA_log <- log(train$LBA)
train$WHH_log <- log(train$WHH)
train$WHD_log <- log(train$WHD)
train$WHA_log <- log(train$WHA)
train$VCH_log <- log(train$VCH)
train$VCD_log <- log(train$VCD)
train$VCA_log <- log(train$VCA)

# Box-Cox Transform
lambda = 2
train$B365H_bc <- (train$B365H^lambda - 1)/lambda
train$B365D_bc <- (train$B365D^lambda - 1)/lambda
train$B365A_bc <- (train$B365A^lambda - 1)/lambda
train$BWH_bc <- (train$BWH^lambda - 1)/lambda
train$BWD_bc <- (train$BWD^lambda - 1)/lambda
train$BWA_bc <- (train$BWA^lambda - 1)/lambda
train$IWH_bc <- (train$IWH^lambda - 1)/lambda
train$IWD_bc <- (train$IWD^lambda - 1)/lambda
train$IWA_bc <- (train$IWA^lambda - 1)/lambda
train$LBH_bc <- (train$LBH^lambda - 1)/lambda
train$LBD_bc <- (train$LBD^lambda - 1)/lambda
train$LBA_bc <- (train$LBA^lambda - 1)/lambda
train$WHH_bc <- (train$WHH^lambda - 1)/lambda
train$WHD_bc <- (train$WHD^lambda - 1)/lambda
train$WHA_bc <- (train$WHA^lambda - 1)/lambda
train$VCH_bc <- (train$VCH^lambda - 1)/lambda
train$VCD_bc <- (train$VCD^lambda - 1)/lambda
train$VCA_bc <- (train$VCA^lambda - 1)/lambda

# Reciprocal Transform
train$B365H_rec <- 1/train$B365H
train$B365D_rec <- 1/train$B365D
train$B365A_rec <- 1/train$B365A
train$BWH_rec <- 1/train$BWH
train$BWD_rec <- 1/train$BWD
train$BWA_rec <- 1/train$BWA
train$IWH_rec <- 1/train$IWH
train$IWD_rec <- 1/train$IWD
train$IWA_rec <- 1/train$IWA
train$LBH_rec <- 1/train$LBH
train$LBD_rec <- 1/train$LBD
train$LBA_rec <- 1/train$LBA
train$WHH_rec <- 1/train$WHH
train$WHD_rec <- 1/train$WHD
train$WHA_rec <- 1/train$WHA
train$VCH_rec <- 1/train$VCH
train$VCD_rec <- 1/train$VCD
train$VCA_rec <- 1/train$VCA

train_x = data.matrix(train[,-1])
train_y = train[,1]

# PCA Dimension Reduction
train_x.pca <- prcomp(train_x, center = FALSE, scale = FALSE) # summary(train_x.pca)
pc.use <- 1 # length(which(train_x.pca$sdev > 1))  
trunc <- train_x.pca$x[,1:pc.use] %*% t(train_x.pca$rotation[,1:pc.use])

  # Add the center (and re-scale) back to data
if(train_x.pca$scale != FALSE){
  trunc <- scale(trunc, center = FALSE , scale=1/train_x.pca$scale)
}
if(train_x.pca$center != FALSE){
  trunc <- scale(trunc, center = -1 * train_x.pca$center, scale=FALSE)
}

# XGBoost Classifier Prediction - Out of Sample 
xgb_train <- xgb.DMatrix(data=trunc, label=train_y)
xgbc <- xgboost(data=xgb_train, max.depth=max.depth, nrounds=nrounds)
ind <- which(p$FTR_pred == '')
pred_out <- predict(xgbc, data.matrix(pred[ind,]))
p$FTR_pred[ind] <- as.character(as.factor((levels(train_y))[round(pred_out)]))

# Export csv
p <- dplyr::select(p, c(1,ncol(p)))
write.csv(p,"Match_Prediction.csv", row.names = FALSE)

stop <- Sys.time()
runtime <- stop - start
msg_box(c('Done: ',runtime,' Minutes'))