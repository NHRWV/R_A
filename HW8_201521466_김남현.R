library(tidyverse)
library(e1071)
library(DataExplorer)
library(caret)

df <- read.csv("통신회사고객이탈예측.csv", header = T, stringsAsFactors = T)
df$customerID <- as.character(df$customerID)

plot_intro(df)


df_s <- df[, c(7:15, 21)]
df_a <- df[, c(6, 16:21)]
df_p <- df[, c(1:5, 21)]
df <- df[,-1]

set.seed(25) 
intrain_s <- createDataPartition(y = df_s$Churn, p = 0.7, list = FALSE) 
train_s <- df_s[intrain_s, ]
test_s <- df_s[-intrain_s, ]

set.seed(25)
intrain_a <- createDataPartition(y = df_a$Churn, p = 0.7, list = FALSE) 
train_a <- df_a[intrain_a, ]
test_a <- df_a[-intrain_a, ]

set.seed(25)
intrain_p <- createDataPartition(y = df_p$Churn, p = 0.7, list = FALSE) 
train_p <- df_p[intrain_p, ]
test_p <- df_p[-intrain_p, ]

mdl_s <- naiveBayes(Churn ~ ., data = train_s)
mdl_a <- naiveBayes(Churn ~ ., data = train_a)
mdl_p <- naiveBayes(Churn ~ ., data = train_p)

mdl_s




pred_s <- predict(mdl_s, test, type='class')
confusionMatrix(nbpred, test$AHD)
methods("predict")
