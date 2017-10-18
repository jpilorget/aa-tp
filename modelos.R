#Elijo el directorio y cargo las librerías
#setwd("C:/Users/Usuario/Downloads")
setwd("~/TP")
library(e1071)
library(caret)
library(caretEnsemble)
library(pROC)
library(rpart)
library(randomForest)
library(adabag) #De acá saqué el boosting
library(ipred)
library(xgboost) #Esta es una alternativa para boosting, no la probé aún

#Cargo el dataset final
basefinal <- read.csv("basefinal.csv", stringsAsFactors = F)

#Convierto las columnas lógicas a numéricas
basefinal$igcol <- as.numeric(basefinal$igcol)
basefinal$istest <- as.numeric(basefinal$istest)
basefinal$urlexists <- as.numeric(basefinal$urlexists)

#Convierto la variable de extensión a numérica
basefinal$profext <- as.numeric(as.factor(basefinal$profext))

#Convierto la clase primero a número y luego a factor
basefinal$gender.3 <- as.factor(as.numeric(as.factor(basefinal$gender.3)))

#Recorto el dataset excluyendo variables ya representadas por otras
#Por ejemplo, cantidad de tuits o retuits.
basefinal <- basefinal[,c(1:44,49:75,78,80:86)]

#Creo el conjunto de test y de train
test <- basefinal[basefinal$istest==1,]
train <- basefinal[basefinal$istest == 0,]

#Filtro el conjunto de train para gender.confidence == 1 y saco la columnas istest y conf.
train <- train[train$conf == 1,]
train <- train[,-c(73,76)]
test <- test[,-c(73,76)]


#Construyo un modelo Naïve Bayes
modelonb <- naiveBayes(gender.3~.,train)
prednb <- predict(modelonb,newdata=test[,-77])

a<-table(prednb,test$gender.3)
(a[1,1]+a[2,2])/(a[1,1]+a[1,2] +a[2,1]+a[2,2])
roc_obj <- roc(test$gender.3, conv(prednb))
auc(roc_obj) # Se obtiene un AUC de 0,784 con el dataset completo

#Construyo un modelo Random Forest
modelorf <- randomForest(gender.3~.,train)
predrf <- predict(modelorf,newdata=test[,-77])

a<-table(predrf,test$gender.3)
(a[1,1]+a[2,2])/(a[1,1]+a[1,2] +a[2,1]+a[2,2])
roc_obj <- roc(test$gender.3, conv(predrf))
auc(roc_obj) # Se obtiene un AUC de 0,827 con el dataset completo

#Construyo un modelo Regresión Logística
fit <-glm(gender.3~.,train,family = binomial(link="logit"))
predlog <- predict(fit,test[,-77])
predlog <- ifelse(predlog > 0.75,1,0)
roc_obj <- roc(test$gender.3, predlog)
auc(roc_obj) # Se obtiene un AUC de 0,815 con el dataset completo


#Hago un boosting
fit <-boosting(gender.3~.,train)
predboost <- predict(fit,test[,-77])
predboost <- ifelse(predboost > 0.75,1,0)
roc_obj <- roc(conv(test$gender.3), conv(predboost$class))
auc(roc_obj) # Se obtiene un AUc de 0,81 con el dataset completo

#Hago un bagging

gbag <- bagging(gender.3 ~ ., train, coob=TRUE)
predbag <- predict(gbag,test[,-77])
roc_obj <- roc(conv(test$gender.3), conv(predbag))
auc(roc_obj) # Se obtiene un AUc de 0,816 con el dataset completo
