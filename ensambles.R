p1 <- c(1,1,1,1,0)
p2 <- c(1,0,0,1,1)
p3 <- c(1,0,1,0,0)
pf <- c(1,0,0,1,0)

##ENSAMBLE POR VOTACION SIMPLE
predrf2 <- ifelse(predrf == 1,0,1)
prednb2 <- ifelse(prednb == 1,0,1)
predlog2 <- ifelse(predlog == 1,2,1)
predictframe <- data.frame(predrf,predlog2,predbag)
predictframe <- sapply(predictframe, function(x) as.numeric(x))


getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

votsimple <- apply(predictframe,1,getmode)
roc_obj <- roc(test$gender.3, votsimple)
auc(roc_obj) # Se obtiene un AUC de 0,784 con el dataset completo



## ENSAMBLE 2: PROMEDIO PONDERADO (ACA VAN COMO PROBA)
predictframe <- data.frame(p1=predlog,p2=prednb,p3=predrf,pf=test$gender.3)
predictframe$pf <- ifelse(predictframe$pf==2,1,0)
predictframe$pf <- as.numeric(predictframe$pf)
trindpre <- sample(nrow(predictframe),nrow(predictframe)*0.7)
names(predictframe) <- c("p1","p2","p3","pf")
trainpred <- predictframe[trindpre,]
testpred <-predictframe[-trindpre,]
predfin <- glm(pf~.,trainpred,family=binomial(link="logit"))
a <- predict(predfin,testpred[,1:3])
#table(a,testpred$pf)
a <- ifelse(a>=1,2,1)
b <- table(a,testpred$pf)
#(b[1,1]+b[2,2])/(b[1,1]+b[1,2] +b[2,1]+b[2,2])
roc_obj <- roc(testpred$pf, a)
auc(roc_obj)

a_uno_cero <- function(x,corte){
    return(ifelse(x > corte,1,0))
}

accuracy<-function(predic,tes){
    vec1 <- (sum(abs(predic-tes)))
    return((length(tes)-vec1)/length(tes))
}

accuracy(a_uno_cero(p2,0.5),pf)

vecacc <- apply(predictframe[,1:3],2,function(x) accuracy(a_uno_cero(x,0.5),pf))
vecacc <- sapply(vecacc, function(x) x/sum(vecacc))

prompon <- apply(predictframe[,1:3],1,function(x) sum(unlist(x * vecacc)))
#version promedio sin ponderar:
#prompon <- apply(predictframe[,1:3],1,function(x) mean(unlist(x)))
prompon <- a_uno_cero(prompon,0.5)
prompon
pf

##ENSAMBLE 3 (Probar como proba y como 1-0)


trindpre <- sample(nrow(predictframe),nrow(predictframe)*0.7)
trainpred <- predictframe[trindpre,]
testpred <-predictframe[-trindpre,]

predfin <- LogitBoost(factor(pf)~.,trainpred)
a <- predict(predfin,testpred[,1:3])
a <- ifelse(a >0.5,1,0)
table(a,testpred$pf)
