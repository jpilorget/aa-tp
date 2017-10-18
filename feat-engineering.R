#Cargo las librerías (deben estar instaladas)
#Debería modificar el código para hacer un require que instale cada paquete si no lo está.

library(xlsx)
library(plotrix)
library(stringr)
library(slam)
library(e1071)
library(colorspace)
library(caret)
library(caretEnsemble)
library(pROC)
library(stringr)
library(RWeka)
library(rpart)
library(randomForest)
library(dplyr)
library(adabag)
library(ipred)
library(twitteR)
library(tm)
library(SnowballC)
library(wordcloud)
library(slam)
library(tm)
library(stringr)
library(xgboost)
library(readr)
library(car)

#Creo la función "conv"
conv <- function(x){
    x <- as.character(x)
    x[x=="human"] <- "1"
    x[x=="brand"] <- "0"
    x <- as.numeric(x)
    return(x)
}

#Elijo el directorio y cargo los datasets
setwd("~/TP 2")
datos<- read.csv("gender-classifier-DFE-791531.csv", encoding = "utf-8", stringsAsFactors = F)
pictext <- read.csv("pictext.csv", stringsAsFactors = F)
datos <- data.frame(datos,pictext[,5])
colors <- read.csv("colors2.csv", stringsAsFactors = F)
#Filtro los casos con clase
datos <- datos[datos$gender != "unknown",]
datos <- datos[datos$gender != "",]
datos$gender<- ifelse(datos$gender == "male", "human", ifelse(datos$gender == "female", "human", "brand")) 
colors$colores <- ntile(colors$cant.col.img,4)
colors$colores[is.na(colors$colores)] <- 0
colors <- data.frame(colors,conf=datos$gender.confidence,gender=datos$gender)
#Renombro train como datos para conservar el resto del código
text <- datos[,c(11,15,20,6)]

#para under sampling
#datos2 <- datos
#datosh <- datos[datos$gender=="human",]
#datosb <- datos[datos$gender=="brand",]
#datos <- rbind(datosb,datosh[sample(nrow(datosh),nrow(datosh)*0.70),])

metadata <- data.frame(metadata,datos$gender.confidence)
metadataf <- data.frame(gender=metadata$gender,profext,favxant,conf = metadata$datos.gender.confidence)
#trind <- sample(nrow(metadataf),nrow(metadataf)*0.6)
#train <- metadataf
#test <- test

#Hago pruebas para ver cómo predicen los colores (perdón Mauro, te hice pija el código)
#Creo la variable test
sample(nrow(metadata)*0.1)
datos$istest <- ifelse(nrow(datos)== istest,1,0)
test <- colors[istest,]
train <- colors[-istest,]
train <- train[train$conf == 1,]

library(caTools)
istest <- sample.split(metadatos$gender,SplitRatio=0.2)
metadatos <- data.frame(metadata,textimg,colores,m2f,metadataf,istest,colors)
metadatos <- metadatos[,c(6,8,11,12,14:44,48,51,52,54,55,62,64)]
write.csv(metadatos,"metadatos.csv", row.names =  F)

modelo2 <- naiveBayes(gender~.,train)
pred <- predict(modelo2,test[-10])

table(pred,test$gender)
(a[1,1]+a[2,2])/(a[1,1]+a[1,2] +a[2,1]+a[2,2])
roc_obj <- roc(conv(test$gender), conv(pred))
auc(roc_obj) # 
plot(roc_obj)


#Continuamos con el código como estaba antes
metadata <- datos[,c(2,10,12,14,17,18,19,22,23,6)]
###metadata
names(metadata)
table(metadata$X_golden,metadata$gender)
table(metadata$gender)

metadata$fecha <- as.Date(metadata$created)
metadata$fecha <- substr(gsub(".*/","",metadata$created),1,3)
metadata$hora <- str_sub(gsub(".*/","",metadata$created),-5,-4)
names(metadata)

metadata$fav_number <- Discretize(factor(metadata$gender) ~ metadata$fav_number)[,1]

## COLORES
c1 <- metadata$link_color

##corrijo c1
c1cor <- vector()
for(i in c1){
    cil <- length(unlist(strsplit(i,"")))
    if (cil < 6){
        ceros <- paste(rep(0,6-cil),collapse="")
        c1cor <- c(c1cor,paste(c(ceros,i),collapse=""))
    }
    else{c1cor <- c(c1cor,i)}
}

c1cor <- sapply(c1cor,function(x) paste(c("#",x),collapse=""))
names(c1cor) <- NULL

errores <- unlist(lapply(metadata$link_color,str_count,"[:punct:]"))
#corrijo a color mayoritario
c1cor[errores>0] <- "#008080"	

c1corarr <- unlist(sapply(c1cor,color.id))

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

color_cercano <- function(colorin){
    colores <- c("red","black","gray50","gray75","white","darkred","yellow4","yellow",
                 "green4","green1","turquoise4","cyan","navy","blue","magenta4","magenta")
    
    mindistcol <- ""
    mindist <- Inf
    for (i in colores){
        distancia <- euc.dist(as.vector(col2rgb(colorin)),as.vector(col2rgb(i)))
        if (distancia < mindist){
            mindist <- distancia
            mindistcol <- i
        }
    }
    return(mindistcol)
}

# ajusto al color mas cercano
c1cor2 <- unlist(sapply(c1cor,color_cercano))
names(c1cor2) <- NULL
table(c1cor2,metadata$gender)


## ahora el otro color
c2 <- metadata$sidebar_color
c2cor <- vector()
for(i in c2){
    cil <- length(unlist(strsplit(i,"")))
    if (cil < 6){
        ceros <- paste(rep(0,6-cil),collapse="")
        c2cor <- c(c2cor,paste(c(ceros,i),collapse=""))
    }
    else{c2cor <- c(c2cor,i)}
}

c2cor <- sapply(c2cor,function(x) paste(c("#",x),collapse=""))
names(c2cor) <- NULL

errores <- unlist(lapply(metadata$sidebar_color,str_count,"[:punct:]"))
#corrijo a color mayoritario
which(errores>0)
c2cor[errores>0] <- "#008080"	

# ajusto al color mas cercano
c2cor2 <- unlist(sapply(c2cor,color_cercano))
names(c2cor2) <- NULL
table(c2cor2,metadata$gender)

##IGUALDAD DE COLORES

igcol <- (c1cor2 ==c2cor2)
table(igcol,metadata$gender)

colores <- data.frame(link=c1cor2,sidebar=c2cor2,igcol)

## LISTO COLORES

## FOTOS
hfo <- unlist(sapply(metadata$profileimage,function(x) length(unlist(strsplit(x,"")))))
names(hfo) <- NULL
hfo <- data.frame(hfo,gender=metadata$gender)
hfod <- Discretize(factor(hfo$gender)~hfo$hfo)
table(hfod$`hfo$hfo`,hfod$`factor(hfo$gender)`)

metadata[1511,]
prof <- metadata$profileimage
profext <- vector()
for (i in prof){
    a <- tail(unlist(unlist(strsplit(i,"\\."))),1)    
    profext <- c(profext,a)
}

profext[profext %in% unique(profext)[c(6:8,11:14)]] <- "sin_formato"
table(profext,metadata$gender)
profext <- tolower(profext)
head(data.frame(prof,profext))

## tweet count


##FAVS POR ANTIGUEDAD
favxant <- round(as.numeric(metadata$fecha)/datos$fav_number)
favxant[favxant == Inf] <- 16
favxant[favxant >0] <- 1

names(metadata)
table(colores$link,metadata$gender)


## BAYES
metadata
metadata <- data.frame(metadata,datos$gender.confidence)
metadataf <- data.frame(gender=metadata$gender,profext,favxant,conf = metadata$datos.gender.confidence)
#trind <- sample(nrow(metadataf),nrow(metadataf)*0.6)
#train <- metadataf
#test <- test

#Creo la variable test
istest <- sample(nrow(metadata)*0.1)
test <- metadataf[istest,]
train <- metadataf[-istest,]
train <- train[train$conf == 1,]


modelo2 <- naiveBayes(gender~.,train)
pred <- predict(modelo2,test[,-which(names(test)=="gender")])

a<- table(pred,test$gender)
(a[1,1]+a[2,2])/(a[1,1]+a[1,2] +a[2,1]+a[2,2])
roc_obj <- roc(conv(test$gender), conv(pred))
auc(roc_obj) # 
plot(roc_obj)

#####################

m2 <- metadata[,c(3,6,8,12)]
m2 <- data.frame(m2,conf=datos$gender.confidence,gender=datos$gender)
m2$hora <- as.numeric(m2$hora)
m2[,1:4] <- scale(m2[,1:4])


m2$gender<- conv(m2$gender)
m2f <- data.frame(m2)
m2f <- within(m2f, fav_number <- factor(fav_number, labels = c(seq(1,6,1))))
testsv <- m2f[istest,]
trainsv <- m2f[-istest,]
trainsv <- trainsv[trainsv$conf == 1,]
testsv <- testsv[,c(1:4,6)]
trainsv <-trainsv[,c(1:4,6)]

modelosv <- svm(gender~.,trainsv)
predsv <- predict(modelosv,testsv[,-5])
pred3 <- predsv
pred3 <- ifelse(pred3>0.9,1,0)

table(pred3,testsv$gender)

roc_objrf <- roc(conv(testsv$gender), pred3)
auc(roc_objrf) # 
plot(roc_objrf)


########
#Palabras
#Primero analizo el texto de la imagen luego de pasar por Clarifai API
pictext <- Corpus(VectorSource(datos$pictext...5.))
pictext <- limpiarCorpus2(pictext)
pictext2 <- data.frame(img = sapply(pictext, as.character), stringsAsFactors = FALSE)
datos$texto_imagen <- pictext2$img

#Miro la TermDocumentMatrix
tdm <- TermDocumentMatrix(pictext,control = list(wordLengths = c(1, 15)))
tdm_nueva <- removeSparseTerms(tdm,sparse = 0.996)
dim(tdm_nueva)
term.freq <- as.data.frame(sort((rowSums(as.matrix(tdm_nueva))),decreasing = T))
term.freq2 <- as.data.frame(sort((rowSums(as.matrix(tdm_nueva))),decreasing = T))
term.freq$palabra <- rownames(term.freq)
term.freq2$palabra <- rownames(term.freq2)
frecuencia <- merge(term.freq, term.freq2, by = "palabra", all.x = T, all.y = T)
write.csv(frecuencia,"frecuencia.csv", row.names = F)
#Ahora saco las principales palabras (sacadas del script palabras)
#Pruebo una versión reducida
wnw <- c("design","symbol","illustration","desktop","business","graphic","patriotism","person",
         "sign","text","vector","image","disjunct","shining","isolated","internet","alphabet",
         "people","portrait", "adult","woman","one","man","fashion","girl","facial","expression")
#Creo las variables

for (i in wnw){
    assign(i,unlist(lapply(datos$texto_imagen,str_count,i)))    
}

## ACA COMIENZA EL ARMADO DEL DATASET 

tuits <- data.frame(id=datos$X_unit_id)
vars <- c("wnw")
for (i in vars){
    for(j in 1:length(get(i))){
        tuits <- cbind(tuits,get(get(i)[j]))
    }
}

textimg <- tuits
textimg[,2:28][tuits[,2:28] > 0] <- 1

nambinari <- "id"
for (i in vars){
    for (j in 1:length(get(i))){
        varbina <- paste(get(i)[j],"bin",collapse=" ")
        nambinari <- c(nambinari,varbina)
    }
}

nambinari <- c(nambinari)

names(textimg) <- nambinari

#namest2 <- names(textimg)
#namest2 <- gsub(" ","",namest2)
#names(textimg) <- namest2
textoimagenes <- textimg[,2:28]
textimg2 <- data.frame(textoimagenes,colors= colors$colores, conf=datos$gender.confidence)
textimg2$gender <- as.factor(datos$gender)


testimg <- textimg2[istest,]
trainimg <- textimg2[-istest,]
trainimg <- trainimg[trainimg$conf >0.999,]


modelo2 <- svm(gender~.,trainimg)
pred3 <- predict(modelo2,testimg[,-30])

table(pred3,testimg$gender)
(a[1,1]+a[2,2])/(a[1,1]+a[1,2] +a[2,1]+a[2,2])
roc_obj <- roc(conv(testimg$gender), conv(pred3))
auc(roc_obj) # 
plot(roc_obj)

#Acá termino con el texto de la imagen y arranco con el de los tuits y las descripciones

toSpace <- content_transformer(function(x,pattern) gsub(pattern," ",x)) 
toLink <- content_transformer(function(x,pattern) gsub(pattern,"link_http ",x)) 
toLink2 <- content_transformer(function(x,pattern) gsub(pattern,"link_http ",x)) 


#Pruebo esta alternativa a "limpiarCorpus" (la función de Mauro), con otros parámetros
removeNumPunct <- function(x) gsub("[^[:alnum:][:space:]]*", "", x)
septag <- function(x) gsub("<", " EMO", x)
septag2 <- function(x) gsub(">", "EMO ", x)

text$text <- data.frame(text = iconv(datos$text, "utf-8", "ascii", "byte"),stringsAsFactors = FALSE)[,1]


limpiarCorpus2 <- function(corpus) {
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, toLink, "https://t.co")
    corpus <- tm_map(corpus, toLink2, "http://t.co")
    corpus <- tm_map(corpus, septag)
    corpus <- tm_map(corpus, septag2)
    corpus <- tm_map(corpus, toSpace, "_")
    corpus <- tm_map(corpus, toSpace, "@")
    myStopwords <- stopwords('english')
    corpus <- tm_map(corpus, removeWords, myStopwords)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, stemDocument)
    corpus <- tm_map(corpus, content_transformer(removeNumPunct))
    return(corpus)
}


tuith <- text[text$gender=="human",]$text
tuitb <- text[text$gender=="brand",]$text
tuith <- Corpus(VectorSource(tuith))
tuith <- limpiarCorpus2(tuith)

tdmh <- TermDocumentMatrix(tuith,control = list(wordLengths = c(1, 10)))
(freq.terms <- findFreqTerms(tdmh, lowfreq = 10))
term.freq <- rollup(tdmh, 2, na.rm=TRUE, FUN = sum)

humanwords <- as.data.frame(as.matrix(term.freq))
humanwords <- cbind(unlist(rownames(humanwords)),humanwords)
rownames(humanwords) <-NULL
names(humanwords) <- c("word","count")
humanwords <- humanwords[order(humanwords$count,decreasing = T),]
head(humanwords)

tuitb <- text[text$gender=="brand",]$text
tuitb <- Corpus(VectorSource(tuitb))
tuitb <- limpiarCorpus2(tuitb)
tdmb <- TermDocumentMatrix(tuitb,control = list(wordLengths = c(1, 10)))
(freq.terms <- findFreqTerms(tdmb, lowfreq = 10))
term.freq <- rollup(tdmb, 2, na.rm=TRUE, FUN = sum)

brandwords <- as.data.frame(as.matrix(term.freq))
brandwords <- cbind(unlist(rownames(brandwords)),brandwords)
rownames(brandwords) <-NULL
names(brandwords) <- c("word","count")
brandwords <- brandwords[order(brandwords$count,decreasing = T),]
head(brandwords)

head(humanwords)


hnob <- c("bitch","gotta","nigga","cry","annoying","deserve","respect",
          "honest","ugly","bitch")

bwords <- data.frame(intersect(brandwords$word,humanwords$word))
names(bwords) <- "word"
hw2 <- humanwords[humanwords$word %in% bwords$word,]
bw2 <- brandwords[brandwords$word %in% bwords$word,]

wic <- merge(hw2,bw2,by="word")
wic$count.x <- as.numeric(as.character(wic$count.x))
wic$count.y <- as.numeric(as.character(wic$count.y))

head(wic)
names(wic) <- c("word","human","brand")
wic <- wic[order(wic$human),]
#write.xlsx(wic,"wic.xlsx")


### REPITO CON DESCRIPCIONES
text$description <- data.frame(text = iconv(text$description, "utf-8", "ascii", "byte"),stringsAsFactors = FALSE)[,1]

desch <- text[text$gender=="human",]$description
descb <- text[text$gender=="brand",]$description
desch <- Corpus(VectorSource(desch))
desch <- limpiarCorpus2(desch)

tdmdh <- TermDocumentMatrix(desch,control = list(wordLengths = c(1, 10)))
(freq.terms <- findFreqTerms(tdmdh, lowfreq = 10))
term.freq <- rollup(tdmdh, 2, na.rm=TRUE, FUN = sum)

humandescr <- as.data.frame(as.matrix(term.freq))
humandescr <- cbind(unlist(rownames(humandescr)),humandescr)
rownames(humandescr) <-NULL
names(humandescr) <- c("word","count")
humandescr <- humandescr[order(humandescr$count,decreasing = T),]
head(humandescr)

descb <- text[text$gender=="brand",]$description
descb <- Corpus(VectorSource(descb))
descb <- limpiarCorpus2(descb)
tdmdb <- TermDocumentMatrix(descb,control = list(wordLengths = c(1, 10)))
(freq.terms <- findFreqTerms(tdmdb, lowfreq = 10))
term.freq <- rollup(tdmdb, 2, na.rm=TRUE, FUN = sum)

branddescr <- as.data.frame(as.matrix(term.freq))
branddescr <- cbind(unlist(rownames(branddescr)),branddescr)
rownames(branddescr) <-NULL
names(branddescr) <- c("word","count")
branddescr <- branddescr[order(branddescr$count,decreasing = T),]
head(branddescr)

head(humandescr)


bdescr <- data.frame(intersect(branddescr$word,humandescr$word))
names(bdescr) <- "word"
hd2 <- humandescr[humandescr$word %in% bdescr$word,]
bd2 <- branddescr[branddescr$word %in% bdescr$word,]

wicd <- merge(hd2,bd2,by="word")
wicd$count.x <- as.numeric(as.character(wicd$count.x))
wicd$count.y <- as.numeric(as.character(wicd$count.y))

head(wicd)
names(wicd) <- c("word","human","brand")
wicd <- wicd[order(-wicd$human),]
#write.xlsx(wicd,"descr.xlsx")



## Predictores de human
humantuit <- humanwords
humandesc <- humandescr
ht <- as.character(humantuit[,1])
hd <- as.character(humandesc[,1])

htvars <- ""
hdvars <- ""

for (i in ht){
    nomi <- str_extract(i, "[:alnum:]+" )
    nombre <- paste(c("texth",nomi),collapse="",sep="")
    htvars <- c(htvars,nombre)
    assign(nombre,unlist(lapply(text$text,str_count,i)))    
}

for (i in hd){
    nomi <- str_extract(i, "[:alnum:]+" )
    nombre <- paste(c("descrh",nomi),collapse="",sep="")
    hdvars <- c(hdvars,nombre)
    assign(nombre,unlist(lapply(text$description,str_count,i)))    
}

htvars <- htvars[2:length(htvars)]
hdvars <- hdvars[2:length(hdvars)]

hpredictors <- data.frame(get(htvars[1]))
for (i in htvars){
    hpredictors <- cbind(hpredictors,get(i))
}
hpredictors[,1] <- NULL
names(hpredictors) <- htvars
head(hpredictors)

hpredictorsd <- data.frame(get(hdvars[1]))
for (i in hdvars){
    hpredictorsd <- cbind(hpredictorsd,get(i))
}
hpredictorsd[,1] <- NULL
names(hpredictorsd) <- hdvars

head(hpredictors)
head(hpredictorsd)

hpredictorsfinal <- cbind(hpredictors,hpredictorsd)

#hpredictors$gender <- conv(datos$gender)
#hpredictorsd$gender <- conv(datos$gender)
hpredictorsfinal$gender <- conv(datos$gender)

hpredictorsfinal[hpredictorsfinal>0] <- 1
#hpredictorsfinal <- data.frame(sapply(hpredictorsfinal,as.factor))

trindh <- sample(nrow(hpredictorsfinal),nrow(hpredictorsfinal)*0.6)
trainhp <- hpredictorsfinal[trindh,]
testhp <- hpredictorsfinal[trindh,]

#trainhp$gender <- as.factor(trainhp$gender)
#testhp$gender <- as.factor(testhp$gender)

#arbolito <- boosting(gender~.,trainhp,boos = TRUE, 
#                     mfinal = 3,  control = rpart.control(minsplit = 0))
arbolito <- rpart(gender~.,trainhp,control=rpart.control(cp = 0.001))
predhum <- predict(arbolito,testhp[,-33])
predhum <- ifelse(predhum>0.6,1,0)
table(predhum,testhp$gender)


roc_obj <- roc(testhp$gender, predhum)
auc(roc_obj)  
plot(roc_obj)

## Predictores de brand
brandtuit <- brandwords
branddesc <- branddescr
bt <- as.character(brandtuit[,1])
bd <- as.character(branddesc[,1])

btvars <- ""
bdvars <- ""

for (i in bt){
    nomi <- str_extract(i, "[:alnum:]+" )
    nombre <- paste(c("textb",nomi),collapse="",sep="")
    btvars <- c(btvars,nombre)
    assign(nombre,unlist(lapply(text$text,str_count,i)))    
}

for (i in bd){
    nomi <- str_extract(i, "[:alnum:]+" )
    nombre <- paste(c("descb",nomi),collapse="",sep="")
    bdvars <- c(bdvars,nombre)
    assign(nombre,unlist(lapply(text$description,str_count,i)))    
}

btvars <- btvars[2:length(btvars)]
bdvars <- bdvars[2:length(bdvars)]


bpredictors <- data.frame(get(btvars[1]))
for (i in btvars){
    bpredictors <- cbind(bpredictors,get(i))
}
bpredictors[,1] <- NULL
names(bpredictors) <- btvars
head(bpredictors)

bpredictorsd <- data.frame(get(bdvars[1]))
for (i in bdvars){
    bpredictorsd <- cbind(bpredictorsd,get(i))
}
bpredictorsd[,1] <- NULL
names(bpredictorsd) <- bdvars
head(bpredictors)

bpredictorsfinal <- cbind(bpredictors,bpredictorsd)

bpredictors$gender <- conv(datos$gender)
bpredictorsd$gender <- conv(datos$gender)
bpredictorsfinal$gender <- conv(datos$gender)

## clasificador final palabras
names(bpredictorsfinal)

palabrasfinal <- cbind(hpredictorsfinal,bpredictorsfinal)
names(palabrasfinal)
palabrasfinal <- palabrasfinal[,-33]
palabrasfinal[palabrasfinal >1] <- 1
palabrasfinal$gender <- as.factor(palabrasfinal$gender)

trindpf <- sample(nrow(palabrasfinal),nrow(palabrasfinal)*0.7)
trainpf <- palabrasfinal[trindpf,]
testpf <- palabrasfinal[trindpf,]

#,control=rpart.control(cp = 0.001)

arbolito <- randomForest(gender~.,trainpf)
predpf <- predict(arbolito,testpf[,-45])
predpf <- ifelse(predpf>0.6,1,0)
table(predpf,testpf$gender)
    
roc_obj <- roc(testpf$gender, predpf)
print(auc(roc_obj))
plot(roc_obj)

control <- rfeControl(functions = rfFuncs,method="cv", number=10)
results <- rfe(trainpf[,1:44],trainpf[,45],sizes=c(1:44),rfeControl=control)



## LAS VARIABLES SERÍAN:
#METADATA (Y TODAS LAS VARIABLES QUE TIENE ADENTRO)
#COLORES (QUE CREO QUE NO ESTÁ UNIDA A METADATA)
#profext,favxant
#palabrasfinal
