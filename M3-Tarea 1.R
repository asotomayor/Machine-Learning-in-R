## M3-Tarea1##
##INTRODUCCIÓN Y OBJETIVO##
#Clasificar a los alumnos de matemáticas como aprobados o suspendidos en  
#función de que la nota sea mayor o menor que  10
## 1. Carga de datos y Análisis Descriptivo##
# Creamos nuevo diretorio de trabajo
if(!file.exists("C:/Users/Antonio/Documents/M3-Machine-Learning/M3-scripts//"))
{dir.create("C:/Users/Antonio/Documents/M3-Machine-Learning/M3-scripts")}
# Establecemos nuevo directorio de trabajo
setwd("C:/Users/Antonio/Documents/M3-Machine-Learning/M3-scripts")

# Limpieza directorio de trabajo

# Carga de librerias
load.libraries <- c('data.table', 'knitr','testthat', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr','mlbench', 'caret', 'ROCR', 'miscset', 'plotrix', 'cluster')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)
rm(list=ls())

# Creación de carpetas para los datos
if(!file.exists("../M3-data")){dir.create("../M3-data")}

## Carga, lectura de datos y fecha
fileURL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00320/student.zip"
download.file(fileURL,destfile="../M3-data/student.zip")
unzip("../M3-data/student.zip", exdir="../M3-data")
list.files("../M3-data")
fechaDescarga <- date()
studentMat <- read.table("../M3-data/student-mat.csv", 
                         row.names=NULL, sep=";", header=TRUE)
#comprobamos nuestro data frame
str(studentMat)
#tenemos 33 variables de las cuales 16 son numericas y 17 categoricas (4 de ellas nominales y 13 de tipo binario)
#Comprobamos la existencia de NA
sapply(studentMat, function(x) sum(is.na(x))) # No tenemos valores NA
# Analiizamos la dsitribucion de las variables categoricas
library(miscset)
library(ggplot2)
ggplotGrid(ncol = 3,
           lapply(c("school", "sex", "address", "famsize","Pstatus","Mjob","Fjob","reason","guardian","schoolsup","famsup","paid","activities","nursery","higher","internet","romantic"),
                  function(col) {
                    ggplot(studentMat, aes_string(col)) + geom_bar() + coord_flip() 
                  }))
# La gran mayoria van al colegio GP
# Mismo numero de chicas y chicos
# La gran mayoria viven en ciudad
# Gran numero de familias nuemrosas
# Gran mayoria de padres juntos
# Mjob other y sector servicios
# Fjob other y sector servicios
# reason para elegir el colegio course y cercania a casa
# Gran mayoria nota final
# Gran mayoria no beca
# Practicamente mismon numero de alumnos que pagan clases particulares 
# mismo numero activities
# Gran mayoria guarderia
# Gran mayoria quiere ir universidad 
# Gran mayoria tiene internet
# Gran mayoria soltero
# Analisis de varaibles categoricas nominales frente a G3
par(mfrow=c(2,2))
plot(studentMat$Mjob, studentMat$G3, main="G3 by Mjob")
plot(studentMat$Fjob, studentMat$G3, main="G3 by Fjob")
plot(studentMat$reason, studentMat$G3, main="G3 by reason")
plot(studentMat$guardian, studentMat$G3, main="G3 by guardian")
grid.arrange(p1,p2,p3,p4, layout_matrix = rbind(c(1,1,1),c(2,3,4)))
# No se oberva una variacion significativa en la nota final en funcion de el tipo de trabajo de la madre o el padre,
# la razon por a queeligieron el colegio o quien este a su cargo en casa.
# Analisis de variables numericas y categoricas tipo binario 
# Creamos una variable dummy pass que sera 1 si G3>9 y 0 en caso contrario
studentMat$pass <- ifelse(studentMat$G3>9, 1,0)
#Recodificacion de variables categoricas binarias
studentMat$school <- ifelse(studentMat$school == "GP", 1, 0)
studentMat$sex <- ifelse(studentMat$sex == "F", 1, 0)
studentMat$address <- ifelse(studentMat$address == "U", 1, 0)
studentMat$famsize <- ifelse(studentMat$famsize == "GT3", 1, 0)
studentMat$Pstatus <- ifelse(studentMat$Pstatus == "T", 1, 0)
studentMat$schoolsup <- ifelse(studentMat$schoolsup == "yes", 1, 0)
studentMat$famsup <- ifelse(studentMat$famsup == "yes", 1, 0)
studentMat$paid <- ifelse(studentMat$paid == "yes", 1, 0)
studentMat$activities <- ifelse(studentMat$activities == "yes", 1, 0)
studentMat$nursery <- ifelse(studentMat$nursery == "yes", 1, 0)
studentMat$higher <- ifelse(studentMat$higher == "yes", 1, 0)
studentMat$internet <- ifelse(studentMat$internet == "yes", 1, 0)
studentMat$romantic <- ifelse(studentMat$romantic == "yes", 1, 0)
# Creamos la matriz de correlacion para los estudiantes de matematicas
plot.new()
library(corrplot)
matCorMat <- cor(studentMat %>% select(- Mjob,-Fjob, -reason, -guardian))
matCorMat[is.na(matCorMat)] <- 0
col <- colorRampPalette(c('#BB4444', '#EE9988', '#FFFFFF', '#77AADD', '#4477AA'))
corrplot(matCorMat, method = "color", tl.srt = 35, tl.col = 'black',
         main = 'Fig. 3 Predictores de la matriz de correlación de estudiantes de Matemáticas')
# 3. Metodo No supervisado clustering k-means
if(! "dplyr" %in% installed.packages()) install.packages("dplyr", depend = TRUE) 
if(! "plotrix" %in% installed.packages()) install.packages("plotrix", depend = TRUE) 
if(! "knitr" %in% installed.packages()) install.packages("knitr", depend = TRUE) 
library(knitr)
library(dplyr)
library(plotrix)
studentMat2 <- studentMat %>% select(- Mjob,-Fjob, -reason, -guardian)
# Calucamos numero de clusters con Elbow
mydata <- studentMat2 
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
for (i in 2:15) wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Numero de Clusters", 
     ylab="Sumas de cuadrados dentro de los grupos", 
     main="Num de clusters óptimo según Elbow", pch=20, cex=2)
#k=5
#Aplicamos entonces kmeans() con k=6
set.seed(1234) 
kmeansMat.clust <- kmeans(studentMat2, 5) 
kmeansMat.clust
kmeansMat.clust$size
library(cluster)
plot.new()
#2chart
clusplot(studentMat2, kmeansMat.clust$cluster, main='2D representation of the Cluster solution Matematicas',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)
plot(studentMat2 %>% select(G1, pass), col = kmeansMat.clust$cluster)
points(as.data.frame(kmeansMat.clust$centers) %>% select(G1, G3), pch = 8, cex = 2)

plot(studentMat2 %>% select(G2, pass), col = kmeansMat.clust$cluster)
points(as.data.frame(kmeansMat.clust$centers) %>% select(G1, G3), pch = 8, cex = 2)

par(mfrow=c(1,2))
radial.plot(kmeansMat.clust$centers[3,], labels = names(kmeansMat.clust$centers[3,]), rp.type = "s", point.symbol = 15, point.col = "blue", mar = c(1,0.5,1,3), radial.lim = c(0,20))
radial.plot(kmeansMat.clust$centers[2,], labels = names(kmeansMat.clust$centers[2,]), rp.type = "s", point.symbol = 15, point.col = "red", mar = c(1,0.5,1,3), radial.lim = c(0,20))
# Analisis del cluster mayoritario: cluster 3
studentMat_final<- studentMat2 %>% mutate(cluster_id = kmeansMat.clust$cluster)
studentMat_cluster3 <- subset(studentMat_final, studentMat_final$cluster_id == "3")
kable(head(studentMat_cluster3))
summary(studentMat_cluster3)
mean(studentMat_cluster3$age)
mean(studentMat_cluster3$G3)
mean(studentMat_cluster3$absences)
mean(studentMat_cluster3$Walc)
## Analisis basado en dos modelos no supervisados
library(mlbench) 
library(caret) 
library(dplyr)
library(ROCR)
#convertimos la variable pass a tipofactor "pass" si "G3>9 y "fail" en caso contrario
studentMat2$pass <- ifelse(studentMat$G3>9, "pass", "fail")
studentMat2$pass <- as.factor(studentMat2$pass)
str(studentMat2)
library(ggplot2)
prop.table(table(studentMat2$pass))
index.studentMat2 <- createDataPartition(studentMat2$pass, p=0.8, list=F)
train.studentMat2 <- studentMat2[index.studentMat2,] 
test.studentMat2 <- studentMat2[ -index.studentMat2, ]
prop.table( table(train.studentMat2$pass))
prop.table( table(test.studentMat2$pass))

zero.var.train.studentMat2 <- nearZeroVar( train.studentMat2[,-dim(train.studentMat2)[2]], saveMetrics=F )
colnames(train.studentMat2)[zero.var.train.studentMat2]
#train.studentMat2.nz <- train.studentMat2[,-zero.var.train.studentMat2]

#zero.var.test.studentMat2 <- nearZeroVar( test.studentMat2[,-dim(test.studentMat2)[2]], saveMetrics=F )
#colnames(test.studentMat2)[zero.var.test.studentMat2]
#test.studentMat2.nz <- test.studentMat2[,-zero.var.test.studentMat2]


cor.train.studentMat2.matrix <- cor( train.studentMat2[, -dim(train.studentMat2)[2]] )
cor.train.studentMat2.index <- findCorrelation( cor.train.studentMat2.matrix, 0.75 ) 
cor.train.studentMat2.index
cor.train.studentMat2 <- train.studentMat2[,-cor.train.studentMat2.index] 
dim(cor.train.studentMat2)
cor.test.studentMat2 <- test.studentMat2[,-cor.train.studentMat2.index] 
dim(cor.test.studentMat2)
## Preprocesado###
xTrans.studentMat2 <- preProcess(cor.train.studentMat2[, -dim(cor.train.studentMat2)[2]]) 
train.studentMat2.prep <- predict( xTrans.studentMat2, cor.train.studentMat2[,-dim(cor.train.studentMat2)[2]]) 
train.studentMat2.prep$pass <- cor.train.studentMat2$pass
test.studentMat2.prep <- predict( xTrans.studentMat2, cor.test.studentMat2[,-dim(cor.test.studentMat2)[2]]) 
test.studentMat2.prep$pass <- cor.test.studentMat2$pass
###Generacion de modelos##
#1. Modelo KNN
knn.control <- trainControl(method="repeatedcv", repeats=5)
knn.studentMat2.model <- train(x=train.studentMat2.prep[,-dim(train.studentMat2.prep)[2]], y=train.studentMat2.prep$pass, method="knn", tuneLength=10, trControl=knn.control)
knn.studentMat2.model
plot1 <- plot(knn.studentMat2.model, metric="Accuracy") 
print(plot1)
###Prediccion sobre el modelo Knn
knn.studentMat2.test <- predict(knn.studentMat2.model, newdata= test.studentMat2.prep[,-dim(train.studentMat2.prep)[2]]) 
knn.studentMat2.test
# Extracción de prediciones 
knn.studentMat2.test.preds <- extractPrediction( list(model1=knn.studentMat2.model), testX=test.studentMat2.prep[,-dim(train.studentMat2.prep)[2]] , testY=test.studentMat2.prep$pass)
conjunto.test.preds <- subset(knn.studentMat2.test.preds, dataType== "Test") 
head(conjunto.test.preds)
# Extraccion de probabilidades 
knn.studentMat2.test.probs <- extractProb( list(model1=knn.studentMat2.model), testX= test.studentMat2.prep[,-dim(train.studentMat2.prep)[2]], testY=test.studentMat2.prep$pass) 
conjunto.test.probs <- subset(knn.studentMat2.test.probs, dataType== "Test") 
plotClassProbs(conjunto.test.probs )
#Evaluación del modelo, matriz de confusión y curvas ROC.
confusionMatrix(knn.studentMat2.test, test.studentMat2.prep$pass )
pr <- prediction(ifelse(knn.studentMat2.test == 'pass',1,0), ifelse(test.studentMat2.prep$pass == 'pass',1,0)) 
prf <- performance(pr, measure = "tpr", x.measure = "fpr") 
plot(prf)
#2 Modelo SVM
svm.control <- trainControl(method="repeatedcv", repeats=5) 
svm.studentMat2.model <- train(x=train.studentMat2.prep[,-dim(train.studentMat2.prep)[2]], y=train.studentMat2.prep$pass, method="svmRadial", tuneLength=10, trControl=svm.control)
svm.studentMat2.model
plot1 <- plot(svm.studentMat2.model, metric="Accuracy") 
print(plot1)
#Preccion
svm.studentMat2.test <- predict(svm.studentMat2.model, newdata= test.studentMat2.prep[,-dim(train.studentMat2.prep)[2]]) 
svm.studentMat2.test
# Evaluación del modelo, matriz de confusión y curvas ROC
confusionMatrix(svm.studentMat2.test, test.studentMat2.prep$pass )
pr <- prediction(ifelse(svm.studentMat2.test == 'pass',1,0), ifelse(test.studentMat2.prep$pass == 'pass',1,0))
prf <- performance(pr, measure = "tpr", x.measure = "fpr") 
plot(prf)
# 3 Comparación de Modelos: KNN vs SVM
models <- list( knn.studentMat2.model, svm.studentMat2.model ) 
compar.models <- resamples( models ) 
summary( compar.models )
dotplot( compar.models)



