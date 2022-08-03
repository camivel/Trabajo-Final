##Trabajo aplicación: Clasificación tranajo infantil

require(pacman)
p_load(tidyverse , rio , caret ,  modelsummary , gamlr,
       ROCR, # ROC
       pROC, ggplot2, doParallel, rattle, MLmetrics,
       janitor, fastDummies, tidymodels)
library(MASS)
library(tree)
library(ISLR)
library(randomForest)

#Convertir a factor las variables categóricas

prop.table(table(final2$trabajo))
final2$migra_ult3<-as.factor(final2$migra_ult3)
final2$dejo_estudiar<-as.factor(final2$dejo_estudiar)
final2$sp_estrato<-as.factor(final2$sp_estrato)
final2$padre_presente<-as.factor(final2$padre_presente)
final2$madre_presente<-as.factor(final2$madre_presente)

#Crear bases de entrenamiento y prueba
set.seed(1111)

sample <- sample(c(TRUE, FALSE), nrow(final2), replace=TRUE, prob=c(0.7,0.3))
training  <- final2[sample, ]
testing   <- final2[!sample, ]


#### Estimación Modelo 1 - árbol simple
set.seed(1111)
arbol1<- tree(trabajo ~., training)
predict1<- predict(arbol1, testing, type = "class")
confusionMatrix(testing$trabajo, predict1)
summary(arbol1)


### Estimación Modelo 2 - árbol simple con upsample
set.seed(1111)

receta <- recipe(trabajo ~ ., data = training) %>%
  themis::step_upsample(trabajo) #Upsample
training_smote <- bake(prep(receta), new_data=NULL)
prop.table(table(training_smote$trabajo))

arbol2<- tree(trabajo ~., training_smote)
predict2<- predict(arbol2, testing, type = "class")
confusionMatrix(testing$trabajo, predict2)
summary(arbol2)

### Estimación Modelo 3 - árbol 2 con número óptimo de terminales
set.seed(1111)
cvarbol<- cv.tree(arbol2, FUN = prune.misclass)
cvarbol

arbol3<- prune.misclass(arbol2, best = 14) #14 nodos terminales
predict3<- predict(arbol3, testing, type = "class")
confusionMatrix(testing$trabajo, predict3)
summary(arbol3)

### Estimación Modelo 4 - Random Forest
set.seed(1111)
RForest<- randomForest(trabajo ~., data=training_smote, importance=TRUE, proximity=TRUE)
RForest

predict4<- predict(RForest, testing, type="class")
confusionMatrix(testing$trabajo, predict4)
summary(RForest)
varImpPlot(RForest)




