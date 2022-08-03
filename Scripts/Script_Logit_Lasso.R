setwd("C:/Users/Diana Contreras/OneDrive - Universidad de los Andes/1.Big Data/Trajabajo Final")
install.packages("stargazer")
require(data.table)
library(dplyr)
library(tidyverse)
require(caret)
library(glmnet)
library(ggplot2)
library(pROC)
library(themis)
library(cowplot)
library(stargazer)

str(final2)
final2$sexo<-as.character(final2$sexo)
final2$sexo<-tolower(final2$sexo)
final2$sexo<-factor(final2$sexo)
final2$migra_ult3<-factor(final2$migra_ult3)
final2$dejo_estudiar<-factor(final2$dejo_estudiar)
final2$sp_estrato<-factor(final2$sp_estrato)
final2$padre_presente<-factor(final2$padre_presente)
final2$madre_presente<-factor(final2$madre_presente)
#Miramos que tan balnaceada está la base de datos, vemos que solo el 0.04% de niños trabajan.
prop.table(table(final2$trabajo))

#Partición de la base

set.seed(1111)

split1<- createDataPartition(final2$trabajo, p=.7)[[1]]
length(split1)/nrow(final2) #Quedó bien

trainSplit<-final2[split1,]
testSplit<-final2[-split1,]

nrow(trainSplit)/nrow(final2)
nrow(testSplit)/nrow(final2)

split2<-createDataPartition(testSplit$trabajo, p=1/3)[[1]]

testing <-testSplit[-split2,]
evaluation <-testSplit[split2,]

nrow(testing)/nrow(final2)
nrow(evaluation)/nrow(final2) #20% de la muestra y 10%, bien

#Primero voy ha hacer el modelo sin balancear la base, luego balanceando
#Logit-------------------------------------------------------------------
set.seed(1111)
logit<-train(trabajo~.,
             data=trainSplit,
             method="glm",
             trControl=trainControl("cv", number =10, savePredictions=T),
             family="binomial",
             preProcess=c("center", "scale"),
             na.action=na.omit)
logit$finalModel
#Regularización----------------------------------------------------------
el<- train(trabajo~., 
           data=trainSplit, 
           method= "glmnet",
           trControl=trainControl("cv", number =10, savePredictions="all"),
           preProcess=c("center", "scale"),
           na.action=na.omit)
el
models<- list(ridge= logit, elastic=el)
colnames(trainSplit)

#Coeficientesl
coef_elastic<-as.matrix(round(coef(el$finalModel, el$bestTune$lambda),3))

#Importancia  de las variables
imp_el<-as.data.frame(varImp(el)$importance)
imp_el$Var<-row.names(imp_el)

iel <-ggplot(imp_el, aes(x=reorder(Var, -Overall), y=Overall)) +
  geom_bar(stat="identity", fill="coral2") + 
  theme_classic()+
  labs(y="", x="")+
  ggtitle("Elastinc net, lambda=0.1") +
  theme(axis.text.x = element_text(angle=90, size=6, vjust = 0.5))

#Alternative cutoff-----------------------------------------------------
evaluation<-na.omit(evaluation)
evalresults<-data.frame(evaluation$trabajo)

#Predecimos
evalresults$logit<-predict(logit, newdata=evaluation, type="prob")[,1]
evalresults$elastic<-predict(el, newdata=evaluation, type="prob")[,1]

#AUC
rfROC<-roc(evalresults$evaluation.trabajo, evalresults$elastic) #elastic
rfROC #0.558
rfROC4<-roc(evalresults$evaluation.trabajo, evalresults$logit) #logit
rfROC4 #AUC 0.5912

#Metricas, umbral
rfThresh_el <-coords(rfROC, x="best", best.method ="closest.topleft") #0.0284
rfThresh_lo <-coords(rfROC4, x="best", best.method ="closest.topleft") #0.0226

Thresh_el<-rfThresh_el$threshold
Thresh_lo<-rfThresh_lo$threshold 
#UPSAMPLIN-----------------------------------------------------------------------

set.seed(1111)
receta <- recipe(trabajo ~ ., data = trainSplit) %>%
  themis::step_upsample(trabajo) #Upsample
training_up <- bake(prep(receta), new_data=NULL)
prop.table(table(training_up$trabajo))

#Logit
set.seed(1111)
logit_up<-train(trabajo~.,
                data=training_up,
                method="glm",
                trControl=trainControl("cv", number =10, savePredictions=T),
                family="binomial",
                preProcess=c("center", "scale"),
                na.action=na.omit)
#Elastic Net

el_up<- train(trabajo~., 
              data=training_up, 
              method= "glmnet",
              trControl=trainControl("cv", number =10, savePredictions=T),
              preProcess=c("center", "scale"),
              na.action=na.omit) # lambda=0.002707
#Predicción por fuera de muestra------------------------------------
testing<-na.omit(testing)
test_Results<-data.frame(trabajo=testing$trabajo)

test_Results$el<-predict(el, 
                         newdata=testing,
                         type="prob")[,1]
test_Results$logit<-predict(logit, 
                            newdata=testing,
                            type="prob")[,1]
test_Results$logit_up<-predict(logit_up, 
                               newdata=testing,
                               type="prob")[,1]
test_Results$el_up<-predict(el_up, 
                            newdata=testing,
                            type="prob")[,1]

#Clasificación segun el umbral
test_Results2 <- test_Results %>% 
  mutate(logit_thresh=ifelse(logit>Thresh_lo, "2", "1"),
         elasticN_thresh=ifelse(el>Thresh_el, "2", "1"),
         logit_up=ifelse(logit_up>0.5, "2", "1"),
         elasticN_up=ifelse(el_up>0.5, "2", "1"))
#Confusion matrix-----------------------------
#Pongo todos en los mismos niveles
t<-test_Results2
t$logit_thresh <- factor(t$logit_thresh,levels=c(1,2), labels=c("No", "Si"))
t$elasticN_thresh <- factor(t$elasticN_thresh,levels=c(1,2), labels=c("No", "Si"))
t$elasticN_up <- factor(t$elasticN_up,levels=c(1,2), labels=c("No", "Si"))
t$logit_up <- factor(t$logit_up,levels=c(1,2), labels=c("No", "Si"))

#Sensibilidad
s0<-caret::sensitivity(t$logit_thresh, t$trabajo, positive="Si")
s3<-caret::sensitivity(t$elasticN_thresh, t$trabajo, positive="Si")
s4<-caret::sensitivity(t$elasticN_up, t$trabajo, positive="Si")
s5<-caret::sensitivity(t$logit_up, t$trabajo, positive="Si")

caret::confusionMatrix(t$logit_thresh, t$trabajo, positive="Si")
caret::confusionMatrix(t$elasticN_thresh, t$trabajo, positive="Si")
caret::confusionMatrix(t$elasticN_up, t$trabajo, positive="Si")
caret::confusionMatrix(t$logit_up, t$trabajo, positive="Si")
lo<-logit$finalModel
tbl_regression(logit$finalModel)%>%
  modify_caption("**Tabla 1. Estadisticas descriptivas**") %>% 
  as_flex_table() %>%
  flextable::save_as_docx(path="Modelo.docx")
stargazer(lo, type="text", title= "Logit niño trabaja", align = TRUE, out="model.text")
l