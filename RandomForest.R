install.packages("randomForest")
install.packages("pROC")
library(randomForest)

#S1: deal data
mushroomData=read.csv("mushrooms.csv",stringsAsFactors=TRUE)
summary(mushroomData)
mushroomData$type=as.factor(mushroomData$type)
head(mushroomData)
dim(mushroomData)
str(mushroomData)

#S2: set training and testing data
mushroom=mushroomData[order(runif(8124)),]
mushroom_train=mushroom[1:6000,]
mushroom_test=mushroom[6001:8124,]

#S3: build model 
model=randomForest(type~.,data=mushroom_train,ntree=500,mtry=3,importance=TRUE,
                   proximity=TRUE,type=classification,suset=sampleData)
plot(model)
model$importance
varImpPlot(model, main = "variable importance")

#S4: verify model
pre_ran = predict(model,newdata=mushroom_test)
obs_p_ran = data.frame(prob=pre_ran,obs=mushroom_test$type)
library(gmodels)
CrossTable(pre_ran,mushroom_test$type,prop.chisq = FALSE,
           prop.c=FALSE,prop.r = FALSE,
           dnn=c('predicted','actual'))

library(pROC)
ran_roc = roc(mushroom_test$type,as.numeric(pre_ran))
plot(ran_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='Random Forest ROC,mtry=3,ntree=500')


