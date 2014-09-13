# Data preProcess
setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\forest-type-cover-prediction")
train = read.csv("train.csv")
train$Wilderness_Area1=as.factor(train$Wilderness_Area1)
train$Wilderness_Area2=as.factor(train$Wilderness_Area2)
train$Wilderness_Area3=as.factor(train$Wilderness_Area3)
train$Wilderness_Area4=as.factor(train$Wilderness_Area4)
train$Soil_Type1=as.factor(train$Soil_Type1)
train$Soil_Type2=as.factor(train$Soil_Type2)
train$Soil_Type3=as.factor(train$Soil_Type3)
train$Soil_Type4=as.factor(train$Soil_Type4)
train$Soil_Type5=as.factor(train$Soil_Type5)
train$Soil_Type6=as.factor(train$Soil_Type6)
train$Soil_Type7=as.factor(train$Soil_Type7)
train$Soil_Type8=as.factor(train$Soil_Type8)
train$Soil_Type9=as.factor(train$Soil_Type9)
train$Soil_Type10=as.factor(train$Soil_Type10)
train$Soil_Type11=as.factor(train$Soil_Type11)
train$Soil_Type12=as.factor(train$Soil_Type12)
train$Soil_Type13=as.factor(train$Soil_Type13)
train$Soil_Type14=as.factor(train$Soil_Type14)
train$Soil_Type15=as.factor(train$Soil_Type15)
train$Soil_Type16=as.factor(train$Soil_Type16)
train$Soil_Type17=as.factor(train$Soil_Type17)
train$Soil_Type18=as.factor(train$Soil_Type18)
train$Soil_Type19=as.factor(train$Soil_Type19)
train$Soil_Type20=as.factor(train$Soil_Type20)
train$Soil_Type21=as.factor(train$Soil_Type21)
train$Soil_Type22=as.factor(train$Soil_Type22)
train$Soil_Type23=as.factor(train$Soil_Type23)
train$Soil_Type24=as.factor(train$Soil_Type24)
train$Soil_Type25=as.factor(train$Soil_Type25)
train$Soil_Type26=as.factor(train$Soil_Type26)
train$Soil_Type27=as.factor(train$Soil_Type27)
train$Soil_Type28=as.factor(train$Soil_Type28)
train$Soil_Type29=as.factor(train$Soil_Type29)
train$Soil_Type30=as.factor(train$Soil_Type30)
train$Soil_Type31=as.factor(train$Soil_Type31)
train$Soil_Type32=as.factor(train$Soil_Type32)
train$Soil_Type33=as.factor(train$Soil_Type33)
train$Soil_Type34=as.factor(train$Soil_Type34)
train$Soil_Type35=as.factor(train$Soil_Type35)
train$Soil_Type36=as.factor(train$Soil_Type36)
train$Soil_Type37=as.factor(train$Soil_Type37)
train$Soil_Type38=as.factor(train$Soil_Type38)
train$Soil_Type39=as.factor(train$Soil_Type39)
train$Soil_Type40=as.factor(train$Soil_Type40)
train$Cover_Type=as.factor(train$Cover_Type)
train$Soil_Type7=NULL
train$Soil_Type15=NULL
train$Id=NULL
summary(train)
train$Cover_Type = as.character(train$Cover_Type)
train$Cover_Type[train$Cover_Type == "1"] <- "Seg1"
train$Cover_Type[train$Cover_Type == "2"] <- "Seg2"
train$Cover_Type[train$Cover_Type == "3"] <- "Seg3"
train$Cover_Type[train$Cover_Type == "4"] <- "Seg4"
train$Cover_Type[train$Cover_Type == "5"] <- "Seg5"
train$Cover_Type[train$Cover_Type == "6"] <- "Seg6"
train$Cover_Type[train$Cover_Type == "7"] <- "Seg7"
train$Cover_Type = as.factor(train$Cover_Type)
x_vars = setdiff(names(train),c("Cover_Type"))

# GBM model
library(caret)
set.seed(1234)    
Grid <-  expand.grid(
    n.trees = c(1000),
    interaction.depth = c(22) ,
    shrinkage = 0.2)
fitControl <- trainControl(method = "none",allowParallel = T, classProbs = TRUE)
GBMmodel <- train(Cover_Type ~ ., data = train, method = "gbm", trControl = fitControl, verbose = TRUE,
                  tuneGrid = Grid, metric = "ROC")
pred1 = predict(GBMmodel, newdata = train)
confusionMatrix(pred1, train$Cover_Type)

# RF model
set.seed(1234)
tc <- trainControl("repeatedcv", 10, allowParallel = T, classProbs=TRUE, savePred=T) 
fit2 <- train(Cover_Type ~ ., data = train, method = "rf", trControl = tc, verbose=T, prox=T)
pred2 <- predict(fit2, newdata = train)
confusionMatrix(pred2, train$Cover_Type)
getTree(fit2$finalModel, 2)
classCenter(x, label, prox)


# NB model
set.seed(1234)
tc <- trainControl("repeatedcv", 10, allowParallel = T,classProbs=TRUE, savePred=T) 
fit3 <- train(Cover_Type ~ ., data = train, method = "nb", trControl = tc, verbose=T)
pred3 <- predict(fit3, newdata = train)
confusionMatrix(pred3, train$Cover_Type)

# Ensembling model
ensemble.data <- data.frame(pred1,pred2,train)
ensembleFit <- train(Cover_Type ~ ., data=ensemble.data, method='gam', verbose=T)
ensemblePred <- predict(ensembleFit, newdata=train)
gc()

# test data
test = read.csv("test.csv")
test$Wilderness_Area1=as.factor(test$Wilderness_Area1)
test$Wilderness_Area2=as.factor(test$Wilderness_Area2)
test$Wilderness_Area3=as.factor(test$Wilderness_Area3)
test$Wilderness_Area4=as.factor(test$Wilderness_Area4)
test$Soil_Type1=as.factor(test$Soil_Type1)
test$Soil_Type2=as.factor(test$Soil_Type2)
test$Soil_Type3=as.factor(test$Soil_Type3)
test$Soil_Type4=as.factor(test$Soil_Type4)
test$Soil_Type5=as.factor(test$Soil_Type5)
test$Soil_Type6=as.factor(test$Soil_Type6)
test$Soil_Type7=as.factor(test$Soil_Type7)
test$Soil_Type8=as.factor(test$Soil_Type8)
test$Soil_Type9=as.factor(test$Soil_Type9)
test$Soil_Type10=as.factor(test$Soil_Type10)
test$Soil_Type11=as.factor(test$Soil_Type11)
test$Soil_Type12=as.factor(test$Soil_Type12)
test$Soil_Type13=as.factor(test$Soil_Type13)
test$Soil_Type14=as.factor(test$Soil_Type14)
test$Soil_Type15=as.factor(test$Soil_Type15)
test$Soil_Type16=as.factor(test$Soil_Type16)
test$Soil_Type17=as.factor(test$Soil_Type17)
test$Soil_Type18=as.factor(test$Soil_Type18)
test$Soil_Type19=as.factor(test$Soil_Type19)
test$Soil_Type20=as.factor(test$Soil_Type20)
test$Soil_Type21=as.factor(test$Soil_Type21)
test$Soil_Type22=as.factor(test$Soil_Type22)
test$Soil_Type23=as.factor(test$Soil_Type23)
test$Soil_Type24=as.factor(test$Soil_Type24)
test$Soil_Type25=as.factor(test$Soil_Type25)
test$Soil_Type26=as.factor(test$Soil_Type26)
test$Soil_Type27=as.factor(test$Soil_Type27)
test$Soil_Type28=as.factor(test$Soil_Type28)
test$Soil_Type29=as.factor(test$Soil_Type29)
test$Soil_Type30=as.factor(test$Soil_Type30)
test$Soil_Type31=as.factor(test$Soil_Type31)
test$Soil_Type32=as.factor(test$Soil_Type32)
test$Soil_Type33=as.factor(test$Soil_Type33)
test$Soil_Type34=as.factor(test$Soil_Type34)
test$Soil_Type35=as.factor(test$Soil_Type35)
test$Soil_Type36=as.factor(test$Soil_Type36)
test$Soil_Type37=as.factor(test$Soil_Type37)
test$Soil_Type38=as.factor(test$Soil_Type38)
test$Soil_Type39=as.factor(test$Soil_Type39)
test$Soil_Type40=as.factor(test$Soil_Type40)

# Perform prediction on test data
testPred1 = predict(GBMmodel, newdata = test[,x_vars])
testPred2 = predict(fit2, newdata = test[,x_vars])
testPred3 = predict(fit3, newdata = test[,x_vars])

# Create file for submission
submission = data.frame(Id = test$Id, Cover_Type = as.character(gsub("Seg", "", testPred3)))
write.csv(submission, "predNB.csv", row.names=FALSE)
