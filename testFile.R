# Setup env
    setwd("/Users/ivan/Work_directory/ForestType")
    library(caret);library(data.table)
    dir()
# Load data
    system.time(train <- fread("train.csv"))
    system.time(test <- fread("test.csv"))
    test$Cover_Type <- NA
    sample <- fread("sampleSubmission.csv")
# Preprocessing data
    str(train)
    str(test)
    head(sample)
# Missing value
    sum(is.na(train))
# Detect near zero variance
    nzv <- nearZeroVar(train,saveMetrics = T)
    str(nzv,vec.len = 2)
# Find linear combos
    fc <- findLinearCombos(train)
    fc
# Plotting data
    boxplot(train$Cover_Type,train$Elevation)
# Train the model
    fit1 <- train(Cover_Type~., method="rpart",data = train)
    fit2 <- train(Cover_Type~., method="rf",data = train)
    fit3 <- svm()
    summary(fit1)
    require(rattle)
    fancyRpartPlot(fit1)
# Predict model
    pred1 <- predict(fit1,test)
    pred1.output <- data.frame(Id=test$Id, Cover_Type=round(pred1,digits = 0))
    head(pred1.output)
    table(pred1.output$Cover_Type)
    table(train$Cover_Type)
    write.table(pred1.output, "pred1.csv",sep = ",",row.names = F)
