# Setup env
    setwd("/Users/ivan/Work_directory/ForestType")
    library(caret);library(data.table)
    dir()
# Load data
    system.time(train <- fread(dir()[6]))
    system.time(test <- fread(dir()[3]))
    sample <- fread(dir()[1])
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
    plot(train$Cover_Type,train$Elevation)
# Train the model
    fit1 <- train(Cover_Type~., method="rpart",data = train)
    fit2 <- train(Cover_Type~., method="rf",data = train)
    fit3 <- svm()
    summary(fit1)
# Predict model
    pred1 <- predict(fit1,train)
    pred1
    train$Cover_Type
