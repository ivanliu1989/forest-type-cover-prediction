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
# Detect near zero variance
    nzv <- nearZeroVar(train,saveMetrics = T)
    str(nzv,vec.len = 2)
# Find linear combos
    attach(train)
    fc <- findLinearCombos(train)
    fc
# Plotting data
    plot(Cover_Type,Elevation)
# Train the model
    fit1 <- train(Cover_Type~., method="rpart",data = train)
    fit2 <- train(Cover_Type~., method="rf",data = train)
    fit3 <- svm()