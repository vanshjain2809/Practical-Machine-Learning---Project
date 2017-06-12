library(caret)
library(corrplot)

fileUrl1 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
fileUrl2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(fileUrl1,destfile = "training.csv")
download.file(fileUrl2,destfile = "testing.csv")

training <- read.csv("training.csv")
testing <- read.csv("testing.csv")

data_test_NAs <- apply(testing, 2, function(x) {sum(is.na(x))})
data_test_clean <- testing[,which(data_test_NAs == 0)]
data_test_clean <- data_test_clean[8:length(data_test_clean)]

a <- names(data_test_clean)

data_train_clean <- training[a[1:52]]
data_train_clean$classe <- training$classe


inTrain <- createDataPartition(y = data_train_clean$classe, p = 0.7, list = FALSE)
new_train <- data_train_clean[inTrain, ]
new_test <- data_train_clean[-inTrain, ]


corrplot(new_test, order = "FPC", method = "circle", type = "lower", tl.cex = 0.8,  tl.col = rgb(0, 0, 0))


boostFit <- train(classe ~ ., method = "gbm", data = new_train, verbose = TRUE, trControl = trainControl(method = "cv", number = 10))

pred1 <- predict(boostFit,new_test)
confusionMatrix(new_test$classe,pred1)

final_predictions <- predict(boostFit,data_test_clean)






