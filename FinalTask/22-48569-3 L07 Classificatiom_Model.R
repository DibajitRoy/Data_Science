
# 1. Load Libraries
install.packages(c("caret", "rpart", "rpart.plot"))
library(caret)
library(rpart)
library(rpart.plot)


# 2. Load Dataset

data <- read.csv("F://Desktop//Original_data_with_more_rows.csv")
head(data)
str(data)

# 3. Create Target Variable

data$AverageScore <- rowMeans(
  data[, c("MathScore", "ReadingScore", "WritingScore")]
)
data$Performance <- ifelse(data$AverageScore >= 50, "Pass", "Fail")
data$Performance <- as.factor(data$Performance)

# Remove unnecessary column
data$Unnamed.0 <- NULL

#  Train-Test Split

set.seed(123)
trainIndex <- createDataPartition(data$Performance, p = 0.7,list = FALSE)
trainData <- data[trainIndex, ]
testData  <- data[-trainIndex, ]

# Decision Tree Model

model_dt <- rpart(
  Performance ~ Gender + EthnicGroup + ParentEduc +
    LunchType + TestPrep + MathScore + ReadingScore + WritingScore,
  data = trainData,
  method = "class"
)

print(model_dt)
rpart.plot(model_dt)


# Prediction (Decision Tree)

pred_dt <- predict(model_dt,newdata = testData,type = "class")


# Evaluation (

conf_mat <- confusionMatrix( pred_dt,testData$Performance)
conf_mat
accuracy <- conf_mat$overall["Accuracy"]
accuracy

#  Logistic Regression (Binary Classification)

model_log <- glm(
  Performance ~ MathScore + ReadingScore + WritingScore,data = trainData,family = binomial)

summary(model_log)

# Prediction & Evaluation

prob <- predict(model_log,newdata = testData,type = "response")
pred_log <- ifelse(prob > 0.5, "Pass", "Fail")
pred_log <- as.factor(pred_log)

confusionMatrix(pred_log,testData$Performance)
