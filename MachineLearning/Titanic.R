## Titanic Exercises
library(titanic)
library(caret)
library(tidyverse)
library(rpart)
options(digits = 3)

# clean data
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

# split into train_dataing and test sets
dim(titanic_clean)
set.seed(42, sample.kind = "Rounding")
testind <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
test <- titanic_clean[testind,]
train_data <- titanic_clean[-testind,]
dim(test)
dim(train_data)
mean(train_data$Survived == 1)

# baseline prediction by guessing
set.seed(3, sample.kind = "Rounding")
ntest <- length(test$Survived)
guess <- sample(c(0,1), ntest, replace = TRUE) %>% as.factor()
confusionMatrix(test$Survived, guess)

# examine train_dataing set for sex differences
table(train_data$Sex, train_data$Survived)
sum(train_data$Sex == "female" & train_data$Survived == 1)/sum(train_data$Sex == "female")
sum(train_data$Sex == "male" & train_data$Survived == 1)/sum(train_data$Sex == "male")

# predicting survival on test set: 1 for female, 0 for male
predictions_sex <- ifelse(test$Sex == "female", 1, 0) %>% as.factor()
confusionMatrix(test$Survived, predictions_sex)

# examining classes in train_dataing set
table(train_data$Pclass, train_data$Survived)
sum(train_data$Pclass == 1 & train_data$Survived == 1)/sum(train_data$Pclass == 1)
sum(train_data$Pclass == 2 & train_data$Survived == 1)/sum(train_data$Pclass == 2)
sum(train_data$Pclass == 3 & train_data$Survived == 1)/sum(train_data$Pclass == 3)

# predict based on class: 1 for first class, 0 otherwise
predictions_class <- ifelse(test$Pclass == 1, 1, 0) %>% as.factor()
confusionMatrix(test$Survived, predictions_class)

# combining prediction features
table(train_data$Pclass, train_data$Survived, train_data$Sex)
sum(train_data$Sex == "female" & train_data$Pclass == 1 & train_data$Survived == 1)/
  sum(train_data$Sex == "female" & train_data$Pclass == 1)
sum(train_data$Sex == "female" & train_data$Pclass == 2 & train_data$Survived == 1)/
  sum(train_data$Sex == "female" & train_data$Pclass == 2)
sum(train_data$Sex == "female" & train_data$Pclass == 3 & train_data$Survived == 1)/
  sum(train_data$Sex == "female" & train_data$Pclass == 3)
sum(train_data$Sex == "male" & train_data$Pclass == 1 & train_data$Survived == 1)/
  sum(train_data$Sex == "male" & train_data$Pclass == 1)
sum(train_data$Sex == "male" & train_data$Pclass == 2 & train_data$Survived == 1)/
  sum(train_data$Sex == "male" & train_data$Pclass == 2)
sum(train_data$Sex == "male" & train_data$Pclass == 3 & train_data$Survived == 1)/
  sum(train_data$Sex == "male" & train_data$Pclass == 3)

# using combined prediction features: predict 1 if passenger is female and first or second class
predictions_sexclass <- ifelse(test$Pclass != 3 & test$Sex == "female", 1, 0) %>% as.factor()
confusionMatrix(test$Survived, predictions_sexclass)

# comparing results
sensitivity(predictions_sex, test$Survived)
sensitivity(predictions_class, test$Survived)
sensitivity(predictions_sexclass, test$Survived)
specificity(predictions_sex, test$Survived)
specificity(predictions_class, test$Survived)
specificity(predictions_sexclass, test$Survived)
confusionMatrix(test$Survived, predictions_sexclass)$positive
BA <- function(predict, reference) {
  sens <- sensitivity(predict, reference)
  spec <- specificity(predict, reference)
  0.5*(sens + spec)
}
BA(predictions_sex, test$Survived)
BA(predictions_class, test$Survived)
BA(predictions_sexclass, test$Survived)

# F1 scores for models
F_meas(predictions_sex, test$Survived)
F_meas(predictions_class, test$Survived)
F_meas(predictions_sexclass, test$Survived)

# Using generative models: LDA
set.seed(1, sample.kind = "Rounding")
train_data_df <- data.frame(Fare = train_data$Fare, Survived = train_data$Survived)
fit_lda <- caret::train(Survived ~ Fare, data = train_data_df, method = "lda")
predictions_lda <- predict(fit_lda, test)
confusionMatrix(predictions_lda, test$Survived)$overall[["Accuracy"]]

# QDA
set.seed(1, sample.kind = "Rounding")
train_data_df <- data.frame(Fare = train_data$Fare, Survived = train_data$Survived)
fit_qda <- caret::train(Survived ~ Fare, data = train_data_df, method = "qda")
predictions_qda <- predict(fit_qda, test)
confusionMatrix(predictions_qda, test$Survived)$overall[["Accuracy"]]

# Logistic regression with one predictor
set.seed(1, sample.kind = "Rounding")
train_data_df <- data.frame(Age = train_data$Age, Survived = train_data$Survived)
fit_glm <- caret::train(Survived ~ Age, data = train_data_df, method = "glm")
predictions_glm <- predict(fit_glm, test)
confusionMatrix(predictions_glm, test$Survived)$overall[["Accuracy"]]

# Logistic regression with multiple predictors
set.seed(1, sample.kind = "Rounding")
train_data_df <- data.frame(Age = train_data$Age, Sex = train_data$Sex, 
                            Fare = train_data$Fare, Pclass = train_data$Pclass, 
                            Survived = train_data$Survived)
fit_glm2 <- caret::train(Survived ~ Sex + Pclass + Age + Fare, data = train_data_df, method = "glm")
predictions_glm2 <- predict(fit_glm2, test)
confusionMatrix(predictions_glm2, test$Survived)$overall[["Accuracy"]]

# Logistic regression with all predictors
set.seed(1, sample.kind = "Rounding")
fit_glm3 <- caret::train(Survived ~ ., data = train_data, method = "glm")
predictions_glm3 <- predict(fit_glm3, test)
confusionMatrix(predictions_glm3, test$Survived)$overall[["Accuracy"]]

# kNN
set.seed(6, sample.kind = "Rounding")
k <- seq(3, 51, 2)
fit_knn <- caret::train(Survived ~ ., data = train_data, method = "knn", tuneGrid = data.frame(k = k))
plot(fit_knn)
k[which.max(fit_knn$results$Accuracy)]
predictions_knn <- predict(fit_knn, test)
confusionMatrix(predictions_knn, test$Survived)$overall[["Accuracy"]]

# optimizing kNN training parameters with cross-validation
set.seed(8, sample.kind = "Rounding")
k <- seq(3, 51, 2)
Control <- trainControl(method = "cv", number = 10, p = .9)
fit_knn2 <- caret::train(Survived ~ ., data = train_data, method = "knn", 
                         tuneGrid = data.frame(k = k), trControl = Control)
plot(fit_knn2)
k[which.max(fit_knn2$results$Accuracy)]
predictions_knn2 <- predict(fit_knn2, test)
confusionMatrix(predictions_knn2, test$Survived)$overall[["Accuracy"]]

# classification tree
set.seed(10, sample.kind = "Rounding")
cp <- seq(0, 0.05, 0.002)
fit_rpart <- caret::train(Survived ~ ., data = train_data, method = "rpart", 
                          tuneGrid = data.frame(cp = cp))
plot(fit_rpart)
cp[which.max(fit_rpart$results$Accuracy)]
predictions_rpart <- predict(fit_rpart, test)
confusionMatrix(predictions_rpart, test$Survived)$overall[["Accuracy"]]
plot(fit_rpart$finalModel, margin = 0.1)
text(fit_rpart$finalModel, cex = 0.75)

# random forest
set.seed(14, sample.kind = "Rounding")
mtry <- seq(1, 7)
fit_rf <- caret::train(Survived ~ ., data = train_data, method = "rf", 
                          tuneGrid = data.frame(mtry = mtry), ntree = 100)
plot(fit_rf)
mtry[which.max(fit_rf$results$Accuracy)]
predictions_rf <- predict(fit_rf, test)
confusionMatrix(predictions_rf, test$Survived)$overall[["Accuracy"]]
varImp(fit_rf)