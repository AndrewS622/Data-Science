## Classification with More Than 2 Classes and the Caret Package
# Trees and Random Forests

# create data set with 0.75X increase for an increase of 1 on x (on average)
library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1, sample.kind = "Rounding")
x <- rnorm(n, 0, 1)
y <- 0.75*x + rnorm(n, 0, sigma)
dat <- data.frame(x, y)

# fit a classification tree and plot
fit <- rpart(y ~ ., data = dat)
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

# make scatter plot of y vs. x with predicted values
dat %>%
  mutate(y_hat = predict(fit)) %>%
  ggplot() + 
  geom_point(aes(x,y)) + 
  geom_step(aes(x, y_hat), col = 2, size = 1)

# use random forest algorithm 
library("randomForest")
fit <- randomForest(y ~ x, data = dat)

# plot results
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

# check for convergence
plot(fit)

# change parameters
library("randomForest")
fit <- randomForest(y ~ x, data = dat, nodesize = 50,maxnodes = 25 )
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

## Caret Package
library(rpart)
library(caret)
library(dslabs)
data("tissue_gene_expression")

# train classification tree
set.seed(1991, sample.kind = "Rounding")
params = data.frame(cp=seq(0, 0.1, 0.01))
tissues <- data.frame(tissue_gene_expression$x, y = tissue_gene_expression$y)
fit <- caret::train(y ~ ., data = tissues, method = "rpart", tuneGrid = params)

# plot results
plot(fit$results$cp, fit$results$Accuracy)

# repeat allowing splitting of any node
set.seed(1991, sample.kind = "Rounding")
fit_rpart <- caret::train(y ~ ., data = tissues, method = "rpart", tuneGrid = params, control = rpart.control(minsplit = 0))
plot(fit_rpart$results$cp, fit_rpart$results$Accuracy)
max(fit_rpart$results$Accuracy)

# plot tree results
plot(fit_rpart$finalModel)
text(fit_rpart$finalModel, cex = 0.75)

# use random forest
params <- data.frame(mtry = seq(50, 200, 25))
set.seed(1991, sample.kind = "Rounding")
fit <- caret::train(y ~ ., data = tissues, method = "rf", tuneGrid = params, nodesize = 1)
plot(fit$results$mtry, fit$results$Accuracy)
fit$results$mtry[which.max(fit$results$Accuracy)]

# variable importance
imp <- varImp(fit)
imp

# extract predictors from rpart
tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms
