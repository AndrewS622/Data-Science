## Distance
library(dslabs)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

# Euclidean distance between each observation
d <- dist(tissue_gene_expression$x)

# example within-tissue distances
as.matrix(d)[1,2]
as.matrix(d)[39,40]
as.matrix(d)[73,74]

# example out-of-tissue distances
as.matrix(d)[1,39]
as.matrix(d)[1,40]
as.matrix(d)[2,39]
as.matrix(d)[2,40]

as.matrix(d)[1,73]
as.matrix(d)[1,74]
as.matrix(d)[2,73]
as.matrix(d)[2,74]

as.matrix(d)[73,39]
as.matrix(d)[74,39]
as.matrix(d)[73,40]
as.matrix(d)[74,40]

# plot distances
image(as.matrix(d))

# nearest neighbors
library(caret)
set.seed(1, sample.kind = "Rounding")

# partition heights data set
idx <- createDataPartition(heights$sex, list = FALSE)

# train knn over different k values
ks <- seq(1, 101, 3)
train <- heights[-idx,]
test <- heights[idx,]
models <- sapply(ks, function(k) {
  model <- knn3(sex ~ height, data = train, k = k)
  y_hat_knn <- predict(model, test, type = "class")
  F_meas(test$sex, y_hat_knn)
})
ggplot(mapping = aes(ks, models)) + geom_point()
max(models)
ks[which.max(models)]

# gene expression
library(dslabs)
data("tissue_gene_expression")
set.seed(1)
idx <- createDataPartition(tissue_gene_expression$y, times = 1, p = 0.5, list = FALSE)
xtest <- tissue_gene_expression$x[idx,] %>% data.frame()
xtrain <- tissue_gene_expression$x[-idx,] %>% data.frame()
ytest <- tissue_gene_expression$y[idx] %>% data.frame()
ytrain <- tissue_gene_expression$y[-idx] %>% data.frame()

ks <- ks <- seq(1, 11, 2)
models <- sapply(ks, function(k) {
  model <- knn3(ytrain$. ~ ., data = xtrain, k = k)
  y_hat_knn <- predict(model, xtest, type = "class")
  confusionMatrix(data = y_hat_knn, reference = ytest$.)$overall["Accuracy"]
})
ggplot(mapping = aes(ks, models)) + geom_point()
max(models)
ks[which.max(models)]

## Cross-validation
library(tidyverse)
library(caret)

# generate set of random predictors and outcomes
set.seed(1996, sample.kind="Rounding") 
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

# use random sample of predictors
x_subset <- x[ ,sample(p, 100)]

# perform cross-validation
fit <- train(x_subset, y, method = "glm")
fit$results

# search for most predictive features by grouping and comparing
library(genefilter)
tt <- colttests(x, y)
pvals <- tt$p.value
ind <- which(pvals < 0.01)
length(ind)

# re-run cross-validation
x_subset <- x[,ind]
fit <- train(x_subset, y, method = "glm")
fit$results

# fit using knn instead of glm
fit <- train(x_subset, y, method="knn", tuneGrid = data.frame(k = seq(101,301,25)))
ggplot(fit)

# high accuracy due to using entire model in training
# redo cross-validation with selection
inds <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
tt <- colttests(x[inds,], y[inds])
pvals <- tt$p.value
ind <- which(pvals < 0.01)
x_subset <- x[inds,ind]
y_subset <- y[inds]
fit <- train(x_subset, y_subset, method = "glm")
fit$results
predictions <- predict(fit, newdata = x[-inds,ind])
table(true = y[-inds], model = predictions)
mean(y[-inds] == predictions)
fit <- train(x_subset, y_subset, method = "knn", tuneGrid = data.frame(k = seq(101,301,25)))
predictions <- predict(fit, newdata = x[-inds,ind])
table(true = y[-inds], model = predictions)
mean(y[-inds] == predictions)

# different data set
library(dslabs)
data("tissue_gene_expression")
fit <- train(tissue_gene_expression$x, tissue_gene_expression$y, method = "knn", tuneGrid = data.frame(k = seq(1,7,2)))
ggplot(fit)

## Bootstrapping
library(dslabs)
library(caret)
data(mnist_27)
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(mnist_27$train$y, 10)

# checking for independence of sampling
no3 <- c()
no4 <- c()
no7 <- c()
for (i in 1:10) {
  str = ifelse(i < 10, paste0("Resample0", i), "Resample10")
  no3 <- append(no3, sum(indexes[[str]] == 3))
  no4 <- append(no4, sum(indexes[[str]] == 4))
  no7 <- append(no7, sum(indexes[[str]] == 7))
}
sum(no3)

# MC simulation
set.seed(1, sample.kind = "Rounding")
B <- 10000
p75s <- replicate(B, {
  y <- rnorm(100,0,1)
  quantile(y, 0.75)
})
mean(p75s)
sd(p75s)

# use bootstrap instead
set.seed(1, sample.kind = "Rounding")
y <- rnorm(100,0,1)
set.seed(1, sample.kind = "Rounding")
indexes <- createResample(y, 10)
p75s <- c()
for (i in 1:10) {
  str = ifelse(i < 10, paste0("Resample0", i), "Resample10")
  p75s <- append(p75s, quantile(y[indexes[[str]]], 0.75))
}
mean(p75s)
sd(p75s)

# repeat with 10000
set.seed(1, sample.kind = "Rounding")
indexes <- createResample(y, 10000)
p75s <- c()
for (i in 1:10000) {
  str = sprintf("Resample%05d", i)
  p75s <- append(p75s, quantile(y[indexes[[str]]], 0.75))
}
mean(p75s)
sd(p75s)

## Generative Models
library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

# train linear discriminant analysis algorithm
fit <- train(x, y, method = "lda")
fit$results
fit$finalModel

# plot results and find driving factors (largest means)
plot(fit$finalModel$means[1,], fit$finalModel$means[2,])
rank(-fit$finalModel$means[1,])

# repeat with quadratic discriminant analysis
fit <- train(x, y, method = "qda")
fit$results
plot(fit$finalModel$means[1,], fit$finalModel$means[2,])
rank(-fit$finalModel$means[1,])

# differences are more important than means in predictive power- preprocess
fit <- train(x, y, method = "lda", preProcess = "center")
fit$results
plot(fit$finalModel$means[1,], fit$finalModel$means[2,])
rank(-(abs(fit$finalModel$means[1,] - fit$finalModel$means[2,])))

# repeat with all tissue types
data("tissue_gene_expression")
set.seed(1993, sample.kind="Rounding")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

fit <- train(x, y, method = "lda", preProcess = "center")
fit$results
plot(fit$finalModel$means[1,], fit$finalModel$means[2,])
rank(-(abs(fit$finalModel$means[1,] - fit$finalModel$means[2,])))
