## Ensembles
library(caret)
library(dslabs)
set.seed(1, sample.kind = "Rounding") 
data("mnist_27")

# train different models on MNIST data for 2 and 7
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")
fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 
names(fits) <- models

# predict outcome with each model
predictions <- sapply(models, function(model){
  predict(fits[model], mnist_27$test)[[1]]
})
dim(predictions)
colnames(predictions) <- models

# compute model accuracies
accuracy <- sapply(models, function(model){
  confusionMatrix(as.factor(predictions[,model]), mnist_27$test$y)$overall[["Accuracy"]]
})
accuracy
mean(accuracy)

# create ensemble model with majority voting
predict_sum <- rowSums(predictions == 7)
ensemble <- ifelse(predict_sum > 5, 7, 2) %>% as.factor()
ensemble_acc <- confusionMatrix(ensemble, mnist_27$test$y)$overall[["Accuracy"]]
ensemble_acc

# compare ensemble to individual models
sum(accuracy > ensemble_acc)
accuracy > ensemble_acc

# use minimum accuracy for each method as estimate for performance
acc_min <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_min)

# repeat ensemble only with models that have min accuracy > 0.8
which_models <- acc_min >= 0.8
num_acc <- sum(which_models)
predict_sum_subset <- rowSums(predictions[,which_models] == 7)
ensemble_subset <- ifelse(predict_sum_subset > num_acc/2, 7, 2) %>% as.factor()
ensemble_subset_acc <- confusionMatrix(ensemble_subset, mnist_27$test$y)$overall[["Accuracy"]]
ensemble_subset_acc

## Recommendation Systems
library(tidyverse)
library(ggplot2)
library(lubridate)
library(dslabs)
data("movielens")

# plot number of ratings for movie vs. year movie came out
movielens %>%
  group_by(movieId) %>%
  summarize(year = year, num_rat = n()) %>%
  distinct() %>%
  ggplot(aes(year, num_rat)) + 
  geom_point() + 
  scale_y_continuous("sqrt")

# find year with highest median number of ratings
rats_per_year <- movielens %>%
  group_by(movieId) %>%
  summarize(year = year, num_rat = n()) %>% 
  distinct() %>%
  ungroup() %>%
  group_by(year) %>%
  summarize(med = median(num_rat)) %>%
  ungroup() 
rats_per_year[which.max(rats_per_year$med),]
rats_per_year %>% ggplot(aes(year, med)) + 
  geom_point()

# select top 25 movies with highest avg number of ratings/yr
movielens %>%
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(year = year, title = title, num_rat = n(), rat_per_year = num_rat/(2018-year), rat = mean(rating)) %>%
  distinct() %>%
  ungroup() %>%
  arrange(movieId, desc(rat_per_year)) %>%
  top_n(10, rat_per_year)

# plot trend
movielens %>%
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(year = year, title = title, num_rat = n(), rat_per_year = num_rat/(2018-year), rat = mean(rating)) %>%
  distinct() %>%
  ungroup() %>%
  group_by(round(rat_per_year)) %>%
  summarize(rat_per_year, m_rat = mean(rat)) %>%
  ggplot(aes(rat_per_year, m_rat)) + 
  geom_point() + 
  geom_smooth()

# create date column 
movielens <- mutate(movielens, date = as_datetime(timestamp))

# compute average rating for each week and plot against date
movielens %>%
  mutate(date = round_date(dat, "week")) %>%
  group_by(date) %>%
  summarize(date, rat = mean(rating)) %>%
  ungroup() %>%
  distinct() %>%
  ggplot(aes(date, rat)) + 
  geom_point() + 
  geom_smooth()

# group by genres
movielens %>%
  group_by(genres) %>%
  summarize(genres, n = n(), rat = mean(rating), sdev = sd(rating)) %>%
  ungroup() %>%
  distinct() %>%
  filter(n > 1000) %>%
  ggplot(aes(genres, rat)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = rat-sdev, ymax = rat + sdev)) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

## Regularization
# simulate students in 1000 schools
library(tidyverse)
library(ggplot2)
set.seed(1986, sample.kind="Rounding")
n <- round(2^rnorm(1000, 8, 1))

# set true school quality independent of size
set.seed(1, sample.kind="Rounding")
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

# view top 10
schools %>% top_n(10, quality) %>% arrange(desc(quality))

# simulate test scores normality distributed around quality
set.seed(1, sample.kind="Rounding")
mu <- round(80 + 2*rt(1000, 5))
scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean), sdev = sapply(scores, sd))

# top schools by average score
schools %>% top_n(10, score) %>% arrange(desc(score))

# compare school sizes
median(schools$size)
schools %>% top_n(10, score) %>% arrange(desc(score)) %>% 
  summarize(median(size))
schools %>% top_n(-10, score) %>% arrange(score) %>% 
  summarize(median(size))

# plot relationship, highlighting true top ten
true10 <- schools %>% top_n(10, quality) %>% arrange(desc(quality)) %>%
  pull(id)
schools %>% ggplot(aes(size, sdev, fill = id %in% true10)) + 
  geom_point(size = 2, shape = 21)
# standard error larger when school small, so best and worst likely to be in this region

# regularization in picking best schools
overall <- mean(sapply(scores, mean))
alpha <- 25 # regularization parameter
score_reg <- sapply(scores, function(score) {
  reg = overall + sum(score - overall)/(length(score) + alpha)
})
schools_reg <- mutate(schools, reg = score_reg)
schools_reg %>% top_n(10, reg) %>% arrange(desc(reg))

# maximize RMSE from different alphas
alphas <- seq(10, 250)
RMSE <- function(qualities, estimates) {
  sqrt((1/length(qualities))*sum((qualities - estimates)^2))
}
RMSEs <- sapply(alphas, function(alpha) {
  qualities <- schools$quality
  estimates <- sapply(scores, function(score) {
    overall + sum(score - overall)/(length(score) + alpha)
  })
  RMSE(qualities, estimates)
})
plot(alphas, RMSEs)
alphas[which.min(RMSEs)]

# use best alpha from above
alpha <- alphas[which.min(RMSEs)]
score_reg <- sapply(scores, function(score) {
  reg = overall + sum(score - overall)/(length(score) + alpha)
})
schools_reg <- mutate(schools, reg = score_reg)
schools_reg %>% top_n(10, reg) %>% arrange(desc(reg))

# if overall mean is not subtracted
RMSEs <- sapply(alphas, function(alpha) {
  qualities <- schools$quality
  estimates <- sapply(scores, function(score) {
    sum(score)/(length(score) + alpha)
  })
  RMSE(qualities, estimates)
})
plot(alphas, RMSEs)
alphas[which.min(RMSEs)]

# Matrix Factorization and SVD
# grade scores for 100 students in 24 subjects above/below the average
set.seed(1987, sample.kind="Rounding")
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

# visualize test scores 
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

# correlation between scores
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# compute SVD
s <- svd(y)
names(s)
y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

# sum of squares of columns
ss_y <- colSums(y^2)
ss_yv <- colSums((y %*% s$v)^2)
sum(ss_y)
sum(ss_yv)

# plot above results
plot(ss_y)
plot(ss_yv)

# due to orthogonality of U, we can see:
plot(sqrt(ss_yv), s$d)
abline(0,1)

# percent variability explained by first three columns of YV
sum(ss_yv[1:3])/sum(ss_yv)
sum(s$d[1:3]^2)/sum(s$d^2)

# can compute UD without forming diagonal matrix using sweep
identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))
UD <- sweep(s$u, 2, s$d, FUN = "*")

# average score for each student across all subjects
student_avg <- rowMeans(y)
plot(student_avg, UD[,1])
abline(0,-5)

# suggests multiplying Y by V is similar to taking student's average
my_image(s$v)

# exploring the SVD
U <- s$u
V <- s$v
plot(U[,1], ylim = c(-0.3,0.3))
plot(V[,1], ylim = c(-0.3,0.3))
my_image(y)
my_image(s$d[1] * U[,1, drop=FALSE] %*% t(V[,1, drop=FALSE]))

# good students tend to be good in all subjects, but still missing within-subject similarities
# look at residuals
resid <- y - with(s,(u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# repeat with second column
plot(U[,2], ylim = c(-0.3,0.3))
plot(V[,2], ylim = c(-0.3,0.3))
my_image(resid)
my_image(s$d[2] * U[,2, drop=FALSE] %*% t(V[,2, drop=FALSE]))
# contains differences in ability between math/science and arts

# compute new residuals
resid <- y - with(s,sweep(u[, 1:2], 2, d[1:2], FUN="*") %*% t(v[, 1:2]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# repeat for third column
plot(U[,3], ylim = c(-0.3,0.3))
plot(V[,3], ylim = c(-0.3,0.3))
my_image(resid)
my_image(s$d[3] * U[,3, drop=FALSE] %*% t(V[,2, drop=FALSE]))
# differences in math and science

# compute new residuals
resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# structure is essentially gone with 98% of variability explained
z = range(y)
my_image(y, zlim = z)
my_image(y - resid, zlim = z)
my_image(resid, zlim = z)

# Dimension Reduction
library(dslabs)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)

# plot first principal components
pca <- prcomp(tissue_gene_expression$x)
dim(pca$x)
plot(pca$x[,1], pca$x[,2], col = tissue_gene_expression$y)
text(pca$x[,1], pca$x[,2], tissue_gene_expression$y)

# average across all predictors for each observation
obs_avg <- rowSums(tissue_gene_expression$x)
plot(obs_avg, pca$x[,1], col = tissue_gene_expression$y)
cor(obs_avg, pca$x[,1])

# remove center to prevent this having an effect on the PCA
x_0 <- tissue_gene_expression$x - rowMeans(tissue_gene_expression$x)
pc <- prcomp(x_0)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2],
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) + 
  geom_point()

# look at first 10 PCs for each tissue
for (i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

# variance explained
summ <- summary(pc)
plot(summ$importance[2,])
plot(summ$importance[3,])
sum(summ$importance[3,] <= 0.5)

# Clustering
library(dslabs)
data("tissue_gene_expression")
tgex <- tissue_gene_expression$x - rowMeans(tissue_gene_expression$x)
d <- dist(tgex)

# hierarchical clustering
plot(hclust(d))

# select most variable genes
library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)
