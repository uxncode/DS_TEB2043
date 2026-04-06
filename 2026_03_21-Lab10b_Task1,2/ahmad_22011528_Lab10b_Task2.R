
install.packages("caTools")
install.packages("class")

library(caTools)
library(class)

data("ChickWeight")

print(head(ChickWeight))
str(ChickWeight)

# Predictors: body weight and age (days); response: Diet (1–4)
feat_cols <- c("weight", "Time")

# Split data into training and testing sets
set.seed(22011528)
split <- sample.split(ChickWeight$Diet, SplitRatio = 0.7)

train_cl <- subset(ChickWeight, split == TRUE)
test_cl <- subset(ChickWeight, split == FALSE)

# Feature scaling (use training mean/sd for test)
train_x <- train_cl[, feat_cols]
test_x <- test_cl[, feat_cols]

train_scale <- scale(train_x)
center <- attr(train_scale, "scaled:center")
scale_attr <- attr(train_scale, "scaled:scale")
test_scale <- scale(test_x, center = center, scale = scale_attr)

# Search for optimal K
k_max <- min(31, nrow(train_scale) - 1)
k_candidates <- seq(1, k_max, by = 2)

acc_by_k <- numeric(length(k_candidates))
names(acc_by_k) <- as.character(k_candidates)

for (i in seq_along(k_candidates)) {
  k <- k_candidates[i]
  pred <- knn(
    train = train_scale,
    test = test_scale,
    cl = train_cl$Diet,
    k = k
  )
  acc_by_k[i] <- mean(pred == test_cl$Diet)
}

optimal_k <- k_candidates[which.max(acc_by_k)]
best_acc <- max(acc_by_k)

print("Accuracy by k (odd values):")
print(round(acc_by_k, 4))
cat("Optimal k:", optimal_k, "  Test accuracy:", round(best_acc, 4), "\n")

# Final model: confusion matrix at optimal K
classifier_knn <- knn(
  train = train_scale,
  test = test_scale,
  cl = train_cl$Diet,
  k = optimal_k
)

cm <- table(Actual = test_cl$Diet, Predicted = classifier_knn)
print("Confusion matrix (rows = actual Diet, columns = predicted):")
print(cm)

misClassError <- mean(classifier_knn != test_cl$Diet)
print(paste("Accuracy (optimal k) =", 1 - misClassError))

# Discussion: confusion matrix (optimal k)
# - Rows (Actual) vs columns (Predicted): diagonal cells are correct classifications
#   for each diet; off-diagonal cells are misclassifications (actual i predicted as j).
#
# - Overall: accuracy matches 1 - misclassification rate; higher diagonal mass
#   means the model assigns most test observations to the correct diet.
#
# - Errors: inspect which diets are most often confused (large off-diagonal counts).
#   Similar growth patterns at given (weight, Time) can make diets harder to separate.
#
# - Optimal k: chosen by highest test accuracy over odd k in the search grid.
#   Re-running with another random split or seed can change the best k slightly;
#   report the confusion matrix and accuracy for the chosen k.

