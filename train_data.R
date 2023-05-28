install.packages("e1071")
library("e1071")

data(iris)
df = iris


set.seed(12345)
train_indices <- sample(1:nrow(df), 0.7 * nrow(df))
train_set <- df[train_indices, ]
test_set <- df[-train_indices, ]

svm_model = svm(Species ~ ., data = train_set, kernel = "linear")

predictions <- predict(svm_model, test_set)

confusion_matrix <- table(predictions, test_set$Species)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(confusion_matrix)
cat("Accuracy:", accuracy)