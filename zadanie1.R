library(tidyverse)
library(caret)
library(e1071) # Pre SVM
library(glmnet) # Pre lasso a ridge regresiu
library(rpart) #decision tree
library(rpart.plot)

# Load the dataset
matches <- read_csv("C:/Users/Lenovo/Desktop/FIIT/ING/IB 2. Semester/Objavovanie Znalostí/projekt1/OZNAL/OZNAL/results.csv")

# Inspect the first few rows of the dataset
head(matches)

#Specifications of each column
spec(matches)

# Check for missing values

missing_values <- colSums(is.na(matches))
print(missing_values)

# summary
summary(matches)


matches$date <- as.Date(matches$date)

matches <- matches %>%
  mutate(outcome = case_when(
    home_score > away_score ~ "win",
    home_score < away_score ~ "lose",
    TRUE ~ "draw"
  ))

# Summary statistics for numeric variables
summary(matches[ , c("home_score", "away_score")])

# Distribution of home and away scores
ggplot(matches, aes(x = home_score)) + geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  ggtitle("Distribution of Home Scores") + xlab("Home Score") + ylab("Frequency")

ggplot(matches, aes(x = away_score)) + geom_histogram(binwidth = 1, fill = "red", color = "black") +
  ggtitle("Distribution of Away Scores") + xlab("Away Score") + ylab("Frequency")

# Outcome distribution
ggplot(matches, aes(x = outcome)) + geom_bar(fill = "green") +
  ggtitle("Match Outcome Distribution") + xlab("Outcome") + ylab("Count")

#boxplots for outlayers
ggplot(matches, aes(y = home_score)) + geom_boxplot() + 
  ggtitle("Boxplot of Home Scores") + ylab("Home Score")

ggplot(matches, aes(y = away_score)) + geom_boxplot() + 
  ggtitle("Boxplot of Away Scores") + ylab("Away Score")



# Train a linear regression model
lm_model <- lm(home_score ~ away_score + neutral + factor(tournament), data = matches)

library(nnet)
multinom_model <- multinom(outcome ~ away_score + neutral + factor(tournament), data = matches)


# Calculate predictions and evaluate the model
# For regression
predicted_scores <- predict(lm_model, type = "response")
MAE <- mean(abs(predicted_scores - matches$home_score))
RMSE <- sqrt(mean((predicted_scores - matches$home_score)^2))

print(MAE)
print(RMSE)

# For classification
library(caret)
predicted_outcomes <- predict(multinom_model, newdata = matches, type = "class")
# Convert both predicted outcomes and actual outcomes to factors with the same levels
predicted_outcomes_factor <- factor(predicted_outcomes, levels = c("win", "lose", "draw"))
matches$outcome_factor <- factor(matches$outcome, levels = c("win", "lose", "draw"))

#Confusion matrix
confusion_matrix_result <- confusionMatrix(data = predicted_outcomes_factor, reference = matches$outcome_factor)

#Results
print(confusion_matrix_result)

#Tu zacina zadanie2

library(pROC)

# First, we get the predicted probabilities for the "win" classification
predicted_probs <- predict(multinom_model, newdata = matches, type = "prob")

# Now, we convert the actual outcomes and the predicted probabilities to a binary form for the "win" class
matches$binary_outcome <- as.numeric(matches$outcome == "win")
predicted_probs_win <- predicted_probs[,"win"]

# We create the ROC object for the "win" class
roc_curve <- roc(matches$binary_outcome, predicted_probs_win)

# We calculate the AUC for the ROC curve
auc_value <- auc(roc_curve)

# We plot the ROC curve
plot(roc_curve, main = paste("ROC Curve for 'win' class, AUC =", auc_value))

# We print the AUC value
print(auc_value)


# Compute a correlation matrix
correlation_matrix <- matches %>% 
  select(home_score, away_score, neutral) %>% # Adjust based on the numeric variables you have
  cor(use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)

# Visualize the correlation matrix
corr_plot <- ggplot(as.data.frame(as.table(correlation_matrix)), aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  labs(title = "Correlation Matrix of Numeric Variables")

# Visualize outcomes by tournament type
ggplot(matches, aes(x = tournament, fill = outcome)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion of Outcomes", x = "Tournament Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Proportion of Match Outcomes by Tournament Type")

# Display the plots
print(corr_plot)

# Rozdelenie dát na tréningovú a testovaciu sadu
set.seed(123) # Pre reprodukovateľnosť
training_index <- createDataPartition(matches$outcome, p = 0.8, list = FALSE)
training_set <- matches[training_index, ]
test_set <- matches[-training_index, ]

# Uistite sa, že premenná outcome je faktor
training_set$outcome <- factor(training_set$outcome)
test_set$outcome <- factor(test_set$outcome)

# Uistite sa, že premenná outcome je faktor
training_set$outcome <- factor(training_set$outcome)
test_set$outcome <- factor(test_set$outcome)

#Decision tree model 
tree <- rpart(outcome ~ home_score + away_score + neutral, data = training_set, method = 'class')

predictions <- predict(tree, test_set, type = 'class')

# conf matrix
confusionMatrix <- table(predictions, test_set$outcome)
print(confusionMatrix)

# Presnost
accuracy <- sum(predictions == test_set$outcome) / length(predictions)
print(accuracy)

#Graf
rpart.plot(tree)

# Cross-validácia pre Lasso model
set.seed(123)
cv_lasso <- cv.glmnet(
  as.matrix(training_set %>% select(home_score, away_score, neutral)),
  training_set$outcome,
  alpha = 1, # Alpha = 1 pre lasso
  family = "multinomial"
)

# Najlepšie lambda pre Lasso model z cross-validácie
best_lambda <- cv_lasso$lambda.min

#vizualizacia vyberu lambdy
plot(cv_lasso)

# Nastavenie modelu Lasso s vybranou hodnotou lambda
lasso_model <- glmnet(
  as.matrix(training_set %>% select(home_score, away_score, neutral)),
  training_set$outcome,
  alpha = 1, # Alpha = 1 pre lasso
  family = "multinomial",
  lambda = best_lambda
)

# Nastavenie modelu SVM
set.seed(123)
svm_model <- svm(
  outcome ~ home_score + away_score + neutral,
  data = training_set,
  type = "C-classification",
  kernel = "radial"
)

# Predikcia a vyhodnotenie modelu Lasso
lasso_pred <- predict(lasso_model, as.matrix(test_set %>% select(home_score, away_score, neutral)), s = best_lambda, type = "class")
lasso_pred_factor <- factor(lasso_pred, levels = levels(test_set$outcome))

# Kontrola rovnakého počtu prvkov
stopifnot(length(lasso_pred_factor) == length(test_set$outcome))

# Vytvorenie konfúznej matice pre model Lasso
lasso_conf_mat <- confusionMatrix(data = lasso_pred_factor, reference = test_set$outcome)

# Predikcia a vyhodnotenie modelu SVM
svm_pred <- predict(svm_model, test_set)
svm_conf_mat <- confusionMatrix(data = svm_pred, reference = test_set$outcome)

# Výstup konfúznych matíc
print(lasso_conf_mat)
print(svm_conf_mat)

# ROC analýza a AUC pre klasifikačný model (potrebný balíček pROC)
library(pROC)
svm_prob <- attr(predict(svm_model, test_set, probability = TRUE), "probabilities")
svm_roc <- roc(response = test_set$outcome, predictor = svm_prob[, "win"])
svm_auc <- auc(svm_roc)
plot(svm_roc)
print(svm_auc)



