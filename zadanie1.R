library(tidyverse)

# Load the dataset
matches <- read_csv("results.csv")

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
