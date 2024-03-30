# Load the Tidyverse, a collection of R packages for data science
library(tidyverse)

# Load the dataset
matches <- read_csv("results.csv")

# Inspect the first few rows of the dataset
head(matches)

#Specifications of each column
spec(matches)

# Check for missing values
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
