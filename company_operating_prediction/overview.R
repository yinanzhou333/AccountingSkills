library(tidyverse)
library(tidymodels)
library(psych)
library(ROSE)
library("corrplot")
library(stringdist)
library("rpart")
library("caret") #Classification And REgression Training
library("ggplot2")
library("corrplot")
library("rpart.plot")

company <- read.csv(file = './data.csv' , header = T)

glimpse(company)

company %>% select(where(is.integer)) %>% colnames()
cat ('number of companies in the dataset:', length(company$Bankrupt.),'\n')
cat ('number of bankrupt companies:', sum(company$Bankrupt.),'\n')

# Extract metric names
metric_names <- (names(company)[2:(ncol(company)-1)])

# Function to find common words
find_common_words <- function(metric_names) {
  # Split metric names into words
  all_words <- unlist(strsplit(tolower(metric_names), "[^A-Za-z0-9]+"))
  # Count occurrences of each word
  word_counts <- table(all_words)
  # Extract common words (adjust threshold as needed)
  common_words <- names(word_counts[word_counts > 1 & nchar(names(word_counts)) > 3])
  return(common_words)
}
# Find common words
common_words <- find_common_words(metric_names)

# Function to group metric names by common words
group_metrics_by_common_words <- function(metric_names, common_words) {
  grouped_metrics <- lapply(common_words, function(word) {
    subset <- grep(word, metric_names, ignore.case = TRUE)
    metric_group <- paste(metric_names[subset], collapse = ", ")
    return(list(common_word = word, metrics = metric_group))
  })
  return(grouped_metrics)
}
# Group metric names by common words
grouped_metrics <- group_metrics_by_common_words(metric_names, common_words)

# Print the group of each common word
for (i in seq_along(grouped_metrics)) {
  cat("Group", i, ": Common Word -", grouped_metrics[[i]]$common_word, "\n")
  cat("Metrics:", grouped_metrics[[i]]$metrics, "\n\n")
}


# Calculate correlations with "Bankrupt"
correlations <- sapply(metric_names, function(metric) {
  cor(company[[metric]], company$Bankrupt.)
})
# Combine metric names and correlations
correlation_data <- data.frame(Metric = metric_names, Correlation = correlations)
# Sort by absolute correlation values
sorted_correlation_data <- correlation_data[order(abs(correlation_data$Correlation), decreasing = TRUE), ]
# Print the sorted results
print(sorted_correlation_data)


model <-rpart(Bankrupt.~., company, method="class", control = rpart.control(cp = 0.01, minsplit = 50, minbucket = 50))
#model <-rpart(Quality~., train_data, method="class")
printcp(model)
rpart.plot(model)
company_predictions <- predict(model, newdata = company, type = "class")
train_accuracy <- sum(company_predictions == company$Bankrupt. )/ length(company$Bankrupt.)
cat("Training Accuracy:", train_accuracy, "\n")
