#KOJO BOTWE AMPIAW OSEI

#1 Import the arules library
library(arules)

# Load the Groceries dataset
data("Groceries")

 #Gathering insights from the dataset
summary(Groceries)

# Display the first 10 transactions in the dataset
head(Groceries, 10)

# Find and display 4 transactions with 4 or more items
#Groceries[sapply(Groceries, length) >= 4][1:4]
# Convert the transactions to a list
transactions_list <- as(Groceries, "list")


# Find and display four transactions with four or more items
#transactions_with_four_or_more_items <- transactions_list[supply(transactions_list, length) >= 4]
transactions_with_4_or_more_items <- Groceries[sapply(Groceries, length) >= 4]
four_or_more_items_transactions <- transactions_with_4_or_more_items[1:4]
four_or_more_items_transactions


# Display the selected transactions
head(transactions_with_4_or_more_items, 4)


# Create frequency plots with various support levels
itemFrequencyPlot(Groceries, support = 0.05) 

# Create a frequency plot for the top 10 most frequent products
itemFrequencyPlot(Groceries, topN = 10)

# Use the apriori algorithm to extract association rules
rules <- apriori(Groceries, parameter = list(support = 0.01, confidence = 0.2))

# Sort and inspect the top 20 rules by confidence
rules <- sort(rules, by = "confidence", decreasing = TRUE)
inspect(rules[1:20])

# Research other rule interestingness measures
help(interestMeasure)

# 4 different rule interestingness measures
head(interestMeasure(rules, c("coverage", "kulczynski", "jaccard", "conviction"), Groceries))

#  interestingness measures to identify and analyze highly scoring rules


####################
# Taking "Groceries" as our dataset
min_support_values <- c(0.01, 0.02, 0.03)  # Define a range of support values
frequency_plots <- list()

for (min_support in min_support_values) {
  # Calculate item frequencies based on the support threshold
  item_frequency <- itemFrequency(Groceries)
  
  # Create a bar plot of the topN items (10)
  freq_plot <- barplot(sort(item_frequency, decreasing = TRUE)[1:10], 
                       main = paste("Top 10 Items (Support =", min_support, ")"),
                       xlab = "Item", ylab = "Support",
                       col = "green", horiz = TRUE)
  
  #Adjusting value of support (5)
  freq_plot <- barplot(sort(item_frequency, decreasing = TRUE)[1:5], 
                       main = paste("Top 5 Items (Support =", min_support, ")"),
                       xlab = "Item", ylab = "Support",
                       col = "blue", horiz = TRUE)
  
  frequency_plots[[as.character(min_support)]] <- freq_plot
}

# View the generated frequency plots
View(frequency_plots)



