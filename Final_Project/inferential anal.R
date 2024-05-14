# Load necessary libraries
library(ggplot2)

# Load data from a CSV file
profits <- read.csv("https://raw.githubusercontent.com/maxdokukin/Politician-Trades/main/Data/data/trades.csv")
str(profits)

# Check if the columns exist and fix column names if necessary
names(profits)

# Calculate the Pearson correlation coefficient
correlation <- cor(profits$Report.Gap, profits$Annualized.Percentage.Profit, use = "complete.obs")

# Create a ggplot object with a scatter plot
p <- ggplot(profits, aes(x = Report.Gap, y = Annualized.Percentage.Profit)) +
  geom_point(alpha = 0.5, size = 2) +  # Add points with some transparency and size adjustment
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add a linear regression line without a confidence interval
  labs(title = "Reporting Gap vs Annualized Percentage Profit",
       x = "Reporting Gap",
       y = "Annualized Percentage Profit") +
  annotate("text", x = max(profits$Reporting.Gap, na.rm = TRUE), y = min(profits$Annualized.Percentage.Profit, na.rm = TRUE), label = sprintf("Correlation: %.2f", correlation), hjust = 1.5, vjust = -0.5, size = 5, color = "blue")  # Add the correlation coefficient as text

# Print the plot
print(p)
