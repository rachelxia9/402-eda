library(data.table)
library(irr)
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
library(dplyr)
library(tidyr)

data <- "/Users/rachelxia/CNS/EDA/testdata/Data/Final_Results-Master.csv"


# inter-rater: two way mixed effects - single rater - absolute agreement
# > 0.9 = excellent reliability
# test-retest: consistency

data <- read.csv(data)

# Extract the base ID (without _1 or _2)
data$BaseID <- sub("_[12]", "", data$ID)

# Split the data into two separate data frames for _1 and _2
data_1 <- data[grep("_1", data$ID), ]
data_2 <- data[grep("_2", data$ID), ]

# Merge the data frames by BaseID
merged_data <- merge(data_1, data_2, by = "BaseID", suffixes = c("_1", "_2"))

ratings <- cbind(merged_data$Percent_change_1, merged_data$Percent_change_2)

# Calculate ICC (3,1) using the irr package
icc_result <- icc(ratings, model = "twoway", type = "agreement", unit = "single")

# Print the result
print(icc_result)

# run Pearson's R 
correlation <- cor(merged_data$Percent_change_1, merged_data$Percent_change_2, method = 'pearson')
print(correlation)


# Perform a paired t-test
t_test_result <- t.test(merged_data$Total.Removed_1, merged_data$Total.Removed_2, paired = TRUE)

# Print the result
print(t_test_result)

# Reshape data to long format
data_long <- merged_data %>%
  pivot_longer(cols = starts_with("Total.Removed"), 
               names_to = "Condition", 
               values_to = "Value")
# Create paired plot
paired_plot <- ggpaired(
  data_long,
  x = "Condition",       # Column indicating paired conditions
  y = "Value",           # Column indicating values to be plotted
  color = "black",       # Color of points and lines
  fill = "white",        # Fill color for bars (if applicable)
  palette = NULL,        # Optional: Define custom colors
  width = 0.5,           # Width of bars or points
  point.size = 1.2,      # Size of points (if applicable)
  line.size = 0.5,       # Size of connecting lines
  line.color = "black",  # Color of connecting lines
  linetype = "solid",    # Type of connecting lines
  title = "Paired Plot of Total Removed Trials", # Title of the plot
  xlab = "Session",    # Label for x-axis
  ylab = "# of Trials Removed",        # Label for y-axis
  facet.by = NULL,       # Facet by another variable if needed
  panel.labs = NULL,     # Custom panel labels (if faceting)
  short.panel.labs = TRUE, # Use short labels if faceting
  label = NULL,          # Add labels to points (if needed)
  font.label = list(size = 11, color = "black"), # Font size and color for labels
  label.select = NULL,   # Select labels to display (if applicable)
  repel = FALSE,         # Use ggrepel to avoid overlapping labels
  label.rectangle = FALSE, # Draw rectangle around labels
  ggtheme = theme_pubr() # Theme for the plot
)

# Print the plot
print(paired_plot)

# Save the plot to a file
ggsave("paired_plot.png", plot = paired_plot, width = 8, height = 6)


# Create a scatter plot with ggplot2
plot <- ggplot(merged_data, aes(x = Percent_change_1, y = Percent_change_2)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relationship of Percent Change between Test-Retest Sessions",
       x = "Percent Change from First Session",
       y = "Percent Change from Second Session") +
  annotate("text", x = min(merged_data$Percent_change_1),
           y = max(merged_data$Percent_change_2),
           label = paste("r =", round(correlation, 2)),
           hjust = 0, color = "black")

# Print the plot
print(plot)

# Save the plot to a file
ggsave("correlation_plot.png", plot = plot, width = 8, height = 6)


# Specify the file path to save the CSV file
output_file <- "/Users/rachelxia/CNS/EDA/testdata/Data/test_retest.csv"

# Write ICC results to CSV file
fwrite(icc_result, file = output_file)




