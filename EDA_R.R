# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(ggplot2)    # For advanced plotting
library(patchwork)  # For arranging plots

# Load the dataset (assuming it's a CSV file)
house_data <- read.csv("C:/Users/user/Downloads/newhouse.csv", stringsAsFactors = FALSE)  # Adjust the file path accordingly

# Convert total_sqft to numeric (remove non-numeric values)
house_data$total_sqft <- as.numeric(gsub("[^0-9.]", "", house_data$total_sqft))

# Data Cleaning: Remove missing values
house_data <- na.omit(house_data)

# Summary statistics of numerical variḁbles
summary_stats <- summary(house_data)
̥
# Summary of categorical variables̥
cat_summary <- list(
  Area_Type = table(house_data$area_type),
  Availability = table(house_data$availability),
  Location = table(house_data$location),
  Size = table(house_data$size),
  Society = table(house_data$society)
)

# Scatter plot: Price vs. Total Square Feet
scatter_plot <- ggplot(house_data, aes(x = total_sqft, y = price)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Price vs. Total Square Feet",
       x = "Total Square Feet",
       y = "Price")

# Box plot: Price distribution by Area Type
box_plot <- ggplot(house_data, aes(x = area_type, y = price, fill = area_type)) +
  geom_boxplot() +
  labs(title = "Price Distribution by Area Type",
       x = "Area Type",
       y = "Price",
       fill = "Area Type")

# Histogram: Distribution of Prices
histogram_plot <- ggplot(house_data, aes(x = price)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Prices",
       x = "Price",
       y = "Frequency") +
  theme_minimal()

# Bar plot: Availability of Houses
bar_plot <- ggplot(house_data, aes(x = availability, fill = availability)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Availability of Houses",
       x = "Availability",
       y = "Count") +
  scale_fill_brewer(palette = "Set3")

# Correlation Matrix
correlation_matrix <- cor(house_data[, c("total_sqft", "bath", "balcony", "price")])
corrplot_plot <- corrplot::corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", title = "Correlation Matrix")


average_price_location_plot <- ggplot(average_price_location, aes(x = reorder(location, -average_price), y = average_price)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Top 10 Locations by Average Price",
       x = "Location",
       y = "Average Price") +
  theme_minimal()

# Arrange all plots using patchwork
plots <- scatter_plot + box_plot + histogram_plot + bar_plot + corrplot_plot + size_distribution_plot + average_price_location_plot
plots <- plots + plot_layout(ncol = 2)
plots
