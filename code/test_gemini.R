# Load the ggplot2 library, which contains the diamonds dataset and plotting functions.
# If you don't have it installed, run: install.packages("ggplot2")
library(ggplot2)

# Create the plot
# We initialize the plot with the 'diamonds' dataset.
# We map 'carat' to the x-axis and 'price' to the y-axis.
ggplot(diamonds, aes(x = carat, y = price)) +

  # Add a smoothed conditional mean line to each facet.
  # The 'se = FALSE' argument hides the confidence interval ribbon for a cleaner look.
  geom_smooth(se = FALSE, color = "steelblue") +

  # Create a separate plot for each category in the 'cut' variable.
  # The '~' formula indicates that we are faceting by the 'cut' column.
  # 'ncol = 3' arranges the facets into 3 columns.
  facet_wrap(~ cut, ncol = 3) +

  # Add informative labels and a title to the plot for clarity.
  labs(
    title = "Relationship between Carat and Price, by Cut Quality",
    subtitle = "A smoothed line shows the general trend for each category",
    x = "Carat",
    y = "Price (in USD)"
  ) +

  # Apply a clean, minimal theme to the plot.
  theme_minimal() +

  # Customize theme elements for better readability.
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold") # Make facet labels bold
  )

