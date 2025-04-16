library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)
# Load and prepare data
df <- read_excel("Input_gleam.xlsx", sheet = "Population")
df <- df %>% select(c(1:12))

# Convert Year to numeric and filter for relevant years
df$Year <- as.numeric(df$Year)
df <- df %>% filter(Year %in% c(2010:2022))

# Define future years (for predictions up to 2035)
future_years <- data.frame(Year = seq(2010, 2035))

# Define the prediction function
ppredict_future <- function(df, future_years) {
  if (!"Item" %in% names(future_years)) {
    future_years <- expand.grid(Year = future_years$Year, Item = unique(df$Item))
  }
  
  split_df <- split(df, df$Item)
  results <- lapply(names(split_df), function(item) {
    item_data <- split_df[[item]]
    model <- lm(Value ~ Year, data = item_data)
    item_future_years <- subset(future_years, Item == item)
    future_values <- predict(model, newdata = item_future_years)
    data.frame(Year = item_future_years$Year, Value = future_values, Item = item)
  })
  
  do.call(rbind, results)
}

# Run prediction function to generate predictions for future years
predicted_data <- ppredict_future(df, future_years)

# Calculate the regression formulas and yearly percentage increase for each Item
formulas <- lapply(split(df, df$Item), function(item_data) {
  model <- lm(Value ~ Year, data = item_data)
  intercept <- coef(model)[1]
  slope <- coef(model)[2]
  
  # Calculate initial value at Year 2010 (using intercept + slope * 2010)
  initial_value_2010 <- intercept + slope * 2010
  
  # Calculate yearly percentage increase based on slope
  yearly_increase <- (slope / initial_value_2010) * 100
  
  # Create a formula string
  formula <- paste0("y = ", round(slope, 2), "x + ", round(intercept, 2))
  
  data.frame(
    Item = unique(item_data$Item),
    Formula = formula,
    Yearly_Percentage_Increase = round(yearly_increase, 2)
  )
})

# Combine formulas and increases into a single data frame
formula_table <- do.call(rbind, formulas)

# Plot with formulas
ggplot() +
  # Original data with solid lines
  geom_line(data = df, aes(x = Year, y = Value/1000, group = Item, color = Item), size = 1.5) +
  # Predicted data with dashed lines
  geom_line(data = predicted_data, aes(x = Year, y = Value/1000, group = Item, color = Item), linetype = "dashed", size = 1) +
  # Labels at the end of each line
  geom_text_repel(data = df %>% group_by(Item) %>% slice_tail(n = 1),  
                  aes(x = Year, y = Value/1000, label = Item, color = Item), 
                  hjust = 0, 
                  nudge_x = 0, 
                  direction = "y", 
                  segment.color = NA) +
  # Add regression formulas as text annotations
  geom_text(data = formula_table, aes(x = 2035, y = max(df$Value) / 1000, 
                                      label = Formula, color = Item), 
            hjust = 1.2, vjust = -1) +
  # Vertical line to indicate extrapolation start
  geom_vline(xintercept = 2022, linetype = "dotted", color = "gray", size = 1) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 10)
  ) +
  labs(y = "mil tCO2-eq", x = "") +
  scale_color_brewer(palette = "Set1") +
  ggtitle("Livestock population (2020-2035)")
