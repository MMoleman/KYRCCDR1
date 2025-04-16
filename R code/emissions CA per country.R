#total emissions gleam default values:
library(dplyr)       # For data manipulation
library(readxl)      # For reading Excel files
library(ggplot2)     # For data visualization
library(ggpubr)      # For arranging multiple ggplot objects (provides ggarrange)
library(tidyr)       # For tidying data
library(readr)
library(cowplot)
library(ggpubr)

FAOSTAT <- read_excel("C:/Rfiles/WB/CCDRKYR/KYRCCDR1/FAOSTAT_data_en_4-5-2025.xls") %>%
  mutate(Value = case_when(
    grepl("CH4", Element) ~ Value * 27,
    grepl("N2O", Element) ~ Value * 273,
    TRUE ~ Value
  ))%>%
  filter(Element %in% c('Enteric fermentation (Emissions CH4)',
                        'Manure management (Emissions CH4)',
                        'Manure management (Emissions N2O)',
                        'Manure left on pasture (Emissions N2O)'))%>%
  select(Area, Item, Value, Element, Year) %>%
  group_by(Area, Year) %>%
  summarise(Value = sum(Value, na.rm=TRUE))%>%
  filter(Year %in% c(2000:2022))  # Include 2022 by interpolation if needed

# Define the future years, including 2022 if missing
future_years <- data.frame(Year = seq(2022, 2035))

# Prediction function as before
predict_future <- function(df, future_years) {
  model <- lm(Value ~ Year, data = df)  # Fit a linear model
  future_values <- predict(model, newdata = future_years)
  data.frame(Year = future_years$Year, Value = future_values, Area = unique(df$Area))
}

# Apply prediction function
future_data <- df2 %>% 
  group_by(Area) %>% 
  do(predict_future(., future_years)) %>% 
  ungroup()

# Combine original and extrapolated data
df_combined <- bind_rows(df2, future_data)

# Split the data into original and extrapolated portions
original_data <- df_combined %>% filter(Year <= 2022)
predicted_data <- df_combined %>% filter(Year >= 2022)

# Plot with separate line types for original and predicted data
ggplot() +
  # Original data with solid lines
  geom_line(data = original_data, aes(x = Year, y = Value/1000, group = Area, color = Area), size = 1) +
  # Predicted data with dashed lines
  geom_line(data = predicted_data, aes(x = Year, y = Value/1000, group = Area, color = Area), 
            size = 1, linetype = "dashed") +
  # Labels at the end of each line
  geom_text_repel(data = df_combined %>% group_by(Area) %>% slice_tail(n = 1),  
                  aes(x = Year, y = Value/1000, label = Area, color = Area), 
                  hjust = 0, 
                  nudge_x = 2, 
                  direction = "y", 
                  segment.color = NA) +
  # Vertical line to indicate extrapolation start
  geom_vline(xintercept = 2022, linetype = "dotted", color = "gray") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 10)
  ) +
  labs(y = "mil tCO2-eq", x = "") +
  scale_color_brewer(palette = "Set1") +
  ggtitle("Direct emissions livestock Central Asia (2000-2035)")

df1 <- df %>%
  group_by(Year, Area) %>%
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  filter(Year == "2000" | Year == "2021")%>%
  pivot_wider(names_from = Year, values_from = c(Value)) %>%
  mutate(percentage = (`2021` - `2000`)/`2000`*100)
 
