df <- read_excel("gleam_nationaltotal_KyR.xlsx")

df <- df %>%
  filter(parameter %in% c("Total GHG emissions",
                          "Total CH4",
                          "Total N2O",
                          "Total CO2")) %>%
  filter((Specie %in% c("Chicken", "Pigs") & orientation == "All") | 
           (Specie %in% c("Cattle", "Sheep", "Goat", "Buffalo") & orientation != "All")) %>%
  filter(system != "All") %>%
  filter(system != "Selection")%>%
  group_by(Specie, parameter, unit) %>%
  summarise(value = sum(default, na.rm=TRUE))

total <- df %>%
  group_by(parameter, unit) %>%
  summarise(Total = sum(value, na.rm=TRUE))

merge <- left_join(df, total, by=c("parameter","unit"))
merge <- merge %>%
  mutate(percentage = value / Total * 100)%>%
  group_by(parameter) %>%
  arrange((value)) %>%  # Arrange by value in descending order within each parameter
  mutate(parameter = factor(parameter, levels = unique(parameter)),  # Set order for parameter
         Specie = factor(Specie, levels = unique(Specie))) %>%
  ungroup()

species_palette <- c("Cattle" = "#1f78b4",   # Blue
                     "Sheep" = "#33a02c",   # Green
                     "Goat" = "#e31a1c",    # Red
                     "Pigs" = "#ff7f00",    # Orange
                     "Chicken" = "#6a3d9a",  # Purple
                     "Buffalo" = "#fdbf6f")  # Light Yellow-Orange

merge$parameter <- factor(merge$parameter, 
                          levels = c("Total GHG emissions",
                                     "Total CH4",
                                     "Total N2O",
                                     "Total CO2"))

# Stacked bar plot with percentage labels
ggplot(merge, aes(x = parameter, y = percentage, fill = Specie)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = species_palette) +
  geom_text(aes(label = ifelse(percentage >= 5, sprintf("%.1f%%", percentage), "")),
            position = position_stack(vjust = 0.5), size = 3.5, fontface = "bold", color = "white") +
  labs(title = "Percentage Contribution by Species",
       y = "Percentage (%)", x = "Parameter") +
  theme_minimal()+
  theme(axis.title.x = element_blank())

test <- merge %>%
  mutate(CO2_eq = case_when(
    parameter == "Total CH4" ~ value * 25,
    parameter == "Total N2O" ~ value * 265,
    TRUE ~ value)) %>%
  group_by(Specie) %>%
  mutate(Total_CO2_eq = sum(CO2_eq[parameter == "Total GHG emissions"], na.rm = TRUE)) %>%
  filter(parameter != "Total GHG emissions") %>%
  mutate(
    label = case_when(
      parameter == "Total CH4" ~ "CH4_Percentage",
      parameter == "Total CO2" ~ "CO2_Percentage",
      parameter == "Total N2O" ~ "N2O_Percentage",
      TRUE ~ NA_character_),
    Percentage = (CO2_eq / Total_CO2_eq) * 100
  ) %>%
  ungroup()

species_palette <- c("Total CH4" = "#1f78b4",   # Blue
                     "Total N2O" = "#33a02c",   # Green
                     "Total CO2" = "#e31a1c")   # Red

test <- test %>%
  mutate(Show_Percentage = ifelse(Specie %in% c("Sheep", "Cattle") & Percentage > 10, Percentage, NA))

# Create the dodged bar plot of CO2 equivalents
ggplot(test, aes(x = Specie, y = CO2_eq / 1e9, fill = parameter)) +  # Use 'parameter' for coloring
  geom_bar(stat = "identity", position = "stack") +  # Stack bars
  scale_fill_manual(values = species_palette) +  # Custom color palette
  geom_text(aes(label = ifelse(!is.na(Show_Percentage), sprintf("%.1f%%", Show_Percentage), ""),
                y = CO2_eq / 1e9,  # Position text at the top of the CH4 bar
                group = parameter), 
            position = position_stack(vjust = 0.5),  # Align text with bars in the stack
            size = 3.5, fontface = "bold", color="White") +  # Adjust text size and style
  # Text formatting
  labs(title = "Emissions by Species (in million tCO2 eq)",
       y = "Million tCO2 eq", x = "Species") +
  theme_minimal(base_size = 15) +  # Set base font size for better readability
  theme(legend.title = element_blank(),  # Remove legend title
        legend.position = "top",  # Position legend at the top
        axis.title.x = element_blank())  # Remove x-axis title

test1 <- test %>%
  mutate(Total_CO2_eq = ifelse(parameter == "Total CH4", Total_CO2_eq, NA))
  

# Create the dodged bar plot of CO2 equivalents
ggplot(test1, aes(x = Specie, y = CO2_eq / 1e9, fill = parameter)) +  
  geom_bar(stat = "identity", position = "stack") +  # Stack bars by 'parameter'
  
  scale_fill_manual(values = species_palette) +  # Apply custom color palette
  
  # Add percentage labels within stacked segments, using 'Show_Percentage' values
  geom_text(aes(label = ifelse(!is.na(Show_Percentage), sprintf("%.1f%%", Show_Percentage), ""),
                y = CO2_eq / 1e9,  # Position labels within the stack
                group = parameter), 
            position = position_stack(vjust = 0.5),  # Center text within stacked bars
            size = 3.5, fontface = "bold", color = "white") +  # White text for readability
  
  # Add top-of-bar labels using 'Column7' values for each Specie
  geom_text(aes(label = round(Total_CO2_eq / 1e9, 2), y = CO2_eq / 1e9),  # Round Total_CO2_eq to 1 decimal
            stat = "identity", 
            position = position_stack(vjust = 1),  # Position labels above bars
            vjust = -0.5, size = 4, fontface = "bold", color = "black") +  # Larger font for visibility
  
  # Titles and axis labels
  labs(title = "Emissions by Species (in million tCO2 eq)",
       y = "Million tCO2 eq", x = "Species") +
  
  # Theme adjustments for aesthetics
  theme_minimal(base_size = 15) +
  theme(legend.title = element_blank(),          # Remove legend title
        legend.position = "top",                 # Position legend at the top
        axis.title.x = element_blank(),          # Remove x-axis title
        panel.grid.minor = element_blank(),      # Remove minor grid lines
        plot.title = element_text(face = "bold"))+
  ylim(0,4.2)



