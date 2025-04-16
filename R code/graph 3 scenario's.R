df <- read_excel("C:/Rfiles/WB/CCDRKYR/KYRCCDR1/R code/Kyrgyz_all_scenarios_data4.xlsx")

FAOstat <- read_excel("C:/Rfiles/WB/CCDRKYR/KYRCCDR1/FAOSTAT_data_en_4-5-2025.xls") %>%
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
  summarise(Value = sum(Value, na.rm=TRUE)/1000*0.96)%>%
  filter(Year %in% c(2000:2022))%>%
  ungroup()%>%
  select(Year, Value) %>%
  rename(Total_direct = Value)
FAOstat$Year <- as.numeric(FAOstat$Year)

# List of scenarios to process
scenarios <- c("Herd", "Current", "Broiler", "Resilience")

# Initialize an empty list to store all projections
annual_projection_all_list <- list()
# Loop through each scenario and calculate the projections
for (scenario in scenarios) {
  # Filter data for the current scenario
  scenario_data <- df %>%
    filter(Scenario %in% c(scenario, "REF")) %>%
    mutate(Scenario = scenario)
  # Calculate the projections for the current scenario
  projections <- scenario_data %>%
    do({
      # Extract 2022 and 2050 values for Total_direct and Protein_total
      total_direct_2022 <- .$Total_direct[.$Year == 2022]
      total_direct_2050 <- .$Total_direct[.$Year == 2050]
      protein_total_2022 <- .$Protein_total[.$Year == 2022]
      protein_total_2050 <- .$Protein_total[.$Year == 2050]
      # Create a sequence for years from 2022 to 2050
      years <- 2022:2050
      # Linear interpolation for Total_direct and Protein_total
      total_direct_values <- seq(total_direct_2022, total_direct_2050, length.out = length(years))
      protein_total_values <- seq(protein_total_2022, protein_total_2050, length.out = length(years))
      # Combine the projections into a data frame for the current scenario
      data.frame(
        Scenario = rep(scenario, length(years)),
        Year = years,
        Total_direct = total_direct_values,
        Protein_total = protein_total_values
      )
    })
  # Add the projections to the list
  annual_projection_all_list[[scenario]] <- projections
}
# Combine all the projections into a single data frame
annual_projection_all <- bind_rows(annual_projection_all_list)
annual_projection_all <- annual_projection_all %>% distinct()
# Print the final projections for all scenarios
print(annual_projection_all)
# Plot the projections for Total_direct and Protein_total for all scenarios
REF_2022_value <- df %>%
  filter(Scenario == "REF", Year == 2022) %>%
  summarise(Total_direct = sum(Total_direct, na.rm = TRUE)) %>%
  pull(Total_direct)
# Replace the FAOstat value for 2022 with the "REF" scenario value
FAOstat_updated <- FAOstat %>%
  mutate(Total_direct = if_else(Year == 2022, REF_2022_value/1000000000, Total_direct))
color_palette <- c(
  "Herd" =  "#FFB81C",
  "Broiler" = "#006747",  # Red
  "Resilience" = "#00A9E0",  # Yellow
  "Current" = "Black"   # Green
)


df <- df %>%
  mutate(Scenario = factor(Scenario, levels = c("Broiler", "Current", "Resilience", "Herd"),
                           labels = c("Reformed growth+ Herd control 
                                      + Dietary shift", "Current growth", "Reformed growth", "Reformed growth + Herd control")))
# Plotting
p1 <- ggplot(annual_projection_all, aes(x = Year, y = Total_direct/1000000000, color = Scenario)) +
  geom_line(size = 1.2) +
  labs( x = "Year", y = "Direct Livestock Emissions
(mil tCO2-eq)") +
  theme_minimal(base_size = 15) +  # Increase base font size for better readability
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),   # X-axis title is already removed
    axis.title.y = element_text(size = 14, face = "bold"),  # Bold Y-axis title for emphasis
    axis.text = element_text(size = 12),  # Increase axis text size
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  # Title centered and bold
    strip.text = element_text(size = 11, face = "bold"),  # Facet labels in bold
    legend.text = element_text(size = 12),  # Adjust legend text size
    legend.position = "none",
    panel.grid.minor.x = element_blank()
  ) +
  geom_line(data = FAOstat_updated, aes(x = Year, y = Total_direct), color = "black", size = 1.2) +  # Make FAO line thicker
  geom_hline(yintercept = REF_2022_value /1000000000, color = "black", linetype = "dashed", size =0.8) +  # Dashed line with specified size
  annotate("text", x = 2000, y = REF_2022_value /1000000000, label = "REF 2022", color = "black",
           hjust = 0, vjust = -0.5, size = 4) +  # Increase annotation size and make bold
  scale_color_manual(values = color_palette)
df <- read_excel("C:/Rfiles/WB/CCDRKYR/KYRCCDR1/R code/Kyrgyz_all_scenarios_data1.xlsx")
df <- df %>%
  mutate(Scenario = factor(Scenario, 
                           levels = c("Broiler", "REF", "Herd", "Current", "Resilience"),
                           labels = c("Reformed growth
+ Herd control
+ Dietary shift",
                                      "REF",
                                      "Reformed growth
+ Herd control",
                                      "Current growth",
                                      "Reformed growth")))
color_palette <- c(
  "Reformed growth 
  + Herd control 
  + Dietary shift" = "#006747",
  "Reformed growth 
  + Herd control" = "#FFB81C",
  "Current growth" = "Black",
  "Reformed growth" = "#00A9E0"
)
# Separate the "REF" scenario for the dotted line
df_REF <- df %>%
  filter(Scenario == "REF") %>%
  summarise(Protein_cap = mean(Protein_cap))  # Get a representative value for the REF line
# Filter out the "REF" scenario from the main data
df_no_REF <- df %>% filter(Scenario != "REF")
# Plotting
# Separate the "REF" scenario for the dotted line
df_REF <- df %>%
  filter(Scenario == "REF") %>%
  summarise(Protein_cap = mean(Protein_cap))  # Get a representative value for the REF line
# Filter out the "REF" scenario from the main data
df_no_REF <- df %>%
  filter(Scenario != "REF")
color_palette <- c(
  "Reformed growth 
  + Herd control 
  + Dietary shift" = "#006747",
  "Reformed growth 
  + Herd control" = "#FFB81C",
  "Current growth" = "Black",
  "Reformed growth" = "#00A9E0"
)
# Plotting
p2 <- ggplot() +
  # Bars for all scenarios (excluding "REF") but with horizontal bars
  geom_bar(data = df_no_REF,
           aes(x = Scenario, y = Protein_cap, fill = Scenario),
           stat = "identity", alpha = 0.8) +
  # Dotted line for the "REF" scenario (horizontal line across the entire chart)
  geom_hline(data = df_REF, aes(yintercept = Protein_cap),
             color = "black", linetype = "dashed", size = 0.8) +
  # Labels and customizations
  labs(x = "Scenario", y = "Animal Protein Production in 2050 
       (g/cap/d)") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.title.y = element_blank(),  # Remove x-axis title
    axis.title.x = element_text(size = 14, face = "bold"),  # Bold Y-axis title
    axis.text = element_text(size = 13),  # Increase axis text size
    axis.ticks.length = unit(0.25, "cm"),  # Adjust tick length
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  # Title centered and bold
    legend.text = element_text(size = 11),  # Adjust legend text size
    legend.position = "none"  # Remove legend
  ) +
  scale_fill_manual(values = c("#006747","#FFB81C", "black" ,"#00A9E0")) +
  coord_flip()

output_file <- ggarrange(p1, p2,
                         labels = c("A", "B"),  # Assign titles "A" and "B" for the plots
                         ncol = 2, nrow = 1,     # Arrange them side by side,           # Align both horizontally and vertically
                         widths = c(1.1, 1))     # Set width for each plot
# Save the arranged plot to a file
ggsave("C:/Rfiles/WB/CCDRKYR/KYRCCDR1/Graphs/mitigationKYRnew2.png",
       plot = output_file,  # Provide the plot object
       width = 9.7,          # Width of the output image (in inches)
       height = 3.7,          # Height of the output image (in inches)
       dpi = 300)
