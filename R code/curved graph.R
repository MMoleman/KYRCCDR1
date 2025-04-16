
df <- read_excel("Model_reformed_mitigaton.xlsx")
df <- df%>%select(Year, `BAU GHG`, Reform, Resilience, Mitigation,REF)%>%
  pivot_longer(
    cols = c(`BAU GHG`, Reform, REF, Resilience, Mitigation),
    names_to = "Scenario",  # Name of the new column
    values_to = "Total_direct"     # Name of the value column
  )

FAOstat <- read_excel("C:/Rfiles/WB/Analyzes CA report/OverviewCANIR_final.xlsx", 
                      sheet = "Converted") %>%
  filter(Country == "Kyrgyzstan") %>%
  select(FAOSTAT, Year) %>%
  group_by(Year) %>%
  summarise(Total_direct = sum(FAOSTAT, na.rm=TRUE)/1000) %>%
  filter(Year %in% c(2000:2022))

# Plot the projections for Total_direct and Protein_total for all scenarios
REF_2022_value <- df %>%
  filter(Scenario == "REF", Year == 2022) %>%
  summarise(Total_direct = sum(Total_direct, na.rm = TRUE)) %>%
  pull(Total_direct)

# Replace the FAOstat value for 2022 with the "REF" scenario value
FAOstat_updated <- FAOstat %>%
  mutate(Total_direct = if_else(Year == 2022, REF_2022_value/1000000000, Total_direct))

color_palette <- c(
  "Mitigation" = "red",
  "Resilience" = "#006747",  # Red
  "Reform" = "#FFB81C",  # Yellow
  "Current" = "Black"   # Green
)

df <- df%>%filter(Scenario != "REF")

# Plotting
p1 <- ggplot(df, aes(x = Year, y = Total_direct/1000000000, color = Scenario)) +
  geom_line(size = 1) +
  labs( x = "Year", y = "Direct Livestock Emissions 
       (mil tCO2-eq)") +
  theme_minimal(base_size = 15) +  # Increase base font size for better readability
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),   # X-axis title is already removed
    axis.title.y = element_text(size = 12, face = "bold"),  # Bold Y-axis title for emphasis
    axis.text = element_text(size = 11),  # Increase axis text size
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  # Title centered and bold
    strip.text = element_text(size = 11, face = "bold"),  # Facet labels in bold
    legend.text = element_text(size = 11),  # Adjust legend text size
    legend.position = "none"  # Position legend on top for clarity
  ) +
  geom_line(data = FAOstat_updated, aes(x = Year, y = Total_direct), color = "black", size = 1) +  # Make FAO line thicker
  geom_hline(yintercept = REF_2022_value /1000000000, color = "black", linetype = "dashed", size =0.8) +  # Dashed line with specified size
  annotate("text", x = 2000, y = REF_2022_value /1000000000, label = "REF 2022", color = "black", 
           hjust = 0, vjust = -0.5, size = 3) +  # Increase annotation size and make bold
  scale_color_manual(values = color_palette)

df <- read_excel("C:/Rfiles/WB/CCDRKYR/KYRCCDR1/R code/Kyrgyz_all_scenarios_data1.xlsx")

df <- df %>%
  mutate(Scenario = factor(Scenario, levels = c("Broiler", "Current", "Reformed", "Resilience", "REF")))


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
  filter(Scenario != "REF") %>%
  mutate(Scenario = case_when(
    Scenario == "Broiler" ~ "Mitigation",
    Scenario ==  "Current" ~  "Current",
    Scenario == "Resilience" ~ "Resilience",
    Scenario == "Reformed" ~ "Reform",
    TRUE ~ Scenario
  )) %>%
  mutate(Scenario = factor(Scenario, levels = c("Mitigation", "Current","Reform",  "Resilience")))


# Plotting
p2 <- ggplot() +
  # Bars for all scenarios (excluding "REF") but with horizontal bars
  geom_bar(data = df_no_REF, 
           aes(x = Scenario, y = Protein_cap, fill = Scenario), 
           stat = "identity", position = "dodge", alpha = 0.8) +
  
  # Dotted line for the "REF" scenario (horizontal line across the entire chart)
  geom_hline(data = df_REF, aes(yintercept = Protein_cap), 
             color = "black", linetype = "dashed", size = 0.8) +
  
  # Annotation for the dashed line, horizontal text, above the dashed line
  annotate("text", x = 1, y = df_REF$Protein_cap, label = "REF 2022", color = "black", 
           angle = -90, hjust = 4, vjust = -0.5, size = 3) +  # Adjust position of the annotation
  
  # Labels and customizations
  labs(x = "Scenario", y = "Animal Protein Production in 2050 (g/cap/d)") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.title.y = element_blank(),  # Remove x-axis title
    axis.title.x = element_text(size = 12, face = "bold"),  # Bold Y-axis title
    axis.text = element_text(size = 11),  # Increase axis text size
    axis.ticks.length = unit(0.25, "cm"),  # Adjust tick length
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  # Title centered and bold
    legend.text = element_text(size = 11),  # Adjust legend text size
    legend.position = "none"  # Remove legend
  ) +
  scale_fill_manual(values = color_palette) +
  coord_flip()  # Flip the axes for horizontal bars

output_file <- ggarrange(p1, p2, 
                         labels = c("A", "B"),  # Assign titles "A" and "B" for the plots
                         ncol = 2, nrow = 1,     # Arrange them side by side,           # Align both horizontally and vertically
                         widths = c(1.1, 1))     # Set width for each plot

# Save the arranged plot to a file
ggsave("C:/Rfiles/WB/CCDRKYR/KYRCCDR1/Graphs/mitigationKYRnew.png", 
       plot = output_file,  # Provide the plot object
       width = 9,          # Width of the output image (in inches)
       height = 3.7,          # Height of the output image (in inches)
       dpi = 300)    
