

# Data frame creation
data <- data.frame(
  Growth_Strategy = c("b-reform growth", "a-maintained growth", "c-climate-resilient growth"),
  Year = rep(c(2022, 2050), each = 3),
  Total_GHG_Emissions = c(5.8, 5.8, 5.8, 10.4, 9.3, 8.1)
)

additional_data <- data.frame(
  Year = 2010:2022,
  Total_GHG_Emissions = c(4.3106726, 4.4465384, 4.5464277, 4.6952316, 4.8666415, 4.9444138, 5.0617893, 5.1761021, 5.3165558, 5.4688715, 5.5783095, 5.6561128, 5.7012274)
)

# Plot the data
ggplot() +
  geom_line(data=additional_data, aes(x=Year, y = Total_GHG_Emissions), size=1) + # Points at each year for each scenario
  geom_line(data=data, aes(x = Year, y = Total_GHG_Emissions, color = Growth_Strategy, group = Growth_Strategy),size = 1) + # Line for each scenario
  geom_point(data=data, aes(x = Year, y = Total_GHG_Emissions, color = Growth_Strategy, group = Growth_Strategy),size = 3) + # Points at each year for each scenario
  geom_line(data=additional_data, aes(x=Year, y = Total_GHG_Emissions), size=1, color="#F8766D") + # Points at each year for each scenario
  geom_hline(yintercept = 5.8, linetype = "dotted", color = "black") + # Reference line
  scale_y_continuous(limits = c(0, max(data$Total_GHG_Emissions) + 1)) + # Y-axis starts at 0
  labs(
    title = "Total GHG Emissions Scenarios (mil tCO2-eq/year)",
    x = "Year",
    y = "GHG Emissions (mil tCO2-eq/year)"
  ) +
  theme_minimal()
