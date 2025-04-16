#total emissions gleam default values:
library(dplyr)       # For data manipulation
library(readxl)      # For reading Excel files
library(ggplot2)     # For data visualization
library(ggpubr)      # For arranging multiple ggplot objects (provides ggarrange)
library(tidyr)       # For tidying data
library(readr)
library(writexl)

Gleam_def_adoptation <- read_excel("R code/KYR_adop_54_3.xlsx")
Gleam_def_mititgation <- read_excel("R code/KYR_mitigation_54_3.xlsx") 

# Combining all dataframes
Gleam_def <- merge(Gleam_def_adoptation, Gleam_def_mititgation, by=c(1:6)) %>%
  select(-c(2,5,9,11,13,14,17,18))

Shiftbroiler <- 0.85
Entericadd <- 0.3
Entericadop <- 0.20

Gleam_def1 <- Gleam_def %>%
  filter(
    (Specie == "Cattle" & orientation == "Dairy" & system == "Grassland Based") |
      (Specie == "Sheep" & orientation == "Meat" & system == "Grassland Based")|
      (Specie == "Goat" & orientation == "Meat" & system == "Grassland Based"))%>%
  filter(parameter %in% c(
    "CH4: Manure - CH4 from manure management",
    "CH4: enteric fermentation",
    "N2O from manure management",
    "Milk production (Adult Females)",
    "System meat production in carcass weight",
    "Number of heads",
    "GHG emissions linked to milk production",
    "GHG emissions linked to meat production",
    "Milk emission intensity",
    "Meat emission intensity",
    "Milk_protein",
    "Meat_protein",
    "Protein_total",
    "Feed: N2O from manure applied and deposited"
  )) %>%
  pivot_longer(cols = c(5:10), names_to = "Scenario", values_to = "Value") %>%
  mutate(
    parameter = gsub(":", "", parameter),  # Remove colons from parameter names
    parameter = gsub(" ", "_", parameter), # Replace spaces with underscores for easier referencing
    Value = case_when(
      parameter == "CH4_Manure_-_CH4_from_manure_management" ~ Value * 27,
      parameter == "CH4_enteric_fermentation" ~ Value * 27,
      parameter == "N2O_from_manure_management"  ~ Value * 273,
      parameter == "Feed_N2O_from_manure_applied_and_deposited" ~ Value * 273,
      TRUE ~ Value
    ),
    Year = case_when(
      Scenario == "REF" ~ 2022,
      Scenario == "Current" ~ 2050,
      Scenario == "Reformed" ~ 2050,
      Scenario == "Resilience" ~ 2050,
      Scenario == "House" ~ 2050,
      Scenario == "Individual" ~ 2050,
      TRUE ~ NA_real_
    )
  ) %>%
  pivot_wider(names_from = parameter, values_from = Value) %>%
  mutate(`CH4_enteric_fermentation` = ifelse(Specie == "Cattle" & Scenario == "Individual", 
                                             `CH4_enteric_fermentation` - (`CH4_enteric_fermentation`*Entericadd*Entericadop),
                                             `CH4_enteric_fermentation`),
    Total_direct = `CH4_Manure_-_CH4_from_manure_management` + 
      `CH4_enteric_fermentation` + 
      `N2O_from_manure_management` + Feed_N2O_from_manure_applied_and_deposited,
    Meat_protein = `GHG_emissions_linked_to_meat_production` / `Meat_emission_intensity`,
    Milk_protein = `GHG_emissions_linked_to_milk_production` / `Milk_emission_intensity`,
    Protein_total = ifelse(orientation == "Meat", Meat_protein, Meat_protein + Milk_protein),
    `Number_of_heads` = `Number_of_heads`) %>%
  group_by(Specie, Year, Scenario) %>%
  summarise(
    Total_direct = sum(Total_direct, na.rm = TRUE),
    Meat_protein = sum(Meat_protein, na.rm = TRUE),
    Milk_protein = sum(Milk_protein, na.rm = TRUE),
    Protein_total = sum(Protein_total, na.rm = TRUE),
    Number_of_heads = sum(`Number_of_heads`, na.rm = TRUE)
  ) %>%
  group_by(Scenario = if_else(Scenario %in% c("Individual", "House"), "Mitigation_eff", Scenario), Year, Specie) %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")%>%
  mutate(
    ratio_protein_Milk = ifelse(Milk_protein == 0, NA, Milk_protein /Protein_total),
    ratio_protein_Meat = ifelse(Milk_protein == 0, NA, Meat_protein/Protein_total),
    ratio_protein_Milk2 = ifelse(Milk_protein == 0, NA, Milk_protein / Meat_protein),
    EIprotein = Total_direct / Protein_total,
    EIhead = Total_direct / `Number_of_heads`
  ) 

Totalprotein <- Gleam_def1 %>%
  group_by(Scenario, Year) %>%
  filter(Scenario != "REF") %>%
  summarise(Protein_total = sum(Protein_total, na.rm=TRUE))%>%
  pivot_wider(names_from = Scenario, values_from = Protein_total)%>%
  mutate(difference_protein_herd = Mitigation_eff-Current,
         Current_protein = Current,
         Specie = "Cattle") %>%
  select(difference_protein_herd, Year, Current_protein, Specie)

Herd <- Gleam_def1 %>%
  left_join(Totalprotein, by = c("Year", "Specie")) %>%
  filter(Scenario == "Mitigation_eff") %>%
  mutate(Scenario = "Herd",
         Total_direct = ifelse(Specie == "Cattle", Total_direct-(difference_protein_herd*EIprotein),Total_direct),
         Protein_total = ifelse(Specie == "Cattle", Protein_total-difference_protein_herd, Protein_total),
         Milk_protein = ifelse(Specie == "Cattle", Protein_total*ratio_protein_Milk, Milk_protein),
         Meat_protein = ifelse(Specie == "Cattle", Protein_total*ratio_protein_Meat, Meat_protein),
         `Number_of_heads` = ifelse(Specie == "Cattle", Total_direct/EIhead, `Number_of_heads`))

test <- Herd %>%
  summarise(BAU_protein =sum(Current_protein, na.rm=TRUE),
            Protein_total =sum(Protein_total, na.rm=TRUE))

##############################################################################################################


Broiler <- Herd %>%
  filter(Scenario == "Herd") %>%
  mutate(Scenario = "Broiler", 
         Meat_protein = ifelse(Specie == "Cattle", Meat_protein * Shiftbroiler, Meat_protein),
         Broiler_protein = ifelse(Specie == "Cattle", Meat_protein * (1-Shiftbroiler), 0),
         Milk_protein = ifelse(Specie == "Cattle", Meat_protein * ratio_protein_Milk2, Milk_protein),
         Protein_total = ifelse(Specie == "Cattle", Milk_protein+Meat_protein, Protein_total),
         Total_direct = Protein_total * EIprotein,
         `Number_of_heads` = ifelse(Specie == "Cattle", EIhead/Total_direct, `Number_of_heads`))

#######################################################################################################
#add the broiler data
Broil_Kyr <- read_excel("C:/Rfiles/WB/Analyzes CA report/Gleam data/Broiler/BroilKyr.xlsx") 

# Optionally, combine all datasets into one
Broil <- bind_rows(Broil_Kyr) %>%
  filter(orientation == "All" & system == "Broiler") %>%
  select(c(1,3,6,8)) %>%
  filter(parameter %in% c(
    "CH4: enteric fermentation",
    "CH4: Manure - CH4 from manure management",
    "System meat production in carcass weight",
    "N2O from manure management",
    "GHG emissions linked to meat production",
    "Meat emission intensity",
    "Feed: N2O from manure applied and deposited"))  %>%
  mutate(
    parameter = gsub(":", "", parameter),  # Remove colons from parameter names
    parameter = gsub(" ", "_", parameter),
    Broiler = case_when(
      parameter == "CH4_Manure_-_CH4_from_manure_management" ~ Broiler * 27,
      parameter == "CH4_enteric_fermentation" ~ Broiler * 27,
      parameter == "N2O_from_manure_management" ~ Broiler * 273,
      parameter == "Feed_N2O_from_manure_applied_and_deposited" ~ Broiler * 273,
      TRUE ~ Broiler
    )) %>%
  pivot_wider(names_from = parameter, values_from = Broiler) %>%
  mutate(
    Total_direct = `CH4_Manure_-_CH4_from_manure_management` + 
      `CH4_enteric_fermentation` + 
      `N2O_from_manure_management`+Feed_N2O_from_manure_applied_and_deposited,
    Broiler_protein = `GHG_emissions_linked_to_meat_production` / `Meat_emission_intensity`,
    EIbroiler = Total_direct/Broiler_protein,
    Kgproteinperliveweight_broiler = System_meat_production_in_carcass_weight/Broiler_protein,
    Year = 2050) %>%
  select(EIbroiler, Kgproteinperliveweight_broiler, Year)

#Bind with...
Broiler1 <- Broiler %>%
  left_join(Broil, by=c("Year")) %>%
  filter(Specie == "Cattle") %>%
  mutate(
    Specie = "Broiler",  # Update Specie to "Broiler"
    Meat_protein = Broiler_protein,  # Assuming BAU_protein and Protein_total exist
    Protein_total = Meat_protein,  # Set Protein_total to Meat_protein
    Total_direct = Meat_protein * EIbroiler,  # Adjust Total_direct for Broiler
    Liveweight_Broiler = Meat_protein * Kgproteinperliveweight_broiler  # Adjust Liveweight_Broiler for Broiler
  ) %>%
  select(Specie, Meat_protein, Protein_total, Total_direct, Liveweight_Broiler, Year, Scenario, EIbroiler)

final <- bind_rows(Broiler, Broiler1, Herd, Gleam_def1)

#Populationdata
Pop <- read_excel("C:/Rfiles/WB/Analyzes CA report/Gleam data/population.xlsx") %>%
  select(Iso3, Time, Value) %>%
  mutate(Country = ifelse(Iso3 == "KGZ", "KYR", Iso3),
         Year = Time,
         Population = Value) %>%
  select(Country, Population, Year) %>%
  filter(Country == "KYR")

carc <- final %>%
  filter(Scenario == "Broiler" & Specie == "Broiler") %>%
  ungroup()%>%
  summarise(across(c(Liveweight_Broiler), sum, na.rm = TRUE)) %>%
  mutate(Year = 2050)%>%
  left_join(Pop, by = "Year") %>%
  mutate(
    Liveweight_Broiler = (Liveweight_Broiler*1000)/Population/52)


CA <- final %>%
  group_by(Scenario) %>%
  summarise(across(c("Total_direct",
                     "Protein_total", 'Meat_protein', 'Milk_protein'), sum, na.rm = TRUE)) %>%
  mutate(Year = ifelse(Scenario == "REF", 2022, 2050)) %>%
  left_join(Pop, by = "Year") %>%
  mutate(EI = Total_direct/Protein_total,
         Protein_cap = (Protein_total*1000)/Population/365)

write_xlsx(CA, path = "C:/Rfiles/WB/CCDRKYR/KYRCCDR1/R code/Kyrgyz_all_scenarios_data4.xlsx")

C:/Rfiles/WB/CCDRKYR/KYRCCDR1/R code


