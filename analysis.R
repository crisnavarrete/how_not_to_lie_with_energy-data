pacman::p_load(
  tidyverse,        
  ggplot2,          
  scales,           
  forcats,          
  sf,               
  rnaturalearth,    
  rnaturalearthdata,
  readxl,           
  chilemapas,
  here,
  readr,
  stringr,
  skimr,
  patchwork,
  cowplot,
  rnaturalearth,
  ggrepel,
  writexl
)


rm(list = ls())
options(scipen=999)

# Data ----
gross_gen <- read_excel(here("Datasets", "full_energy_dfs_2026_02_10.xlsx"),
                        sheet = "Gross Energy Generation (GWh)")

installed_cap <- read_excel(here("Datasets", "full_energy_dfs_2026_02_10.xlsx"),
                        sheet = "Installed Capacity (MW)")

primary_supply <- read_excel(here("Datasets", "full_energy_dfs_2026_02_10.xlsx"),
                            sheet = "Primary Energy Supply Mix (Tcal")

secondary_consumption <- read_excel(here("Datasets", "full_energy_dfs_2026_02_10.xlsx"),
                            sheet = "Final Energy ConsumptionSeconda")

consumption_sector <- read_excel(here("Datasets", "full_energy_dfs_2026_02_10.xlsx"),
                            sheet = "Total Energy Consumption by Sec")

electricity_consumption_sector <- read_excel(here("Datasets", "full_energy_dfs_2026_02_10.xlsx"),
                            sheet = "Electricity Consumption by Sect")

energy_intensity <- read_excel(here("Datasets", "full_energy_dfs_2026_02_10.xlsx"),
                            sheet = "Energy Intensity")

ghg_emissions <- read_excel(here("Datasets", "full_energy_dfs_2026_02_10.xlsx"),
                            sheet = "Greenhouse Gas (GHG) Emissions")

ghg_emissions_region <- read_excel(here("Datasets", "full_energy_dfs_2026_02_10.xlsx"),
                            sheet = "GHG Emissions Inventory by Regi")

co2_emissions_removals <- read_excel(here("Datasets", "full_energy_dfs_2026_02_10.xlsx"),
                                     sheet = "Total CO2 Emissions and Removal")

ch4_emissions_removals <- read_excel(here("Datasets", "full_energy_dfs_2026_02_10.xlsx"),
                                     sheet = "Total CH4 Emissions and Removal")

consumption_total_2008_2023 <- read_excel(here("Datasets", "consumption_total_2008_2023.xlsx"))

primary_import_2008_2023 <- read_excel(here("Datasets", "primary_import_2008_2023.xlsx"))

# Data handling -----

skim(gross_gen)

## Generation ----
gross_gen_long <- gross_gen |> 
  select(-total) |> 
  pivot_longer(-year, names_to = "energy_type", values_to = "gwh")

## Installed capacity -----
installed_cap_long <- installed_cap |> 
  select(-total) |> 
  pivot_longer(-year, names_to = "energy_type", values_to = "mw")


## Consumption ----
skim(consumption_total_2008_2023)

consumption_total_2008_2023 %>%
  count(combustible) %>%
  arrange(combustible) %>%
  print(n = Inf)

consumption_total_2008_2023 <- consumption_total_2008_2023 |>
  mutate(tj = tcal * 4.1868)

## Emissions ----
co2_long <- co2_emissions_removals |> 
  select(-total_national_emissions, -total_national_emissions_and_removals) |>
  pivot_longer(-year, names_to = "sector", values_to = "kt")

ghg_long <- ghg_emissions |> 
  select(-total_inventory) |>
  pivot_longer(-year, names_to = "sector", values_to = "MtCO2Eq")

## DCI ----
### Recode combustibles

consumption_total_2008_2023 <- consumption_total_2008_2023 %>%
  mutate(
    fuel = case_when(
      combustible %in% c(
        "Carbón",
        "Coque Mineral",
        "Gas Coque",
        "Gas de Altos Hornos",
        "Alquitrán") ~ "Coal",
      combustible %in% c(
        "Coque de Petróleo") ~ "Petcoke",
      combustible %in% c(
        "Petróleo Diesel") ~ "Diesel",
      combustible %in% c(
        "Petróleo Combustible",
        "D.I. de Petróleo") ~ "Fuel Oil",
      combustible %in% c(
        "Petróleo Crudo") ~ "Crude Oil",
      combustible %in% c(
        "Gasolina de motor",
        "Gasolina de Aviación",
        "Kerosene",
        "Kerosene de Aviación",
        "Nafta") ~ "Oil Products",
      combustible %in% c(
        "Gas Natural",
        "Gas Corriente",
        "Gas de Refinería") ~ "Natural Gas",
      combustible %in% c(
        "Gas Licuado") ~ "LPG",
      combustible %in% c(
        "Biomasa",
        "Pellet de Biomasa") ~ "Biomass",
      combustible == "Biogás" ~ "Biogas",
      combustible == "Licor Negro" ~ "Black Liquor",
      combustible %in% c(
        "Electricidad",
        "Energía Eólica",
        "Energía Hídrica",
        "Energía Solar",
        "Geotermia") ~ "Non-carbon",
      TRUE ~ "Other"
    )
  )

consumption_total_2008_2023 %>%
  count(fuel) %>%
  arrange(fuel) %>%
  print(n = Inf)

consumption_total_2008_2023 %>%
  filter(fuel == "Non-carbon") %>%
  summarise(tcal_sum = sum(tcal, na.rm = TRUE))

### emission factors 
factors_dci <- tibble(
  fuel = c(
    "Coal",
    "Petcoke",
    "Diesel",
    "Fuel Oil",
    "Crude Oil",
    "Oil Products",
    "Natural Gas",
    "LPG",
    "Biomass",
    "Biogas",
    "Black Liquor",
    "Non-carbon"
  ),
  fe_CO2_tCO2_TJ = c(
    94.6,  # Coal
    97.5,  # Petcoke
    74.1,  # Diesel
    77.4,  # Fuel oil
    73.3,  # Crude oil (IPCC default)
    73.3,  # Refined oil average
    56.1,  # Natural gas
    63.1,  # LPG
    0.0,   # Biomass
    0.0,   # Biogas
    0.0,   # Black liquor
    0.0    # Electricity from non-carbon sources
  )
)

energy_dci <- consumption_total_2008_2023 %>%
  left_join(factors_dci, by = "fuel")



#### Check for unmatched fuels
energy_dci %>%
  filter(is.na(fe_CO2_tCO2_TJ)) %>%
  distinct(fuel)

#### Estimating the DCI 
ci_year <- energy_dci %>%
  group_by(año) %>%
  summarise(
    total_energy_tj = sum(tj, na.rm = TRUE),
    total_emissions = sum(tj * fe_CO2_tCO2_TJ, na.rm = TRUE),
    CI_t = total_emissions / total_energy_tj,
    .groups = "drop"
  )

CI_coal <- factors_dci %>%
  filter(fuel == "Coal") %>%
  pull(fe_CO2_tCO2_TJ)

dci <- ci_year %>%
  mutate(
    DCI = 1 - (CI_t / CI_coal)
  )

print(dci)

#### Decomposing the DCI

energy_dci <- energy_dci %>%
  mutate(
    is_noncarbon = fuel == "Non-carbon",
    is_oil_gas = fuel %in% c(
      "Diesel", "Fuel Oil", "Crude Oil",
      "Oil Products", "Natural Gas", "LPG"
    )
  )

energy_shares <- energy_dci %>%
  group_by(año) %>%
  summarise(
    total_energy_tj = sum(tj, na.rm = TRUE),
    noncarbon_energy_tj = sum(tj[is_noncarbon], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    DCINC = noncarbon_energy_tj / total_energy_tj
  )

dci_decomposed <- dci %>%
  left_join(energy_shares, by = "año") %>%
  mutate(
    DCIOG = DCI - DCINC
  )

all.equal(
  dci_decomposed$DCI,
  dci_decomposed$DCIOG + dci_decomposed$DCINC
)

dci_long <- dci_decomposed %>%
  select(año, DCI, DCIOG, DCINC) %>%
  pivot_longer(
    cols = c(DCI, DCIOG, DCINC),
    names_to = "indicator",
    values_to = "value"
  )


# Plots ----

## Gen -----
gross_gen_long |> 
  group_by(year) |> 
  mutate(share = gwh / sum(gwh)) |> 
  ggplot(aes(x = year, y = share, fill = energy_type)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = gross_gen_long$year) +
  labs(x = "Year", y = "Share of Gross Generation", fill = "Energy Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gross_gen_long |> 
  ggplot(aes(x = year, y = gwh, fill = energy_type)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = gross_gen_long$year) + 
  labs(x = "Year", y = "Gross Generation (GWh)", fill = "Energy Type") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Primary x Import ----
primary_import_2008_2023 <- primary_import_2008_2023 |> 
  filter(
    combustible %in% c("Petróleo Crudo", "Gas Natural", "Carbón"))

primary_import_2008_2023 %>%
  ggplot(aes(x = año, y = tcal, fill = categoria)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = unique(primary_import_2008_2023$año)) +
  labs(x = "Year", y = "Primary Supply & Imports (Tcal)", fill = "Category") +
  facet_wrap(~combustible, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

primary_import_2008_2023 %>%
  ggplot(aes(x = año, y = tcal, fill = categoria)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = unique(primary_import_2008_2023$año)) +
  labs(x = "Year", y = "Primary Supply & Imports (Tcal)", fill = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Installed cap -----
installed_cap_long |> 
  group_by(year) |> 
  mutate(share = mw / sum(mw)) |> 
  ggplot(aes(x = year, y = share, fill = energy_type)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = installed_cap_long$year) +
  labs(x = "Year", y = "Share of Installed Capacity", fill = "Energy Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


installed_cap_long |> 
  ggplot(aes(x = year, y = mw, fill = energy_type)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = installed_cap_long$year) +
  labs(x = "Year", y = "Installed Capacity (MW)", fill = "Energy Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Consumption ----
consumption_total_2008_2023 |> 
  ggplot(aes(x = año, y = tcal, fill = combustible)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = consumption_total_2008_2023$año) +
  labs(x = "Year", y = "Consumption (tcal)", fill = "Energy Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

consumption_total_2008_2023 |> 
  ggplot(aes(x = año, y = tcal, fill = fuel)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = consumption_total_2008_2023$año) +
  labs(x = "Year", y = "Consumption (tcal)", fill = "Energy Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

consumption_total_2008_2023 %>%
  group_by(año) %>%
  summarise(total_tcal = sum(tcal, na.rm = TRUE)) %>%
  arrange(año)

## Emissions ----

co2_long %>%
  group_by(year, sector) %>% 
  summarise(total_kt = sum(kt, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  mutate(total_emissions = sum(total_kt, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = total_kt, fill = sector)) +
  geom_col() +
  geom_line(aes(y = total_emissions, group = 1), color = "black", linewidth = 1) +
  geom_point(aes(y = total_emissions), color = "black", size = 1.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = co2_long$year) +
  labs(x = "Year", y = "CO2 Emissions (kt)", fill = "Sector") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

co2_long %>%
  filter(sector != "land_use_change_and_forestry") %>%
  group_by(year, sector) %>% 
  summarise(total_kt = sum(kt, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  mutate(total_emissions = sum(total_kt, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = total_kt, fill = sector)) +
  geom_col()  +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = co2_long$year) +
  labs(x = "Year", y = "CO2 Emissions (kt)", fill = "Sector") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ghg_long %>%
  group_by(year, sector) %>% 
  summarise(total_MtCO2Eq = sum(MtCO2Eq, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  mutate(total_emissions = sum(total_MtCO2Eq, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = total_MtCO2Eq, fill = sector)) +
  geom_col()  +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = ghg_long$year) +
  labs(x = "Year", y = "GHG Emissions (MtCO2Eq)", fill = "Sector") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## DCI ----
ggplot(dci, aes(x = año, y = DCI)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_y_continuous(limits = c(0.2, 0.6), breaks = seq(0, 1, 0.1)) +
  scale_x_continuous(breaks = dci$año) +
  labs(
    title = "Decarbonization Index (DCI)",
    subtitle = "Baseline = 100% coal energy system",
    x = "Year",
    y = "DCI"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Decomp ----
ggplot(dci_long, aes(x = año, y = value, color = indicator)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(
      "DCI"   = "black",
      "DCIOG" = "#D55E00",
      "DCINC" = "#009E73"
    ),
    labels = c(
      "DCI"   = "DCI (Total)",
      "DCIOG" = "DCIOG (Oil & Gas)",
      "DCINC" = "DCINC (Non-carbon)"
    )
  ) +
  scale_y_continuous(limits = c(0, 0.6), breaks = seq(0, 1, 0.1)) +
  scale_x_continuous(breaks = dci$año) +
  labs(
    x = "Year",
    y = "Index value",
    color = NULL,
    title = "Decarbonization Index (DCI) and Contributions by Energy Source",
    subtitle = "Decomposition into Oil & Gas (DCIOG) and Non-carbon Energy (DCINC)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)) 

