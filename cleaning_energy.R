# Config ----

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
  writexl,
  paletteer,
  ggstream
)

options(scipen = 999)

# BNE cleaning ----
## 0. Paths ----
output_dir <- here::here("Datasets", "clean")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# I Cleaning ----
## 1. Data loading ----

energy_balance_2008_2021 <- read_delim(
  here("Datasets", "bne_balance_nacional.csv"),
  delim = ";",
  locale = locale(decimal_mark = ","),
  trim_ws = TRUE,
  show_col_types = FALSE,
  col_types = cols(
    año = col_integer(),
    item = col_character(),
    categoria = col_character(),
    seccion = col_character(),
    combustible = col_character(),
    tcal = col_double()
  )
)

energy_balance_2022_2024 <- read_excel(
  here("Datasets", "bne_2022_2024.xlsx")
)

# This keeps the script compatible if the year column comes as anio.
if ("anio" %in% names(energy_balance_2008_2021)) {
  energy_balance_2008_2021 <- energy_balance_2008_2021 |>
    rename(año = anio)
}

if ("anio" %in% names(energy_balance_2022_2024)) {
  energy_balance_2022_2024 <- energy_balance_2022_2024 |>
    rename(año = anio)
}

## 2. Standardize historical data ----
energy_balance_2008_2021 <- energy_balance_2008_2021 |>
  mutate(
    across(where(is.character), trimws),
    año = as.integer(año),
    tcal = as.numeric(tcal),
    tcal = if_else(abs(tcal) < 1e-9, 0, tcal),
    categoria = case_when(
      categoria == "Centrales Eléctricas: Autoproductores" ~ "Electricidad Autoproducción",
      categoria == "Centrales Eléctricas: Servicio Publico" ~ "Electricidad Servicio Público",
      categoria == "Siderurgia: Altos Hornos" ~ "Siderurgia Altos Hornos",
      categoria == "Siderurgia: Hornos de Coque" ~ "Siderurgia Hornos de Coque",
      categoria == "Refinería y Extracción Petróleo y Gas Natural" ~ "Refinería Petróleo - Gas Natural",
      TRUE ~ categoria
    ),
    seccion = case_when(
      seccion == "Derivados De Petróleo" ~ "Derivados de Petróleo",
      seccion == "Derivados De Carbón" ~ "Derivados de Carbón",
      TRUE ~ seccion
    )
  ) |>
  select(año, item, categoria, seccion, combustible, tcal)

## 3. Standardize 2022-2024 data ----

energy_balance_2022_2024 <- energy_balance_2022_2024 |>
  mutate(
    across(where(is.character), trimws),
    año = as.integer(año),
    tcal = as.numeric(tcal),,
    tcal = if_else(abs(tcal) < 1e-9, 0, tcal),
    categoria = case_when(
      categoria == "Centrales Eléctricas: Autoproductores" ~ "Electricidad Autoproducción",
      categoria == "Centrales Eléctricas: Servicio Publico" ~ "Electricidad Servicio Público",
      categoria == "Refinería y Extracción Petróleo y Gas Natural" ~ "Refinería Petróleo - Gas Natural",
      TRUE ~ categoria
    ),
    seccion = case_when(
      seccion == "Derivados del Petróleo" ~ "Derivados de Petróleo",
      seccion == "Derivados del Carbón" ~ "Derivados de Carbón",
      seccion == "Derivados De Petróleo" ~ "Derivados de Petróleo",
      seccion == "Derivados De Carbón" ~ "Derivados de Carbón",
      TRUE ~ seccion
    )
  ) |>
  select(año, item, categoria, seccion, combustible, tcal)

## 4. Full data set merging ----

bne_2008_2024 <- bind_rows(
  energy_balance_2008_2021,
  energy_balance_2022_2024
) |>
  distinct() |>
  arrange(año, item, categoria, seccion, combustible)

save(bne_2008_2024, file = here("Datasets", "clean", "bne_2008_2024.RData"))

# II Further cleaning and subseting for analysis ----

## 1. Remove duplicate values ----

### keeping a copy of total rows before removing them
bne_total_rows_2008_2024 <- bne_2008_2024 |>
  filter(
    categoria %in% c("Oferta Total", "Consumo Final"),
    item %in% c("OFERTA", "CONSUMO FINAL", "C.TRANSFO.")
  )

### removing total counts in rows
bne_2008_2024_cleaned <- bne_2008_2024 |>
  filter(
    !(item == "OFERTA" & categoria == "Oferta Total"),
    !(item == "CONSUMO FINAL" & categoria == "Consumo Final"),
    !(item == "C.TRANSFO." & categoria == "Consumo Final")
  )

## 2. Sector recode for final consumption ----

### creating sector_consumo variable for final consumption
bne_2008_2024_cleaned <- bne_2008_2024_cleaned |>
  mutate(
    sector_consumo = case_when(
      
      # Energy own use
      item == "CONSUMO FINAL" & categoria %in% c(
        "Sector Energético: Auto Consumo",
        "Carbón y Leña",
        "Electricidad",
        "Siderurgia Hornos de Coque",
        "Siderurgia Altos Hornos",
        "Plantas de Gas",
        "Refinería Petróleo - Gas Natural",
        "Producción de Metanol"
      ) ~ "Sector Energético: Auto Consumo",
      
      # Industry and mining
      item == "CONSUMO FINAL" & categoria %in% c(
        "Sector Industrial y Minero",
        "Cobre",
        "Salitre",
        "Hierro",
        "Papel y Celulosa",
        "Siderurgia",
        "Petroquímica",
        "Cemento",
        "Azúcar",
        "Pesca",
        "Agroindustria",
        "Construcción",
        "Industria Láctea",
        "Industrias Varias",
        "Minas Varias"
      ) ~ "Sector Industrial y Minero",
      
      # Transport
      item == "CONSUMO FINAL" & categoria %in% c(
        "Sector Transporte",
        "Terrestre",
        "Ferroviario",
        "Marítimo",
        "Aéreo",
        "Transporte por Ducto"
      ) ~ "Sector Transporte",
      
      # Commercial, public and residential
      item == "CONSUMO FINAL" & categoria %in% c(
        "Sector Comercial, Público y Residencial",
        "Comercial",
        "Público",
        "Sanitarias",
        "Residencial"
      ) ~ "Sector Comercial, Público y Residencial",
      
      # Non-energy use
      item == "CONSUMO FINAL" & categoria %in% c(
        "Cons. No Energético - Industrial"
      ) ~ "Cons. No Energético - Industrial",
      
      TRUE ~ NA_character_
    )
  )

### dropping duplicated sector_consumo rows in CONSUMO FINAL
bne_2008_2024_cleaned <- bne_2008_2024_cleaned |>
  filter(
    !(item == "CONSUMO FINAL" & categoria == sector_consumo)
  )

#### check: final consumption categories not assigned to sector_consumo
missing_sector_consumo_2008_2024 <- bne_2008_2024_cleaned |>
  filter(item == "CONSUMO FINAL", is.na(sector_consumo)) |>
  count(categoria, sort = TRUE)

if (nrow(missing_sector_consumo_2008_2024) > 0) {
  print(missing_sector_consumo_2008_2024)
  stop("Some CONSUMO FINAL categories were not assigned to sector_consumo.")
}

#### check: final consumption hierarchy
bne_2008_2024_cleaned |>
  filter(item == "CONSUMO FINAL") |>
  select(sector_consumo, categoria) |>
  distinct() |>
  arrange(sector_consumo, categoria) |>
  print(n = Inf)

## 3. Fuel recode ----

bne_2008_2024_cleaned <- bne_2008_2024_cleaned |>
  mutate(
    across(where(is.character), trimws),
    fuel = case_when(
      combustible %in% c(
        "Carbón",
        "Coque Mineral",
        "Coque de Petróleo",
        "Alquitrán",
        "Gas Coque",
        "Gas de Altos Hornos"
      ) ~ "Coal and Coke",
      
      combustible %in% c(
        "Petróleo Crudo",
        "Petróleo Diesel",
        "Petróleo Combustible",
        "Gasolina de motor",
        "Gasolina de Aviación",
        "Kerosene",
        "Kerosen",
        "Kerosene de Aviación",
        "Nafta",
        "Metanol",
        "D.I. de Petróleo",
        "Gas Licuado",
        "Gas de Refinería"
      ) ~ "Oil products",
      
      combustible %in% c(
        "Gas Natural",
        "Gas Corriente"
      ) ~ "Natural gas",
      
      combustible %in% c(
        "Biomasa",
        "Biogás",
        "Pellet de Biomasa",
        "Licor Negro"
      ) ~ "Bioenergy",
      
      combustible == "Energía Hídrica" ~ "Hydro",
      combustible == "Energía Eólica" ~ "Wind",
      combustible == "Energía Solar" ~ "Solar",
      combustible == "Geotermia" ~ "Geothermal",
      combustible == "Electricidad" ~ "Electricity",
      TRUE ~ "Other"
    )
  )

#### check: fuels not assigned to fuel
missing_fuel_2008_2024 <- bne_2008_2024_cleaned |>
  filter(fuel == "Other") |>
  distinct(item, categoria, seccion, combustible) |>
  arrange(item, categoria, seccion, combustible)

if (nrow(missing_fuel_2008_2024) > 0) {
  print(missing_fuel_2008_2024)
  warning("Some fuels were assigned to Other. Check whether this is expected.")
}

# III Core subsets ----

## 1. Supply ----

energy_supply_2008_2024 <- bne_2008_2024_cleaned |>
  filter(item == "OFERTA")

primary_fuels <- energy_supply_2008_2024 |>
  filter(
    categoria == "Producción Primaria",
    tcal > 0
  ) |>
  distinct(fuel) |>
  pull(fuel)

energy_primary_supply_2008_2024 <- energy_supply_2008_2024|>
  filter(
    categoria %in% c(
      "Producción Primaria",
      "Importación",
      "Exportación"
    ),
    fuel %in% primary_fuels
  ) |>
  mutate(
    tcal = if_else(categoria == "Exportación", -tcal, tcal)
  ) 

energy_primary_supply_2008_2024 <- energy_supply_2008_2024 |>
  filter(categoria %in% c("Producción Primaria", "Importación", "Exportación"))

energy_primary_production_2008_2024 <- energy_supply_2008_2024 |>
  filter(categoria == "Producción Primaria")

energy_primary_import_2008_2024 <- energy_supply_2008_2024 |>
  filter(categoria == "Importación")

energy_supply_components_2008_2024 <- energy_supply_2008_2024 |>
  filter(categoria != "Oferta Total")

## 2. Consumption ----

energy_consumption_total_2008_2024 <- bne_2008_2024_cleaned |>
  filter(item %in% c("CONSUMO FINAL", "C.TRANSFO."))

energy_consumption_final_2008_2024 <- bne_2008_2024_cleaned |>
  filter(item == "CONSUMO FINAL", !is.na(sector_consumo), tcal > 0)

energy_consumption_transformation_2008_2024 <- bne_2008_2024_cleaned |>
  filter(item == "C.TRANSFO.")

## 3. Sector subsets for final consumption ----

consumption_transport_2008_2024 <- energy_consumption_final_2008_2024 |>
  filter(sector_consumo == "Sector Transporte")

consumption_industry_mining_2008_2024 <- energy_consumption_final_2008_2024 |>
  filter(sector_consumo == "Sector Industrial y Minero")

consumption_commercial_public_residential_2008_2024 <- energy_consumption_final_2008_2024 |>
  filter(sector_consumo == "Sector Comercial, Público y Residencial")

consumption_energy_own_use_2008_2024 <- energy_consumption_final_2008_2024 |>
  filter(sector_consumo == "Sector Energético: Auto Consumo")

consumption_non_energy_2008_2024 <- energy_consumption_final_2008_2024 |>
  filter(sector_consumo == "Cons. No Energético - Industrial")

## 5. Fuel subsets ----

fuel_coal_coke_2008_2024 <- bne_2008_2024_cleaned |>
  filter(fuel == "Coal and Coke")

fuel_oil_products_2008_2024 <- bne_2008_2024_cleaned |>
  filter(fuel == "Oil products")

fuel_natural_gas_2008_2024 <- bne_2008_2024_cleaned |>
  filter(fuel == "Natural gas")

fuel_bioenergy_2008_2024 <- bne_2008_2024_cleaned |>
  filter(fuel == "Bioenergy")

fuel_electricity_2008_2024 <- bne_2008_2024_cleaned |>
  filter(fuel == "Electricity")

fuel_hydro_2008_2024 <- bne_2008_2024_cleaned |>
  filter(fuel == "Hydro")

fuel_wind_2008_2024 <- bne_2008_2024_cleaned |>
  filter(fuel == "Wind")

fuel_solar_2008_2024 <- bne_2008_2024_cleaned |>
  filter(fuel == "Solar")

fuel_geothermal_2008_2024 <- bne_2008_2024_cleaned |>
  filter(fuel == "Geothermal")

## 6. Final fuel subsets ----

consumption_final_coal_coke_2008_2024 <- energy_consumption_final_2008_2024 |>
  filter(fuel == "Coal and Coke")

consumption_final_oil_products_2008_2024 <- energy_consumption_final_2008_2024 |>
  filter(fuel == "Oil products")

consumption_final_natural_gas_2008_2024 <- energy_consumption_final_2008_2024 |>
  filter(fuel == "Natural gas")

consumption_final_bioenergy_2008_2024 <- energy_consumption_final_2008_2024 |>
  filter(fuel == "Bioenergy")

consumption_final_electricity_2008_2024 <- energy_consumption_final_2008_2024 |>
  filter(fuel == "Electricity")



# IV Summary tables ----

## 1. BNE summaries ----
bne_year_item_2008_2024 <- bne_2008_2024_cleaned |>
  group_by(año, item) |>
  summarise(tcal = sum(tcal, na.rm = TRUE), .groups = "drop") |>
  arrange(año, item)

bne_year_item_2008_2024

bne_year_category_2008_2024 <- bne_2008_2024_cleaned |>
  group_by(año, item, categoria) |>
  summarise(tcal = sum(tcal, na.rm = TRUE), .groups = "drop") |>
  arrange(año, item, categoria)

bne_year_category_2008_2024

bne_year_section_2008_2024 <- bne_2008_2024_cleaned |>
  group_by(año, item, seccion) |>
  summarise(tcal = sum(tcal, na.rm = TRUE), .groups = "drop") |>
  arrange(año, item, seccion)

bne_year_section_2008_2024

bne_year_fuel_2008_2024 <- bne_2008_2024_cleaned |>
  group_by(año, item, fuel) |>
  summarise(tcal = sum(tcal, na.rm = TRUE), .groups = "drop") |>
  arrange(año, item, fuel)

bne_year_fuel_2008_2024

## 2. Consumption summaries ----

consumption_final_year_sector_2008_2024 <- energy_consumption_final_2008_2024 |>
  filter(tcal > 0) |>
  group_by(año, sector_consumo) |>
  summarise(tcal = sum(tcal, na.rm = TRUE), .groups = "drop") |>
  arrange(año, sector_consumo) 

consumption_final_year_fuel_2008_2024 <- energy_consumption_final_2008_2024 |>
  filter(tcal > 0) |>
  group_by(año, fuel) |>
  summarise(tcal = sum(tcal, na.rm = TRUE), .groups = "drop") |>
  arrange(año, fuel)

consumption_final_year_sector_fuel_2008_2024 <- energy_consumption_final_2008_2024 |>
  filter(tcal > 0) |>
  group_by(año, sector_consumo, fuel) |>
  summarise(tcal = sum(tcal, na.rm = TRUE), .groups = "drop") |>
  arrange(año, sector_consumo, fuel)


## 3. Transformation summaries ----
transformation_year_category_2008_2024 <- energy_consumption_transformation_2008_2024 |>
  group_by(año, categoria) |>
  summarise(tcal = sum(tcal, na.rm = TRUE), .groups = "drop") |>
  arrange(año, categoria)

## 4. Supply summaries ----
primary_supply_year_fuel_2008_2024 <- energy_primary_supply_2008_2024 |>
  group_by(año, combustible, fuel) |>
  summarise(tcal = sum(tcal, na.rm = TRUE), .groups = "drop") |>
  arrange(año, combustible)

primary_supply_year_fuel_2008_2024

primary_import_year_fuel_2008_2024 <- energy_primary_import_2008_2024 |>
  group_by(año, combustible, fuel) |>
  summarise(tcal = sum(tcal, na.rm = TRUE), .groups = "drop") |>
  arrange(año, combustible)

primary_import_year_fuel_2008_2024

primary_production_year_fuel_2008_2024 <- energy_primary_production_2008_2024 |>
  filter(tcal > 0) |>
  group_by(año, combustible, fuel) |>
  summarise(tcal = sum(tcal, na.rm = TRUE), .groups = "drop") |>
  arrange(año, combustible)

primary_production_year_fuel_2008_2024

# V DCI ----
energy_consumption_final_2008_2024 |>
  group_by(combustible) |>
  summarise(
    total_tcal = sum(tcal, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(total_tcal)) |> 
  print(n = Inf)

## 0.  recode fuel_dci for DCI calculation -----
energy_consumption_final_2008_2024 <- energy_consumption_final_2008_2024 |>
  mutate(
    fuel_dci = case_when(
      combustible %in% c(
        "Carbón",
        "Coque Mineral",
        "Gas Coque",
        "Gas de Altos Hornos",
        "Alquitrán"
      ) ~ "Coal",
      
      combustible == "Coque de Petróleo" ~ "Petcoke",
      
      combustible == "Petróleo Diesel" ~ "Diesel",
      
      combustible %in% c(
        "Petróleo Combustible",
        "D.I. de Petróleo"
      ) ~ "Fuel Oil",
      
      combustible == "Petróleo Crudo" ~ "Crude Oil",
      
      combustible %in% c(
        "Gasolina de motor",
        "Gasolina de Aviación",
        "Kerosene",
        "Kerosen",
        "Kerosene de Aviación",
        "Nafta",
        "Metanol"
      ) ~ "Oil Products",
      
      combustible %in% c(
        "Gas Natural",
        "Gas Corriente",
        "Gas de Refinería"
      ) ~ "Natural Gas",
      
      combustible == "Gas Licuado" ~ "LPG",
      
      combustible %in% c(
        "Biomasa",
        "Pellet de Biomasa"
      ) ~ "Biomass",
      
      combustible == "Biogás" ~ "Biogas",
      
      combustible == "Licor Negro" ~ "Black Liquor",
      
      combustible == "Electricidad" ~ "Non-carbon",
      
      TRUE ~ NA_character_
    )
  )

### check
energy_consumption_final_2008_2024 |>
  filter(is.na(fuel_dci)) |>
  distinct(combustible)

## 1. Emission factors ----

bne_dci_factors_2008_2024 <- tibble(
  fuel_dci = c(
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
    73.3,  # Crude oil
    73.3,  # Refined oil average
    56.1,  # Natural gas
    63.1,  # LPG
    0.0,   # Biomass
    0.0,   # Biogas
    0.0,   # Black liquor
    0.0    # Electricity / non-carbon direct use
  )
)

## 2. DCI data ----

### DCI uses final consumption leaf rows only.
### Non-energy use is excluded because it does not represent direct energy combustion
bne_dci_final_2008_2024 <- energy_consumption_final_2008_2024 |>
  filter(
    sector_consumo != "Cons. No Energético - Industrial"
  ) |>
  mutate(
    tj = tcal * 4.1868
  ) |>
  left_join(bne_dci_factors_2008_2024, by = "fuel_dci")

### Check: unmatched DCI factors
bne_dci_unmatched_2008_2024 <- bne_dci_final_2008_2024 |>
  filter(is.na(fe_CO2_tCO2_TJ) | is.na(fuel_dci)) |>
  distinct(combustible, fuel_dci)

if (nrow(bne_dci_unmatched_2008_2024) > 0) {
  print(bne_dci_unmatched_2008_2024)
  stop("Some final consumption fuels were not matched to DCI factors.")
}

## 3. Carbon intensity ----

ci_year_2008_2024 <- bne_dci_final_2008_2024 |>
  group_by(año) |>
  summarise(
    total_energy_tj = sum(tj, na.rm = TRUE),
    total_emissions = sum(tj * fe_CO2_tCO2_TJ, na.rm = TRUE),
    CI_t = total_emissions / total_energy_tj,
    .groups = "drop"
  )

CI_coal_2008_2024 <- bne_dci_factors_2008_2024 |>
  filter(fuel_dci == "Coal") |>
  pull(fe_CO2_tCO2_TJ)

## 4. DCI estimation ----

dci_2008_2024 <- ci_year_2008_2024 |>
  mutate(
    DCI = 1 - (CI_t / CI_coal_2008_2024)
  )

## 5. DCI decomposition ----

energy_shares_2008_2024 <- bne_dci_final_2008_2024 |>
  mutate(
    is_zero_direct_carbon = fuel_dci %in% c(
      "Non-carbon",
      "Biomass",
      "Biogas",
      "Black Liquor"
    )
  ) |>
  group_by(año) |>
  summarise(
    total_energy_tj = sum(tj, na.rm = TRUE),
    zero_direct_carbon_energy_tj = sum(tj[is_zero_direct_carbon], na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    DCINC = zero_direct_carbon_energy_tj / total_energy_tj
  )

dci_decomposed_2008_2024 <- dci_2008_2024 |>
  left_join(energy_shares_2008_2024, by = "año") |>
  mutate(
    DCIOG = DCI - DCINC
  )

### Robust check: DCI = DCIOG + DCINC
check_dci_decomposition_2008_2024 <- max(
  abs(
    dci_decomposed_2008_2024$DCI -
      (dci_decomposed_2008_2024$DCIOG + dci_decomposed_2008_2024$DCINC)
  ),
  na.rm = TRUE
)

if (check_dci_decomposition_2008_2024 > 1e-10) {
  stop("DCI decomposition does not hold within tolerance.")
}

### Check: DCI range
if (any(dci_2008_2024$DCI < -0.05 | dci_2008_2024$DCI > 1.05, na.rm = TRUE)) {
  print(dci_2008_2024)
  warning("Some DCI values are outside the expected range. Check fuel factors and totals.")
}

dci_long_2008_2024 <- dci_decomposed_2008_2024 |>
  select(año, DCI, DCIOG, DCINC) |>
  pivot_longer(
    cols = c(DCI, DCIOG, DCINC),
    names_to = "indicator",
    values_to = "value"
  )

dci_long_2008_2024

# VI Final checks ----
## 1. General checks ----

bne_2008_2024_cleaned |>
  count(item) |>
  arrange(item) |>
  print(n = Inf)

energy_consumption_final_2008_2024 |>
  count(sector_consumo, categoria) |>
  arrange(sector_consumo, categoria) |>
  print(n = Inf)

energy_consumption_final_2008_2024 |>
  count(fuel, combustible) |>
  arrange(fuel, combustible) |>
  print(n = Inf)

## 2. Year coverage checks ----

year_check_2008_2024 <- tibble(
  año = 2008:2024
) |>
  left_join(
    bne_2008_2024_cleaned |>
      count(año, name = "n_rows"),
    by = "año"
  )

if (any(is.na(year_check_2008_2024$n_rows))) {
  print(year_check_2008_2024)
  stop("Some years are missing from bne_2008_2024_cleaned.")
}

# VII Save outputs ----
## 1. Full cleaned data ----

save(bne_2008_2024_cleaned, file = here("Datasets", "clean", "bne_2008_2024_cleaned.RData"))

write_xlsx(
  bne_2008_2024_cleaned,
  here("Datasets", "clean", "bne_2008_2024_cleaned.xlsx")
)

## 2. Core subsets ----

saveRDS(energy_supply_2008_2024, here("Datasets", "clean", "energy_supply_2008_2024.rds"))
saveRDS(energy_supply_components_2008_2024, here("Datasets", "clean", "energy_supply_components_2008_2024.rds"))
saveRDS(energy_primary_supply_2008_2024, here("Datasets", "clean", "energy_primary_supply_2008_2024.rds"))
saveRDS(energy_primary_production_2008_2024, here("Datasets", "clean", "energy_primary_production_2008_2024.rds"))
saveRDS(energy_primary_import_2008_2024, here("Datasets", "clean", "energy_primary_import_2008_2024.rds"))
saveRDS(energy_consumption_total_2008_2024, here("Datasets", "clean", "energy_consumption_total_2008_2024.rds"))
saveRDS(energy_consumption_final_2008_2024, here("Datasets", "clean", "energy_consumption_final_2008_2024.rds"))
saveRDS(energy_consumption_transformation_2008_2024, here("Datasets", "clean", "energy_consumption_transformation_2008_2024.rds"))

## 3. Summary tables ----

saveRDS(bne_year_item_2008_2024, here("Datasets", "clean", "bne_year_item_2008_2024.rds"))
saveRDS(bne_year_category_2008_2024, here("Datasets", "clean", "bne_year_category_2008_2024.rds"))
saveRDS(bne_year_section_2008_2024, here("Datasets", "clean", "bne_year_section_2008_2024.rds"))
saveRDS(bne_year_fuel_2008_2024, here("Datasets", "clean", "bne_year_fuel_2008_2024.rds"))
saveRDS(consumption_final_year_sector_2008_2024, here("Datasets", "clean", "consumption_final_year_sector_2008_2024.rds"))
saveRDS(consumption_final_year_fuel_2008_2024, here("Datasets", "clean", "consumption_final_year_fuel_2008_2024.rds"))
saveRDS(consumption_final_year_sector_fuel_2008_2024, here("Datasets", "clean", "consumption_final_year_sector_fuel_2008_2024.rds"))
saveRDS(transformation_year_category_2008_2024, here("Datasets", "clean", "transformation_year_category_2008_2024.rds"))
saveRDS(primary_supply_year_fuel_2008_2024, here("Datasets", "clean", "primary_supply_year_fuel_2008_2024.rds"))
saveRDS(primary_import_year_fuel_2008_2024, here("Datasets", "clean", "primary_import_year_fuel_2008_2024.rds"))
saveRDS(primary_production_year_fuel_2008_2024, here("Datasets", "clean", "primary_production_year_fuel_2008_2024.rds"))

write_xlsx(
  list(
    "Year - Item" = bne_year_item_2008_2024,
    "Year - Category" = bne_year_category_2008_2024,
    "Year - Section" = bne_year_section_2008_2024,
    "Year - Fuel" = bne_year_fuel_2008_2024,
    "Final Consumption - Sector" = consumption_final_year_sector_2008_2024,
    "Final Consumption - Fuel" = consumption_final_year_fuel_2008_2024,
    "Final Consumption - Sector x Fuel" = consumption_final_year_sector_fuel_2008_2024,
    "Transformation - Category" = transformation_year_category_2008_2024,
    "Primary Supply - Fuel" = primary_supply_year_fuel_2008_2024,
    "Primary Imports - Fuel" = primary_import_year_fuel_2008_2024,
    "Primary Production - Fuel" = primary_production_year_fuel_2008_2024
  ),
  here("Datasets", "clean", "bne_summary_tables_2008_2024.xlsx")
)

## 4. DCI outputs ----

saveRDS(bne_dci_factors_2008_2024, here("Datasets", "clean", "bne_dci_factors_2008_2024.rds"))
saveRDS(bne_dci_final_2008_2024, here("Datasets", "clean", "bne_dci_final_2008_2024.rds"))
saveRDS(ci_year_2008_2024, here("Datasets", "clean", "ci_year_2008_2024.rds"))
saveRDS(dci_2008_2024, here("Datasets", "clean", "dci_2008_2024.rds"))
saveRDS(dci_decomposed_2008_2024, here("Datasets", "clean", "dci_decomposed_2008_2024.rds"))
saveRDS(dci_long_2008_2024, here("Datasets", "clean", "dci_long_2008_2024.rds"))

write_xlsx(
  list(
    dci = dci_2008_2024,
    dci_decomposed = dci_decomposed_2008_2024,
    dci_long = dci_long_2008_2024,
    ci_year = ci_year_2008_2024,
    dci_data = bne_dci_final_2008_2024
  ),
  here("Datasets", "clean", "dci_2008_2024.xlsx")
)

#  Hourly gen cleaning ----
# I Cleaning ----

## 1. Loading ----
hourly_files <- c(
  "gen_horaria_00_15.xlsx",
  "gen_horaria_16_19.xlsx",
  "gen_horaria_20_23.xlsx",
  "gen_horaria_2024.xlsx"
)

hourly_gen_dfs <-
  hourly_files |>
  set_names(
    c(
      "hourly_gen_2000_2015",
      "hourly_gen_2016_2019",
      "hourly_gen_2020_2023",
      "hourly_gen_2024"
    )
  ) |>
  map(
    ~ if (.x == "gen_horaria_20_23.xlsx") {
      bind_rows(
        read_excel(
          here("Datasets", .x),
          sheet = 1
        ),
        read_excel(
          here("Datasets", .x),
          sheet = 2
        )
      )
    } else {
      read_excel(
        here("Datasets", .x)
      )
    }
  )

# 2. cleaning 2024 date column ----
purrr::map(hourly_gen_dfs, ~ class(.x$date))

head(hourly_gen_dfs$hourly_gen_2024$date, 20)

hourly_gen_dfs$hourly_gen_2024 <- hourly_gen_dfs$hourly_gen_2024 |>
  mutate(
    date = str_replace_all(
      date,
      c(
        "ene" = "01",
        "feb" = "02",
        "mar" = "03",
        "abr" = "04",
        "may" = "05",
        "jun" = "06",
        "jul" = "07",
        "ago" = "08",
        "sep" = "09",
        "oct" = "10",
        "nov" = "11",
        "dic" = "12"
      )
    ),
    date = paste0(date, "-2024"),
    date = lubridate::dmy(date)
  )

# 3. Merge full df ----
hourly_gen_2000_2024 <- bind_rows(hourly_gen_dfs)

glimpse(hourly_gen_2000_2024)

hourly_gen_2000_2024 |>
  mutate(year = lubridate::year(date)) |>
  group_by(year) |>
  summarise(
    first_date = min(date),
    last_date  = max(date),
    n_days = n_distinct(date),
    .groups = "drop"
  ) |>
  print(n = Inf)

hourly_gen_2000_2024 <- hourly_gen_2000_2024 |> 
  select(-h25)

save(hourly_gen_2000_2024, file = here("Datasets", "clean", "hourly_gen_2000_2024.RData"))

# II Subsets ----

# 1. Further cleaning and recoding ----
hourly_generation_long_2000_2024 <-
  hourly_gen_2000_2024 |>
  pivot_longer(
    h1:h24,
    names_to = "hour",
    values_to = "mw"
  ) |>
  mutate(
    hour = readr::parse_number(hour),
    year = lubridate::year(date),
    month = lubridate::month(date),
    weekday = lubridate::wday(date, label = TRUE)
  )

hourly_generation_long_2000_2024 <- hourly_generation_long_2000_2024 |>
  mutate(
    energy_type = case_when(
      energy_type == "(en blanco)" ~ NA_character_,
      energy_type == "Bess" ~ "BESS",
      energy_type == "Eólica" ~ "Wind power",
      energy_type == "Geotérmica" ~ "Geothermal",
      energy_type == "Hidráulica" ~ "Hydropower",
      energy_type == "Solar" ~ "Solar",
      energy_type == "Termosolar" ~ "Concentrated solar",
      energy_type == "Térmica" ~ "Thermal",
      TRUE ~ energy_type
    )
  )

hourly_generation_long_2000_2024 <- hourly_generation_long_2000_2024 |>
  mutate(
    energy_type = case_when(
      energy_type == "Hydropower" ~ "Hydropower",
      energy_type == "Wind power" ~ "Wind power",
      energy_type %in% c("Solar", "Concentrated solar") ~ "Solar",
      energy_type == "Thermal" ~ "Thermal",
      energy_type == "Geothermal" ~ "Geothermal",
      energy_type == "BESS" ~ "BESS",
      TRUE ~ energy_type
    )
  )

save(hourly_generation_long_2000_2024, file = here("Datasets", "clean", "hourly_generation_long_2000_2024.RData"))
