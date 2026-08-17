# Config ----

pacman::p_load(
  tidyverse,
  ggplot2,
  scales,
  forcats,
  readxl,
  here,
  readr,
  stringr,
  skimr,
  patchwork,
  cowplot,
  ggrepel,
  writexl,
  paletteer,
  ggstream
)

options(scipen = 999)

## 0. Paths ----

clean_dir <- here::here("Datasets", "clean")
output_dir <- here::here("Outputs")
plots_dir <- here::here("Outputs", "figures")
tables_dir <- here::here("Outputs", "tables")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

# I Data ----

## 1. Clean BNE outputs ----

load(here("Datasets", "clean", "bne_2008_2024_cleaned.RData"))

energy_supply_2008_2024 <- readRDS(here("Datasets", "clean", "energy_supply_2008_2024.rds"))
energy_primary_supply_2008_2024 <- readRDS(here("Datasets", "clean", "energy_primary_supply_2008_2024.rds"))
energy_primary_production_2008_2024 <- readRDS(here("Datasets", "clean", "energy_primary_production_2008_2024.rds"))
energy_primary_import_2008_2024 <- readRDS(here("Datasets", "clean", "energy_primary_import_2008_2024.rds"))

energy_consumption_total_2008_2024 <- readRDS(here("Datasets", "clean", "energy_consumption_total_2008_2024.rds"))
energy_consumption_final_2008_2024 <- readRDS(here("Datasets", "clean", "energy_consumption_final_2008_2024.rds"))
energy_consumption_transformation_2008_2024 <- readRDS(here("Datasets", "clean", "energy_consumption_transformation_2008_2024.rds"))

bne_year_item_2008_2024 <- readRDS(here("Datasets", "clean", "bne_year_item_2008_2024.rds"))
bne_year_category_2008_2024 <- readRDS(here("Datasets", "clean", "bne_year_category_2008_2024.rds"))
bne_year_section_2008_2024 <- readRDS(here("Datasets", "clean", "bne_year_section_2008_2024.rds"))
bne_year_fuel_2008_2024 <- readRDS(here("Datasets", "clean", "bne_year_fuel_2008_2024.rds"))

consumption_final_year_sector_2008_2024 <- readRDS(here("Datasets", "clean", "consumption_final_year_sector_2008_2024.rds"))
consumption_final_year_fuel_2008_2024 <- readRDS(here("Datasets", "clean", "consumption_final_year_fuel_2008_2024.rds"))
consumption_final_year_sector_fuel_2008_2024 <- readRDS(here("Datasets", "clean", "consumption_final_year_sector_fuel_2008_2024.rds"))

transformation_year_category_2008_2024 <- readRDS(here("Datasets", "clean", "transformation_year_category_2008_2024.rds"))
primary_supply_year_fuel_2008_2024 <- readRDS(here("Datasets", "clean", "primary_supply_year_fuel_2008_2024.rds"))
primary_import_year_fuel_2008_2024 <- readRDS(here("Datasets", "clean", "primary_import_year_fuel_2008_2024.rds"))
primary_production_year_fuel_2008_2024 <- readRDS(here("Datasets", "clean", "primary_production_year_fuel_2008_2024.rds"))

bne_dci_factors_2008_2024 <- readRDS(here("Datasets", "clean", "bne_dci_factors_2008_2024.rds"))
bne_dci_final_2008_2024 <- readRDS(here("Datasets", "clean", "bne_dci_final_2008_2024.rds"))
ci_year_2008_2024 <- readRDS(here("Datasets", "clean", "ci_year_2008_2024.rds"))
dci_2008_2024 <- readRDS(here("Datasets", "clean", "dci_2008_2024.rds"))
dci_decomposed_2008_2024 <- readRDS(here("Datasets", "clean", "dci_decomposed_2008_2024.rds"))
dci_long_2008_2024 <- readRDS(here("Datasets", "clean", "dci_long_2008_2024.rds"))

## 2. External electricity datasets ----
installed_cap <- read_excel(
  here("Datasets", "full_energy_dfs_2026_02_10.xlsx"),
  sheet = "Installed Capacity (MW)"
)

load(here("Datasets", "clean", "hourly_generation_long_2000_2024.RData"))

# II Data handling ----

## 1. Primary production by  fuel ----
primary_production_fuel_long_2008_2024 <- primary_production_year_fuel_2008_2024 |>
  group_by(año, fuel) |>
  summarise(
    tcal = sum(tcal, na.rm = TRUE),
    .groups = "drop"
  ) |>
  rename(
    year = año,
    energy_type = fuel
  ) |>
  arrange(year, energy_type)

primary_production_fuel_share_2008_2024 <- primary_production_fuel_long_2008_2024 |>
  group_by(year) |>
  mutate(
    share = tcal / sum(tcal, na.rm = TRUE)
  ) |>
  ungroup()

primary_production_fuel_share_2008_2024|>
  count(energy_type) |>
  arrange(energy_type) |>
  print(n = Inf)


### by original combustible 
primary_production_combustible_long_2008_2024 <- primary_production_year_fuel_2008_2024 |>
  group_by(año, combustible) |>
  summarise(
    tcal = sum(tcal, na.rm = TRUE),
    .groups = "drop"
  ) |>
  rename(
    year = año,
    energy_type = combustible
  ) |>
  arrange(year, energy_type)

primary_production_combustible_share_2008_2024 <- primary_production_combustible_long_2008_2024 |>
  group_by(year) |>
  mutate(
    share = tcal / sum(tcal, na.rm = TRUE)
  ) |>
  ungroup()

primary_production_combustible_share_2008_2024|>
  count(energy_type) |>
  arrange(energy_type) |>
  print(n = Inf)

## 2. Installed capacity ----
installed_cap_long <- installed_cap |>
  select(-total) |>
  pivot_longer(
    cols = -year,
    names_to = "energy_type",
    values_to = "mw"
  ) |>
  mutate(
    energy_type = case_when(
      energy_type == "coal" ~ "Coal",
      energy_type == "concentrated_solar_power" ~ "Concentrated solar",
      energy_type == "diesel_oil" ~ "Diesel oil",
      energy_type == "geothermal" ~ "Geothermal",
      energy_type == "hydro" ~ "Hydro",
      energy_type == "natural_gas" ~ "Natural gas",
      energy_type == "other" ~ "Other",
      energy_type == "solar_photovoltaic" ~ "Solar pv",
      energy_type == "wind_power" ~ "Wind",
      TRUE ~ str_to_sentence(str_replace_all(energy_type, "_", " "))
    )
  )

installed_cap_share_2008_2024 <- installed_cap_long |>
  filter(year >= 2008, year <= 2024) |>
  group_by(year) |>
  mutate(
    share = mw / sum(mw, na.rm = TRUE)
  ) |>
  ungroup()

## 4. BNE plot-ready tables ----
final_consumption_fuel_share_2008_2024 <- consumption_final_year_fuel_2008_2024 |>
  group_by(año) |>
  mutate(
    share = tcal / sum(tcal, na.rm = TRUE)
  ) |>
  ungroup()

final_consumption_sector_share_2008_2024 <- consumption_final_year_sector_2008_2024 |>
  group_by(año) |>
  mutate(
    share = tcal / sum(tcal, na.rm = TRUE)
  ) |>
  ungroup()

final_consumption_sector_fuel_share_2008_2024 <- consumption_final_year_sector_fuel_2008_2024 |>
  group_by(año, sector_consumo) |>
  mutate(
    share = tcal / sum(tcal, na.rm = TRUE)
  ) |>
  ungroup()

primary_supply_fuel_share_2008_2024 <- primary_supply_year_fuel_2008_2024 |>
  group_by(año) |>
  mutate(
    share = tcal / sum(tcal, na.rm = TRUE)
  ) |>
  ungroup()

primary_import_main_fuels_2008_2024 <- primary_import_year_fuel_2008_2024 |>
  filter(
    combustible %in% c("Petróleo Crudo", "Gas Natural", "Carbón")
  ) |>
  mutate(
    combustible = recode(
      combustible,
      "Petróleo Crudo" = "Crude oil",
      "Gas Natural" = "Natural gas",
      "Carbón" = "Coal"
    ),
    combustible = factor(combustible, levels = c("Crude oil", "Coal", "Natural gas"))
  )

## 5. DCI plot-ready tables ----
dci_long_2008_2024 <- dci_long_2008_2024 |>
  mutate(
    indicator = factor(
      indicator,
      levels = c("DCI", "DCIOG", "DCINC"),
      labels = c("DCI", "DCIOG", "DCINC")
    )
  )

dci_fuel_contribution_2008_2024 <- bne_dci_final_2008_2024 |>
  group_by(año, fuel_dci) |>
  summarise(
    energy_tj = sum(tj, na.rm = TRUE),
    emissions_tCO2 = sum(tj * fe_CO2_tCO2_TJ, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(año) |>
  mutate(
    energy_share = energy_tj / sum(energy_tj, na.rm = TRUE),
    emissions_share = emissions_tCO2 / sum(emissions_tCO2, na.rm = TRUE)
  ) |>
  ungroup()

dci_sector_contribution_2008_2024 <- bne_dci_final_2008_2024 |>
  group_by(año, sector_consumo) |>
  summarise(
    energy_tj = sum(tj, na.rm = TRUE),
    emissions_tCO2 = sum(tj * fe_CO2_tCO2_TJ, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(año) |>
  mutate(
    energy_share = energy_tj / sum(energy_tj, na.rm = TRUE),
    emissions_share = emissions_tCO2 / sum(emissions_tCO2, na.rm = TRUE)
  ) |>
  ungroup()

# III Checks ----
## 1. Data coverage ----

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
  stop("Some years are missing from the cleaned BNE dataset.")
}

## 2. DCI consistency ----

dci_check_2008_2024 <- max(
  abs(
    dci_decomposed_2008_2024$DCI -
      (dci_decomposed_2008_2024$DCIOG + dci_decomposed_2008_2024$DCINC)
  ),
  na.rm = TRUE
)

if (dci_check_2008_2024 > 1e-10) {
  stop("DCI decomposition does not hold within tolerance.")
}

bne_dci_final_2008_2024 |>
  filter(is.na(fe_CO2_tCO2_TJ) | is.na(fuel_dci)) |>
  distinct(combustible, fuel_dci) |>
  print(n = Inf)

# IV Tables ----
## 1. Main summary tables ----

final_consumption_change_2008_2024 <- consumption_final_year_sector_2008_2024 |>
  group_by(sector_consumo) |>
  arrange(año) |>
  summarise(
    tcal_2008 = tcal[año == 2008][1],
    tcal_2024 = tcal[año == 2024][1],
    absolute_change_tcal = tcal_2024 - tcal_2008,
    percent_change = (tcal_2024 / tcal_2008) - 1,
    .groups = "drop"
  ) |>
  arrange(desc(absolute_change_tcal))

fuel_change_2008_2024 <- consumption_final_year_fuel_2008_2024 |>
  group_by(fuel) |>
  arrange(año) |>
  summarise(
    tcal_2008 = tcal[año == 2008][1],
    tcal_2024 = tcal[año == 2024][1],
    absolute_change_tcal = tcal_2024 - tcal_2008,
    percent_change = (tcal_2024 / tcal_2008) - 1,
    .groups = "drop"
  ) |>
  arrange(desc(absolute_change_tcal))

dci_change_2008_2024 <- dci_decomposed_2008_2024 |>
  arrange(año) |>
  summarise(
    DCI_2008 = DCI[año == 2008][1],
    DCI_2024 = DCI[año == 2024][1],
    DCIOG_2008 = DCIOG[año == 2008][1],
    DCIOG_2024 = DCIOG[año == 2024][1],
    DCINC_2008 = DCINC[año == 2008][1],
    DCINC_2024 = DCINC[año == 2024][1],
    DCI_change = DCI_2024 - DCI_2008,
    DCIOG_change = DCIOG_2024 - DCIOG_2008,
    DCINC_change = DCINC_2024 - DCINC_2008
  )

write_xlsx(
  list(
    "Final consumption sector" = consumption_final_year_sector_2008_2024,
    "Final consumption fuel" = consumption_final_year_fuel_2008_2024,
    "Final sector fuel" = consumption_final_year_sector_fuel_2008_2024,
    "Final sector share" = final_consumption_sector_share_2008_2024,
    "Final fuel share" = final_consumption_fuel_share_2008_2024,
    "Primary supply fuel" = primary_supply_year_fuel_2008_2024,
    "Primary imports fuel" = primary_import_year_fuel_2008_2024,
    "DCI" = dci_2008_2024,
    "DCI decomposed" = dci_decomposed_2008_2024,
    "DCI long" = dci_long_2008_2024,
    "DCI fuel contribution" = dci_fuel_contribution_2008_2024,
    "DCI sector contribution" = dci_sector_contribution_2008_2024,
    "Final sector change" = final_consumption_change_2008_2024,
    "Final fuel change" = fuel_change_2008_2024,
    "DCI change" = dci_change_2008_2024
  ),
  here("Outputs", "tables", "analysis_tables_2008_2024.xlsx")
)

# V Figures ----

## 1. Generation ----
## Primary production share 
plot_primary_production_share_2008_2024 <- primary_production_fuel_share_2008_2024 |>
  ggplot(aes(x = year, y = share, fill = energy_type)) +
  geom_col(
    width = 0.82,
    color = "white",
    linewidth = 0.15
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_x_continuous(
    breaks = 2008:2024,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_fill_manual(
    values = c(
      "Bioenergy"      = "#009E73",
      "Hydro"          = "#0072B2",
      "Natural gas"    = "#56B4E9",
      "Coal and Coke"  = "#4D4D4D",
      "Oil products"   = "#E69F00",
      "Solar"          = "#F0E442",
      "Wind"           = "#CC79A7",
      "Geothermal"     = "#D55E00"
    )
  ) +
  labs(
    x = NULL,
    y = "Share of primary energy production",
    fill = NULL
  ) +
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.1, "lines")
  )

plot_primary_production_share_2008_2024

ggsave(
  filename = here("Outputs", "figures", "01_primary_production_share_2008_2024.jpg"),
  plot = plot_primary_production_share_2008_2024,
  width = 10,
  height = 6,
  dpi = 300
)

plot_primary_production_level_2008_2024 <- primary_production_fuel_long_2008_2024 |>
  filter(year >= 2008, year <= 2024) |>
  mutate(
    energy_type = fct_relevel(
      energy_type,
      "Bioenergy",
      "Hydro",
      "Natural gas",
      "Coal and Coke",
      "Solar",
      "Wind",
      "Geothermal",
      "Oil products"
    )
  ) |>
  ggplot(aes(x = year, y = tcal, fill = energy_type)) +
  geom_col(width = 0.82, color = "white", linewidth = 0.15) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.04))
  ) +
  scale_x_continuous(
    breaks = 2008:2024,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_fill_manual(
    values = c(
      "Bioenergy"      = "#009E73",
      "Hydro"          = "#0072B2",
      "Natural gas"    = "#56B4E9",
      "Coal and Coke"  = "#4D4D4D",
      "Oil products"   = "#E69F00",
      "Solar"          = "#F0E442",
      "Wind"           = "#CC79A7",
      "Geothermal"     = "#D55E00"
    )
  ) +
  labs(
    x = NULL,
    y = "Primary energy production (Tcal)",
    fill = NULL
  ) +
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.1, "lines")
  )

plot_primary_production_level_2008_2024

ggsave(
  filename = here("Outputs", "figures", "02_primary_production_level_2008_2024.jpg"),
  plot = plot_primary_production_level_2008_2024,
  width = 10,
  height = 6,
  dpi = 300
)

## 2. Installed capacity ----
plot_installed_capacity_share_2008_2024 <- installed_cap_share_2008_2024 |>
  mutate(
    energy_type = fct_relevel(
      energy_type,
      "Coal",
      "Diesel oil",
      "Natural gas",
      "Hydro",
      "Wind",
      "Solar pv",
      "Concentrated solar",
      "Geothermal",
      "Other"
    )
  ) |>
  ggplot(aes(x = year, y = share, fill = energy_type)) +
  geom_col(width = 0.82, color = "white", linewidth = 0.15) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_x_continuous(
    breaks = 2008:2024,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_fill_manual(
    values = c(
      "Coal"              = "#4D4D4D",
      "Diesel oil"        = "#B5651D",
      "Natural gas"       = "#4C78A8",
      "Hydro"             = "#72B7B2",
      "Wind"              = "#A0CBE8",
      "Solar pv"          = "#F58518",
      "Concentrated solar"= "#E45756",
      "Geothermal"        = "#54A24B",
      "Other"             = "#9E9E9E"
    )
  ) +
  labs(
    x = NULL,
    y = "Share of installed capacity",
    fill = NULL
  ) +
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.1, "lines")
  )

plot_installed_capacity_share_2008_2024

ggsave(
  filename = here("Outputs", "figures", "03_installed_capacity_share_2008_2024.jpg"),
  plot = plot_installed_capacity_share_2008_2024,
  width = 10,
  height = 6,
  dpi = 300
)

plot_installed_capacity_level_2008_2024 <- installed_cap_long |>
  filter(year >= 2008, year <= 2024) |>
  mutate(
    energy_type = fct_relevel(
      energy_type,
      "Coal",
      "Diesel oil",
      "Natural gas",
      "Hydro",
      "Wind",
      "Solar pv",
      "Concentrated solar",
      "Geothermal",
      "Other"
    )
  ) |>
  ggplot(aes(x = year, y = mw, fill = energy_type)) +
  geom_col(
    width = 0.82,
    color = "white",
    linewidth = 0.15
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  coord_cartesian(
    ylim = c(0, 40000)
  ) +
  scale_x_continuous(
    breaks = 2008:2024,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_fill_manual(
    values = c(
      "Coal"               = "#4D4D4D",
      "Diesel oil"         = "#B5651D",
      "Natural gas"        = "#4C78A8",
      "Hydro"              = "#72B7B2",
      "Wind"               = "#A0CBE8",
      "Solar pv"           = "#F58518",
      "Concentrated solar" = "#E45756",
      "Geothermal"         = "#54A24B",
      "Other"              = "#9E9E9E"
    )
  ) +
  labs(
    x = NULL,
    y = "Installed capacity (MW)",
    fill = NULL
  ) +
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.1, "lines")
  )

plot_installed_capacity_level_2008_2024

ggsave(
  filename = here("Outputs", "figures", "04_installed_capacity_level_2008_2024.jpg"),
  plot = plot_installed_capacity_level_2008_2024,
  width = 10,
  height = 6,
  dpi = 300
)

## 3. Primary supply ----
primary_supply_year_fuel_plot <- primary_supply_year_fuel_2008_2024 |>
  group_by(año, fuel) |>
  summarise(
    tcal = sum(tcal, na.rm = TRUE),
    .groups = "drop"
  )

plot_primary_supply_fuel_2008_2024 <- primary_supply_year_fuel_plot |>
  ggplot(aes(x = año, y = tcal, fill = fuel)) +
  geom_col(
    width = 0.82,
    linewidth = 0.15
  ) +
  scale_y_continuous(
    limits = c(0, 450000),
    breaks = seq(0, 450000, by = 50000),
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_x_continuous(
    breaks = 2008:2024,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_fill_manual(
    values = c(
      "Oil products" = "#4D4D4D",
      "Natural gas"  = "#4C78A8",
      "Coal and Coke"= "#B5651D",
      "Bioenergy"    = "#6BAA75",
      "Hydro"        = "#72B7B2",
      "Wind"         = "#A0CBE8",
      "Solar"        = "#F2CF5B",
      "Geothermal"   = "#8C6D31"
    )
  ) +
  labs(
    x = NULL,
    y = "Primary energy supply (Tcal)",
    fill = NULL
  ) +
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.1, "lines")
  )

plot_primary_supply_fuel_2008_2024

ggsave(
  filename = here("Outputs", "figures", "05_primary_supply_fuel_2008_2024.jpg"),
  plot = plot_primary_supply_fuel_2008_2024,
  width = 10,
  height = 6,
  dpi = 300
)

primary_supply_fuel_share_2008_2024 <- primary_supply_year_fuel_2008_2024 |>
  group_by(año, fuel) |>
  summarise(
    tcal = sum(tcal, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(año) |>
  mutate(
    annual_total_tcal = sum(tcal, na.rm = TRUE),
    share = tcal / annual_total_tcal
  ) |>
  ungroup()

plot_primary_supply_share_2008_2024 <- primary_supply_fuel_share_2008_2024 |>
  mutate(
    fuel = fct_relevel(
      fuel,
      "Oil products",
      "Natural gas",
      "Coal and Coke",
      "Bioenergy",
      "Hydro",
      "Wind",
      "Solar",
      "Geothermal"
    )
  ) |>
  ggplot(aes(x = año, y = share, fill = fuel)) +
  geom_col(
    width = 0.82,
    linewidth = 0.15
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_x_continuous(
    breaks = 2008:2024,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_fill_manual(
    values = c(
      "Oil products" = "#4D4D4D",
      "Natural gas"  = "#4C78A8",
      "Coal and Coke"= "#B5651D",
      "Bioenergy"    = "#6BAA75",
      "Hydro"        = "#72B7B2",
      "Wind"         = "#A0CBE8",
      "Solar"        = "#F2CF5B",
      "Geothermal"   = "#8C6D31"
    )
  ) +
  labs(
    x = NULL,
    y = "Share of primary energy supply",
    fill = NULL
  ) +
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.1, "lines")
  )

plot_primary_supply_share_2008_2024

ggsave(
  filename = here("Outputs", "figures", "06_primary_supply_share_2008_2024.jpg"),
  plot = plot_primary_supply_share_2008_2024,
  width = 10,
  height = 6,
  dpi = 300
)

plot_primary_import_main_fuels_2008_2024 <- primary_import_main_fuels_2008_2024 |>
  mutate(
    combustible = factor(combustible, levels = c("Crude oil", "Coal", "Natural gas"))
  ) |>
  ggplot(aes(x = año, y = tcal, fill = combustible)) +
  geom_col(width = 0.82, color = "white", linewidth = 0.15, show.legend = FALSE) +
  facet_wrap(~combustible, nrow = 1) +
  scale_y_continuous(
    limits = c(0, 110000),
    breaks = seq(0, 110000, by = 10000),
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_x_continuous(
    breaks = c(2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_fill_manual(
    values = c(
      "Crude oil"   = "#4D4D4D",
      "Coal"        = "#B5651D",
      "Natural gas" = "#4C78A8"
    )
  ) +
  labs(
    x = NULL,
    y = "Imports (Tcal)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "none"
  )

plot_primary_import_main_fuels_2008_2024

ggsave(
  filename = here("Outputs", "figures", "07_primary_import_main_fuels_2008_2024.jpg"),
  plot = plot_primary_import_main_fuels_2008_2024,
  width = 10,
  height = 6,
  dpi = 300
)

## 4. Final consumption ----
plot_final_consumption_fuel_2008_2024 <- consumption_final_year_fuel_2008_2024 |>
  ggplot(aes(x = año, y = tcal, fill = fuel)) +
  geom_col(
    width = 0.82,
    color = "white",
    linewidth = 0.15
  ) +
  scale_y_continuous(
    limits = c(0, 350000),
    breaks = seq(0, 350000, by = 50000),
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_x_continuous(
    breaks = 2008:2024,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_fill_manual(
    values = c(
      "Oil products" = "#4D4D4D",
      "Natural gas"  = "#4C78A8",
      "Coal and Coke"= "#B5651D",
      "Electricity"  = "#E45756",
      "Bioenergy"    = "#6BAA75"
    )
  )  +
  labs(
    x = NULL,
    y = "Final consumption (Tcal)",
    fill = NULL
  ) +
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.1, "lines")
  )

plot_final_consumption_fuel_2008_2024

ggsave(
  filename = here("Outputs", "figures", "08_final_consumption_fuel_2008_2024.jpg"),
  plot = plot_final_consumption_fuel_2008_2024,
  width = 10,
  height = 6,
  dpi = 300
)

plot_final_consumption_fuel_share_2008_2024 <- final_consumption_fuel_share_2008_2024 |>
  ggplot(aes(x = año, y = share, fill = fuel)) +
  geom_col(
    width = 0.82,
    color = "white",
    linewidth = 0.15
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_x_continuous(
    breaks = 2008:2024,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_fill_manual(
    values = c(
      "Oil products" = "#4D4D4D",
      "Natural gas"  = "#4C78A8",
      "Coal and Coke"= "#B5651D",
      "Electricity"  = "#E45756",
      "Bioenergy"    = "#6BAA75"
    )
  ) +
  labs(
    x = NULL,
    y = "Share of final energy consumption",
    fill = NULL
  ) +
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.1, "lines")
  )

plot_final_consumption_fuel_share_2008_2024

ggsave(
  filename = here("Outputs", "figures", "09_final_consumption_fuel_share_2008_2024.jpg"),
  plot = plot_final_consumption_fuel_share_2008_2024,
  width = 10,
  height = 6,
  dpi = 300
)

plot_final_consumption_sector_2008_2024 <- consumption_final_year_sector_2008_2024 |>
  mutate(
    sector_consumo = fct_relevel(
      sector_consumo,
      "Sector Energético: Auto Consumo",
      "Sector Comercial, Público y Residencial",
      "Sector Industrial y Minero",
      "Sector Transporte"
    )
  ) |>
  ggplot(aes(x = año, y = tcal, fill = sector_consumo)) +
  geom_col(
    width = 0.82,
    color = "white",
    linewidth = 0.15
  ) +
  scale_y_continuous(
    limits = c(0, 350000),
    breaks = seq(0, 350000, by = 50000),
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_x_continuous(
    breaks = 2008:2024,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_fill_manual(
    values = c(
      "Sector Transporte" = "#4D4D4D",                      # charcoal (analogous to Oil products)
      "Sector Industrial y Minero" = "#4C78A8",            # blue (analogous to Natural gas)
      "Sector Comercial, Público y Residencial" = "#E45756", # coral (analogous to Electricity)
      "Sector Energético: Auto Consumo" = "#6BAA75"        # green (analogous to Bioenergy)
    ),
    labels = c(
      "Energy sector (Own use)",
      "Commercial, Public and Residential",
      "Industry and Mining",
      "Transport"
    )
  ) +
  labs(
    x = NULL,
    y = "Final energy consumption (Tcal)",
    fill = NULL
  ) +
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.2, "lines")
  )

plot_final_consumption_sector_2008_2024

ggsave(
  filename = here("Outputs", "figures", "10_final_consumption_sector_2008_2024.jpg"),
  plot = plot_final_consumption_sector_2008_2024,
  width = 10,
  height = 6,
  dpi = 300
)

plot_final_consumption_sector_share_2008_2024 <- final_consumption_sector_share_2008_2024 |>
  mutate(
    sector_consumo = fct_relevel(
      sector_consumo,
      "Sector Energético: Auto Consumo",
      "Sector Comercial, Público y Residencial",
      "Sector Industrial y Minero",
      "Sector Transporte"
    )
  ) |>
  ggplot(aes(x = año, y = share, fill = sector_consumo)) +
  geom_col(
    width = 0.82,
    color = "white",
    linewidth = 0.15
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_x_continuous(
    breaks = 2008:2024,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_fill_manual(
    values = c(
      "Sector Energético: Auto Consumo" = "#6BAA75",
      "Sector Comercial, Público y Residencial" = "#E45756",
      "Sector Industrial y Minero" = "#4C78A8",
      "Sector Transporte" = "#4D4D4D"
    ),
    labels = c(
      "Energy sector (Own use)",
      "Commercial, Public and Residential",
      "Industry and Mining",
      "Transport"
    )
  ) +
  labs(
    x = NULL,
    y = "Share of final energy consumption",
    fill = NULL
  ) +
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.2, "lines")
  )

plot_final_consumption_sector_share_2008_2024

ggsave(
  filename = here("Outputs", "figures", "11_final_consumption_sector_share_2008_2024.jpg"),
  plot = plot_final_consumption_sector_share_2008_2024,
  width = 10,
  height = 6,
  dpi = 300
)

plot_final_consumption_sector_fuel_2008_2024 <- consumption_final_year_sector_fuel_2008_2024 |>
  filter(sector_consumo != "Sector Energético: Auto Consumo") |>
  mutate(
    sector_consumo = fct_relevel(
      sector_consumo,
      "Sector Industrial y Minero",
      "Sector Transporte",
      "Sector Comercial, Público y Residencial",
    ),
    fuel = fct_relevel(
      fuel,
      "Oil products",
      "Natural gas",
      "Coal and Coke",
      "Electricity",
      "Bioenergy"
    )
  ) |>
  ggplot(aes(x = año, y = tcal, fill = fuel)) +
  geom_col(
    width = 0.82,
    color = "white",
    linewidth = 0.15
  ) +
  facet_wrap(
    ~sector_consumo,
    scales = "free_y",
    ncol = 3
  ) +
  scale_y_continuous(
    limits = c(0, 130000),
    breaks = seq(0, 130000, by = 10000),
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_x_continuous(
    breaks = 2008:2024,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_fill_manual(
    values = c(
      "Oil products" = "#4D4D4D",
      "Natural gas"  = "#4C78A8",
      "Coal and Coke"= "#B5651D",
      "Electricity"  = "#72B7B2",
      "Bioenergy"    = "#6BAA75"
    )
  ) +
  labs(
    x = NULL,
    y = "Final energy consumption (Tcal)",
    fill = NULL
  ) +
  guides(
    fill = guide_legend(
      nrow = 1,
      byrow = TRUE
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.2, "lines")
  )

plot_final_consumption_sector_fuel_2008_2024

ggsave(
  filename = here("Outputs", "figures", "12_final_consumption_sector_fuel_2008_2024.jpg"),
  plot = plot_final_consumption_sector_fuel_2008_2024,
  width = 12,
  height = 8,
  dpi = 300
)

## 6. DCI ----
plot_dci_2008_2024 <- dci_2008_2024 |>
  ggplot(aes(x = año, y = DCI)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_x_continuous(breaks = 2008:2024) +
  labs(
    x = "Year",
    y = "DCI"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_dci_2008_2024

ggsave(
  filename = here("Outputs", "figures", "14_dci_2008_2024.jpg"),
  plot = plot_dci_2008_2024,
  width = 9,
  height = 5,
  dpi = 300
)

### decomp
labels_dci_2008_2024 <- dci_long_2008_2024 |>
  group_by(indicator) |>
  slice_max(año, n = 1) |>
  ungroup()

plot_dci_decomposition_2008_2024 <- dci_long_2008_2024 |>
  mutate(
    indicator = factor(
      indicator,
      levels = c("DCI", "DCIOG", "DCINC")
    )
  ) |>
  ggplot(aes(año, value, color = indicator)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2.6) +
  geom_text(
    data = labels_dci_2008_2024,
    aes(label = indicator),
    hjust = -0.15,
    fontface = "bold",
    size = 4.5,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "DCI"   = "#F46D65",
      "DCIOG" = "#2FB84D",
      "DCINC" = "#5D8FEA"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_x_continuous(
    breaks = seq(2008, 2024, by = 2),
    expand = expansion(mult = c(0.01, 0.08))
  ) +
  labs(
    x = NULL,
    y = "Index value"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.margin = margin(10, 45, 10, 10)
  )

plot_dci_decomposition_2008_2024

ggsave(
  filename = here("Outputs", "figures", "15_dci_decomposition_2008_2024.jpg"),
  plot = plot_dci_decomposition_2008_2024,
  width = 9,
  height = 5,
  dpi = 300
)


## 7. Hourly generation ----
hourly_generation_long_2000_2024 |> 
  count(energy_type) 

hourly_generation_long_2000_2024 |> 
  glimpse()

hourly_generation_oct_2000_2024 <- hourly_generation_long_2000_2024 |>
  filter(
    month == 10,
    !is.na(energy_type)
  ) |>
  group_by(
    year,
    date,
    hour,
    energy_type
  ) |>
  summarise(
    total_mw = sum(mw, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(
    year,
    hour,
    energy_type
  ) |>
  summarise(
    mean_mw = mean(total_mw, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    energy_type = factor(
      energy_type,
      levels = c(
        "Wind power",
        "Thermal",
        "Solar",
        "Hydropower",
        "Geothermal",
        "BESS"
      )
    )
  ) |>
  arrange(
    year,
    hour,
    energy_type
  )

hourly_generation_oct_2000_2024 |>
  count(energy_type)

plot_hourly_generation_2014 <- hourly_generation_oct_2000_2024 |>
  filter(year == 2014) |>
  ggplot(aes(x = hour, y = mean_mw, fill = energy_type)) +
  geom_area(
    position = "stack",
    linewidth = 0.15,
    color = "grey70",
    alpha = 0.95
  ) +
  scale_x_continuous(
    breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24),
    labels = c(
      "0am", "3am", "6am", "9am",
      "noon", "3pm", "6pm", "9pm", "0am"
    ),
    limits = c(0, 24),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 11000),
    breaks = seq(0, 11000, by = 1000),
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_fill_manual(
    values = c(
      "Thermal"     = "#4C78A8",
      "Hydropower"  = "#72B7B2",
      "Wind power"  = "#A0CBE8",
      "Solar"       = "#F2CF5B",
      "Geothermal"  = "#8C6D31",
      "BESS"        = "#E45756"
    )
  ) +
  labs(
    title = "October 2014",
    x = NULL,
    y = "Mean generation (MW)",
    fill = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 10
    ),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

plot_hourly_generation_2024 <- hourly_generation_oct_2000_2024 |>
  filter(year == 2024) |>
  ggplot(aes(x = hour, y = mean_mw, fill = energy_type)) +
  geom_area(
    position = "stack",
    linewidth = 0.15,
    color = "grey70",
    alpha = 0.95
  ) +
  scale_x_continuous(
    breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24),
    labels = c(
      "0am", "3am", "6am", "9am",
      "noon", "3pm", "6pm", "9pm", "0am"
    ),
    limits = c(0, 24),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 11000),
    breaks = seq(0, 11000, by = 1000),
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_fill_manual(
    values = c(
      "Thermal"     = "#4C78A8",
      "Hydropower"  = "#72B7B2",
      "Wind power"  = "#A0CBE8",
      "Solar"       = "#F2CF5B",
      "Geothermal"  = "#8C6D31",
      "BESS"        = "#E45756"
    )
  ) +
  labs(
    title = "October 2024",
    x = NULL,
    y = NULL,
    fill = "Energy type"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    axis.title.y = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 10
    ),
    legend.position = "right",
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

plot_hourly_generation_2014_2024 <- 
  plot_hourly_generation_2014 + plot_hourly_generation_2024

plot_hourly_generation_2014_2024

ggsave(
  filename = here("Outputs", "figures", "17_hourly_generation_2014_vs_2024.jpg"),
  plot = plot_hourly_generation_2014_2024,
  width = 10,
  height = 6,
  dpi = 300
)

