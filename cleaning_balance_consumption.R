rm(list = ls())
options(scipen=999)

# Consumption data cleaning ----
energy_balance_2008_2021 <- read_delim(here("Datasets", "bne_balance_nacional(in).csv"), 
                                       delim = ";", escape_double = FALSE, 
                                       locale = locale(decimal_mark = ","), 
                                       trim_ws = TRUE)

energy_balance_2008_2021 <- energy_balance_2008_2021 |> 
  rename(año = anio) 

energy_consumo_total_2008_2021 <- energy_balance_2008_2021 |> 
  filter(item %in% c("CONSUMO FINAL", "C.TRANSFO.")) 

## Fixing negatives 
energy_consumo_total_2008_2021 <- energy_consumo_total_2008_2021 |> 
  mutate(tcal = abs(tcal))

## Fixing categoria's different spelling through the years 
energy_consumo_total_2008_2021 |> 
  count(categoria) %>%
  arrange(categoria) %>%
  print(n = Inf)

energy_consumo_total_2008_2021 <- energy_consumo_total_2008_2021 |> 
  mutate(
    categoria = case_when(
      categoria == "Centrales Eléctricas: Autoproductores" ~ "Electricidad Autoproducción",
      categoria == "Centrales Eléctricas: Servicio Publico" ~ "Electricidad Servicio Público",
      categoria == "Siderurgia: Altos Hornos" ~ "Siderurgia Altos Hornos",
      categoria == "Siderurgia: Hornos de Coque" ~ "Siderurgia Hornos de Coque",
      TRUE ~ categoria
    )
  )

## Keep filtering 
energy_consumo_total_2008_2021 <- energy_consumo_total_2008_2021 |> 
  filter(categoria %in% c("Sector Energético: Auto Consumo", "Sector Industrial y Minero",
                          "Sector Transporte", "Sector Comercial, Público y Residencial",
                          "Cons. No Energético - Industrial",
                          "Electricidad Autoproducción",
                          "Electricidad Servicio Público",
                          "Siderurgia Altos Hornos",
                          "Siderurgia Hornos de Coque",
                          "Plantas de Gas",
                          "Liquefacción de Gas",
                          "Refinería Petróleo - Gas Natural",
                          "Carbón y Leña",
                          "Producción de Metanol"))

## Merging 2022:2023 ----
### Data loading 
energy_consumption_2022 <- read_excel(here("Datasets", "consumo_final_2022_2023.xlsx"),
                                     sheet = "Consumo Final 2022")

energy_consumption_2023 <- read_excel(here("Datasets", "consumo_final_2022_2023.xlsx"),
                                      sheet = "Consumo Final 2023")

### Merging 
consumption_total_2008_2023<- bind_rows(energy_consumo_total_2008_2021, energy_consumption_2022,
                                        energy_consumption_2023)

### Saving
write_xlsx(consumption_total_2008_2023, here("Datasets", "consumption_total_2008_2023.xlsx"))

# Production data cleaning ----
energy_primary_import_2008_2021 <- energy_balance_2008_2021 |> 
  filter(item %in% c("OFERTA")) 

energy_primary_import_2008_2021 <- energy_primary_import_2008_2021 |> 
  filter(categoria %in% c("Importación", "Producción Primaria")) 

## Merging 2022:2023 ----
### Data loading 
energy_primary_import_2022 <- read_excel(here("Datasets", "oferta_primaria_import_2022_2023.xlsx"),
                                      sheet = "Oferta primaria import 2022")

energy_primary_import_2023 <- read_excel(here("Datasets", "oferta_primaria_import_2022_2023.xlsx"),
                                      sheet = "Oferta primaria import 2023")

### Merging 
primary_import_2008_2023<- bind_rows(energy_primary_import_2008_2021, energy_primary_import_2022,
                                     energy_primary_import_2023)

### Saving
write_xlsx(primary_import_2008_2023, here("Datasets", "primary_import_2008_2023.xlsx"))
