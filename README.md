# How not to lie with energy data

## About this repository

This repository holds the data preparation and descriptive analysis for a paper on how Chile's energy transition is represented in different energy measures. It brings together the national energy balance, installed electricity capacity, and hourly electricity generation. The main annual analysis covers 2008–2024; an hourly figure compares average October generation profiles in 2014 and 2024.

The measures answer different questions. Energy balance quantities are in **teracalories (Tcal)**, installed capacity is in **megawatts (MW)**, and the hourly generation figure reports **mean MW by hour**. Capacity is not generation, and shares depend on their stated denominator. The analysis is descriptive; the figures do not establish causal effects of energy policy.

## Repository map

``` text
.
├── cleaning_energy.R          # Prepare energy balance, DCI and hourly data
├── analysis.R                 # Produce the tables and figures
├── Datasets/                  # Inputs supplied with the repository
│   └── clean/                # Derived data written by cleaning_energy.R
├── Outputs/
│   ├── figures/              # JPEG figures written by analysis.R
│   └── tables/               # Analysis workbook
├── resources/                # Source reports and contextual documents
├── dataset_manual.docx       # Dataset documentation
└── How not to lie with energy data.Rproj
```

`Datasets/clean/` and `Outputs/` already contain derived files in the current checkout. The steps below describe how to regenerate them from the inputs.

## Data inputs and provenance

| Input read by the scripts | Role and coverage |
|------------------------------------|------------------------------------|
| `Datasets/bne_balance_nacional.csv` | Historical national energy balance, 2008–2021. The script reads semicolon separated values with decimal commas. CNE |
| `Datasets/bne_2022_2024.xlsx` | National energy balance for 2022–2024. CNE |
| `Datasets/full_energy_dfs_2026_02_10.xlsx` | Installed capacity from the `Installed Capacity (MW)` worksheet. CNE |
| `Datasets/gen_horaria_00_15.xlsx` | Hourly generation, 2000–2015. CEN |
| `Datasets/gen_horaria_16_19.xlsx` | Hourly generation, 2016–2019. CEN |
| `Datasets/gen_horaria_20_23.xlsx` | Hourly generation, 2020–2023; the script combines sheets 1 and 2. CEN |
| `Datasets/gen_horaria_2024.xlsx` | Hourly generation, 2024. CEN |

The files above are the **direct inputs** to the two R scripts. Other workbooks in `Datasets/` and reports in `resources/` are retained as source or contextual material but are not opened by this pipeline. See `dataset_manual.docx` for fuller dataset documentation. The repository does not include a machine readable source register linking each consolidated input to a download URL, retrieval date, or upstream revision; retain that information when replacing an input.

## Analytical workflow

| Stage | Script | Reads | Writes |
|------------------|------------------|------------------|------------------|
| 1\. Prepare data | `cleaning_energy.R` | National balance CSV and workbook; four hourly generation workbooks | Cleaned balance, annual supply and consumption series, DCI series, and hourly generation objects in `Datasets/clean/` |
| 2\. Analyse and plot | `analysis.R` | Stage 1 outputs; installed capacity workbook | `Outputs/tables/analysis_tables_2008_2024.xlsx` and JPEGs in `Outputs/figures/` |

Stage 1 harmonizes balance categories across the historical and recent files, removes duplicated total rows, recodes sectors and fuels, and creates annual summaries. It then derives the decarbonization index (DCI) and reshapes hourly generation from daily wide records into an hourly long table. Stage 2 checks year coverage and DCI consistency, calculates changes and shares, and draws the figures.

### DCI definition

The script converts final energy consumption from Tcal to TJ using **1 Tcal = 4.1868 TJ**. It multiplies each fuel's TJ by the hard coded direct CO₂ factor in `cleaning_energy.R` to obtain an annual weighted carbon intensity, `CI_t` (tCO₂/TJ). The index is `DCI = 1 − CI_t / 94.6`, where 94.6 tCO₂/TJ is the script's coal reference factor. The decomposition is `DCI = DCIOG + DCINC`; `DCINC` is the energy share assigned a zero direct carbon factor, and `DCIOG` is the remainder.

This is a **decarbonization index based on direct end use carbon intensity relative to coal**, not a measured emissions inventory or a lifecycle estimate. The code excludes non energy final use and assigns zero **direct** factors to electricity, biomass, biogas, and black liquor. These are analytical conventions embedded in the script and matter for interpretation.

## How to run

### Requirements

-   R with support for the base pipe `|>` (R 4.1 or newer).
-   Git LFS if obtaining the repository through Git; the `.gitattributes` file places CSV, Excel, RData, and RDS files in LFS.
-   The R packages requested at the top of each script. Install `pacman` first; `pacman::p_load()` attempts to install and load the others, so the first run may need internet access. Packages include `tidyverse`, `readxl`, `writexl`, `here`, `scales`, `patchwork`, `paletteer`, and `ggstream`. The cleaning script additionally requests spatial packages including `sf`, `rnaturalearth`, and `chilemapas`.

There is no `renv.lock` or other frozen package environment. Save `sessionInfo()` alongside any run used for publication, and record the exact version of each input file.

### Run order

From the repository root:

``` sh
git lfs pull                 # after cloning; skip if the data files are already present
Rscript cleaning_energy.R
Rscript analysis.R
```

Alternatively, open `How not to lie with energy data.Rproj` in RStudio and source the same scripts in that order. Both scripts use `here()` for paths. They create the output directories if needed and overwrite generated files with matching names.

**Current verification status:** the commands reflect the scripts' intended order and file dependencies, but a clean end to end run has not been confirmed. The current `cleaning_energy.R` includes empty arguments near the recent balance conversion and total consumption filter; these should be resolved before treating a fresh rerun as verified. R syntax parsing alone does not test those calls at runtime.

## Outputs

The main workbook, `Outputs/tables/analysis_tables_2008_2024.xlsx`, contains annual final consumption by sector and fuel, primary supply and imports, DCI and its decomposition, fuel and sector contributions, and 2008–2024 change tables.

`analysis.R` saves the following JPEG figure groups to `Outputs/figures/`:

| Numbers | Subject |
|------------------------------------|------------------------------------|
| 01–02 | Primary energy production: fuel shares and levels |
| 03–04 | Installed electricity capacity: shares and levels |
| 05–07 | Primary supply and principal imported fuels |
| 08–12 | Final consumption by fuel and sector |
| 14–15 | DCI and its decomposition |
| 17 | Mean hourly electricity generation in October 2014 and October 2024 |

## License and citation

The original R code and repository documentation are available under the [MIT License](LICENSE). The CSV files and databases we make available for download are processed derivations from public sources. By downloading them, the user accepts that the interpretation and cross-referencing of this data is their sole responsibility
