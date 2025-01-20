################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  janitor,
  readxl,
  tidyverse
)

# Load data --------------------------------------------------------------------
Inv_2006_2021 <- read_excel(here("data/raw/COBI _Invrt_2006-2021_12jul2021.xlsx")) |> 
  clean_names()  |>  
  select(id, anio, sitio, comunidad, transecto, especie, abundancia)

Inv_2022 <- read_csv(here("data/raw/COBI_Invertebrates_2022_19may2023.csv")) |> 
  clean_names() |> 
  mutate(abundancia = as.numeric(abundancia)) |> 
  select(id, anio, sitio, comunidad, transecto, especie, abundancia)

Inv_2023 <- read_csv(here("data/raw/COBI Invertebrados PBC 2023(23feb2023).csv")) |> 
  clean_names() |> 
  mutate(abundancia = as.numeric(abundancia)) |> 
  select(id, anio, sitio, comunidad, transecto, especie, abundancia)

Inv_ERO_2024 <- read_excel(here("data/raw/COBI_Invertebrados_El Rosario_2024.xlsx"),
                           sheet = "Datos") |> 
  clean_names() |> 
  mutate(abundancia = as.numeric(abundancia),
         distancia = as.numeric(distancia),
         abundancia = round(abundancia * (30 / distancia)),
         especie = paste(genero, especie)) |> 
  select(id, anio, sitio, comunidad, transecto, genero,especie, abundancia)

Inv_INT_2024 <- read_excel(here("data/raw/COBI_Invertebrados_Natividad_2024.xls")) |> 
  clean_names() |> 
  mutate(abundancia = as.numeric(abundancia),
         distancia = as.numeric(distancia),
         abundancia = round(abundancia * (30 / distancia)),
         especie = paste(genero, especie)) |> 
  select(id, anio, sitio, comunidad, transecto, especie, abundancia)

Inv <- bind_rows(Inv_2006_2021, Inv_2022, Inv_2023, Inv_ERO_2024, Inv_INT_2024) |> 
  drop_na(abundancia)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
target_spp <- c("Haliotis corrugata",
                "Haliotis fulgens",
                "Mesocentrotus franciscanus",
                "Panulirus interruptus")

final <- Inv |> 
  select(id,
         year = anio,
         site_name = sitio,
         community = comunidad,
         transect = transecto,
         species = especie,
         abundance = abundancia) |> 
  filter(community %in% c("El Rosario", "Isla Natividad"),
         species %in% target_spp) |> 
  mutate(target_spp = case_when(species == "Haliotis corrugata" ~ "pink_abalone",
                                species == "Haliotis fulgens" ~ "green_abalone",
                                species == "Mesocentrotus franciscanus" ~ "red_urchin",
                                species == "Panulirus interruptus" ~ "red_lobster")) %>% 
  select(id, community, site_name, year, species, target_spp, abundance) |> 
  mutate(density = abundance / 60)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = final,
        file = here("data", "processed", "clean_inverts.rds"))

