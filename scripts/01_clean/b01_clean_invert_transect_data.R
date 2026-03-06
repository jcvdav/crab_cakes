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
  select(id, anio, sitio, comunidad, zona, transecto, especie, abundancia)

Inv_2022 <- read_csv(here("data/raw/COBI_Invertebrates_2022_19may2023.csv")) |> 
  clean_names() |> 
  mutate(abundancia = as.numeric(abundancia)) |> 
  select(id, anio, sitio, comunidad, zona, transecto, especie, abundancia)

Inv_2023 <- read_csv(here("data/raw/COBI Invertebrados PBC 2023(23feb2023).csv")) |> 
  clean_names() |> 
  mutate(abundancia = as.numeric(abundancia)) |> 
  select(id, anio, sitio, comunidad, zona, transecto, especie, abundancia)

Inv_ERO_2024 <- read_excel(here("data/raw/COBI_Invertebrados_El Rosario_2024.xlsx"),
                           sheet = "Datos") |> 
  clean_names() |> 
  mutate(abundancia = as.numeric(abundancia),
         distancia = as.numeric(distancia),
         abundancia = round(abundancia * (30 / distancia)),
         especie = paste(genero, especie)) |> 
  select(id, anio, sitio, comunidad, zona, transecto, genero,especie, abundancia)

Inv_INT_2024 <- read_excel(here("data/raw/COBI_Invertebrados_Natividad_2024.xls")) |> 
  clean_names() |> 
  mutate(abundancia = as.numeric(abundancia),
         distancia = as.numeric(distancia),
         abundancia = round(abundancia * (30 / distancia)),
         especie = paste(genero, especie)) |> 
  select(id, anio, sitio, comunidad, zona, transecto, especie, abundancia)

Inv <- bind_rows(Inv_2006_2021, Inv_2022, Inv_2023, Inv_ERO_2024, Inv_INT_2024) |> 
  drop_na(abundancia)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
target_spp <- c("Haliotis corrugata",
                "Haliotis fulgens",
                "Panulirus interruptus")

communities <- c("El Rosario", "Isla Natividad")

data <- Inv |> 
  select(id,
         year = anio,
         site_name = sitio,
         community = comunidad,
         zone = zona, 
         transect = transecto,
         species = especie,
         abundance = abundancia) |> 
  filter(community %in% communities,
         species %in% target_spp) |> 
  mutate(target_spp = case_when(species == "Haliotis corrugata" ~ "pink_abalone",
                                species == "Haliotis fulgens" ~ "green_abalone",
                                species == "Panulirus interruptus" ~ "red_lobster")) %>% 
  select(id, year, community, zone, site_name, transect, species, target_spp, abundance) |> 
  mutate(density = abundance / 60)

## Checks: ---------------------------------------------------------------------
# Does any species have an abundance of exactly 50?
ids_with_50 <- data |>
  filter(abundance == 50) |> 
  pull(id)

ids_with_50

# It looks like Panulirus does. It comes from Inv_2006_2021, which does not include a distance column
# that would allow us to extrapolate the data as done in the methodology.
# We were able to source the original distance data to fix this. The fix is implemented below.

## Fixes -----------------------------------------------------------------------
## NOTE: Survey id 12 8 2021 Chinatown 16 for red lobster (Panuliris interruptus) hit the 50 count maxiumum at a distance of 5m
    # Edits made by JD Reigrut

# Creating correct row from the invert transect data (for back tracing)
fixed_dist <- read_csv(file = here("data", "raw", "Imelda_checks", "datos_invert_50_all_distance.csv")) |> 
  clean_names() |> 
  # Get the abundance and distance data for the record missing distance
  filter(id == ids_with_50) |> 
  mutate(distancia = as.numeric(distancia),
         zone = unique(data$zone[data$id %in% ids_with_50]),
         target_spp = unique(data$target_spp[data$id %in% ids_with_50])) |> 
  # Normalize abundance, convert to density, and obtain 
  mutate(abundance = round(abundance * (30 / distancia)),
         density = abundance / 60) |> 
  select(id, year, community, zone, site_name, transect, species, target_spp, abundance, density)


# Finalize data
final <- data %>%
  # Removing this line from the final df 
  filter(!(id %in% ids_with_50 & species == "Panulirus interruptus")) |> 
  # Adding corrected row back to final df 
  bind_rows(fixed_dist)

## More checks -----------------------------------------------------------------
# Is the final data the same size as the data before we modified the lobster record?
dim(data) == dim(final)

# Other than the record we manually modified, are the datasets the same?
identical(
  data |> filter(!id %in% ids_with_50),
  final |> filter(!id %in% ids_with_50)
)
  
## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = final,
        file = here("data", "processed", "clean_inverts.rds"))


