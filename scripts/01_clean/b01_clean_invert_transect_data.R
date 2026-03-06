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

## NOTE: Survey id 12 8 2021 Chinatown 16 for red lobster (Panuliris interruptus) hit the 50 count maxiumum at a distance of 5m
    # Edits made by JD Reigrut

# Removing this line from the final df 
final <- final %>%
  filter(!(id == "12 8 2021 Chinatown 16" & species == "Panulirus interruptus"))

# Creating correct row from the invert transect data (for back tracing)

invert_dist <- read.csv(
  file = here("data", "raw", "datos_invert_50_all_distance.csv"),
  fileEncoding = "latin1",
  stringsAsFactors = FALSE
)

invert_dist <- invert_dist %>%
  mutate(distance = na_if(Distancia, "Sin información")) %>%
  select(-Distancia) %>%
  mutate(distance = as.numeric(distance),
         extrap_count = (50 / distance) * abundance) %>%
  filter(species %in% c('Panulirus interruptus')) %>%
  drop_na(extrap_count) %>%
  mutate(target_spp = case_match(species, "Panulirus interruptus" ~ "red_lobster")) %>% #  
  mutate(density = (extrap_count / 60)) %>%
  select(-abundance) %>%
  rename(abundance = extrap_count) %>% #Extrapolated count becomes abundance 
  mutate(zone = "Reserva") # Taken from value of other sites in the same survey (by transect ID)

# Adding corrected row back to final df 
final <- bind_rows(final, invert_dist)
  
## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = final,
        file = here("data", "processed", "clean_inverts.rds"))


