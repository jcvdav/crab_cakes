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
pacman::p_load(here,
               readxl,
               tidyverse,
               janitor)

# Define file paths ------------------------------------------------------------
nat_2023_abalone <- here("data/raw/BD_Pesca Reservas Natividad_Abulones_jun2023.xlsx")

# Load data --------------------------------------------------------------------
lb_nat_2023_abalone <- read_excel(nat_2023_abalone, sheet = 2) %>% 
  clean_names()

ss_nat_2023_abalone <- read_excel(nat_2023_abalone, sheet = 3) %>% 
  clean_names()
# ss_nat_2020_abalone <- read_excel(nat_2023_abalone)

## PROCESSING ##################################################################

catch_price_mxn_kg <- 800 # Basedon on data from 2020 in ERO

fuel_price_mxn_l <- 18.45 # From Page 8, Table 1 in COBI_Pesca en Reserva Marina Isla Natividad_06nov23


# lb_nat_2023_abalone %>% group_by(especie) %>% summarize(kg = sum(peso, na.rm = T))
# fact_azul <- 1335 / 3057
# fact_amarillo <- 836 / 2163
# X ----------------------------------------------------------------------------
lb_nat_2023_abalone_clean <- lb_nat_2023_abalone %>% 
  select(date = fecha,
         boat = embarcacion,
         site_name = site,
         # min_depth = profundidad_min,
         # max_depth = profundidad_max,
         mean_depth = brazas,
         catch_kg = peso,
         catch_num = piezas,
         fuel_consumption_l = lts_gas,
         target_spp = especie) %>% 
  mutate(mean_depth_m = mean_depth * 1.85) %>% 
  mutate(target_spp = case_when(target_spp == "AZUL" ~ "green_abalone",
                                target_spp == "AMARILLO" ~ "pink_abalone")) %>% 
  mutate(revenue_mxn = catch_kg * catch_price_mxn_kg,
         cost_mxn = fuel_consumption_l * fuel_price_mxn_l,
         profits_mxn = revenue_mxn - cost_mxn) %>% 
  mutate(catch_total_kg = catch_kg) %>% 
  select(date,
         boat,
         site_name,
         mean_depth_m,
         target_spp,
         catch_total_kg,
         catch_kg,
         catch_num,
         revenue_mxn,
         cost_mxn,
         profits_mxn) 

# Sie structure ----------------------------------------------------------------
ss_nat_2023_abalone_clean <- ss_nat_2023_abalone %>% 
  pivot_longer(cols = everything(),
               names_to = "source",
               values_to = "length_mm") %>% 
  drop_na(length_mm) %>% 
  mutate(date = str_extract(source, "[:digit:]+_[:digit:]+_[:digit:]+"),
         target_spp = str_extract(source, "azul|amarillo"),
         site_name = str_extract(source, "_p_p|_l_p")) %>% 
  mutate(date = dmy(date)) %>% 
  mutate(target_spp = case_when(target_spp == "azul" ~ "green_abalone",
                                target_spp == "amarillo" ~ "pink_abalone")) %>% 
  mutate(site_name = ifelse(site_name == "_l_p", "La Plana", "Punta Prieta")) %>% 
  mutate(size_cm = length_mm / 10) %>% 
  select(date, 
         site_name,
         target_spp,
         size_cm)


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = lb_nat_2023_abalone_clean,
        file = here("data/processed/lb_nat_2023_abalone_clean.rds"))
saveRDS(object = ss_nat_2023_abalone_clean,
        file = here("data/processed/ss_nat_2023_abalone_clean.rds"))
