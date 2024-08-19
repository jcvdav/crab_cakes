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

# Load data --------------------------------------------------------------------
ero_2021_urchin <- read_excel(here("data/raw/BD_Pesca Reserva La Caracolera erizo rojo_Abril2022.xlsx"),
                             sheet = 2) %>% 
  clean_names()

## PROCESSING ##################################################################

gonad_price_mxn_kg <- 353100 / 642 # Ver Página 8, Tabla II del reporte "COBI_Pesca en Reserva El Rosario_sep2022"

# X ----------------------------------------------------------------------------
ero_2021_urchin_clean <- ero_2021_urchin %>% 
    select(date = fecha,
           fisher = buzo,
           skipper = cabo_de_vida,
           captain = capitan,
           boat = nombre_embarcacion,
           lat = latitud,
           long = longitud,
           time_hrs = tiempo_de_pesca,
           fuel_costs_mxn = gasolina,
           meal_costs_mxn = gastos_lunch,
           depth = profundidad,
           dive_number = numero_inmersion,
           site_name = sitio_pesca,
           catch_total_kg = captura_bola,
           catch_gonad_kg = rendimiento_gonodal,
           catch_n = numero_organismos,
           size = talla,
           contains("x")) %>% 
  mutate(time = str_extract(as.character(ymd_hms(time_hrs)), "[:digit:]{2}:[:digit:]{2}:[:digit:]{2}"),
         hours = as.numeric(str_extract(time, "[:digit:]{2}(?=:)")),
         mins = as.numeric(str_extract(time, "(?<=:)[:digit:]{2}(?=:)")),
         time_hrs = hours + (mins / 60),
         target_spp = "red_urchin",
         site_name = str_to_title(site_name)) %>% 
  mutate(site_name = ifelse(site_name == "Caracolera", "La Caracolera", "TURF"))

lb_ero_2021_urchin_clean <- ero_2021_urchin_clean %>% 
  group_by(date, boat, site_name, target_spp) %>% 
  summarize(min_depth_m = min(depth) * 1.85,
            max_depth_m = max(depth) * 1.85,
            mean_depth_m = mean(depth) * 1.85,
            time_hrs = sum(time_hrs),
            total_dives = n_distinct(dive_number),
            catch_total_kg = sum(catch_total_kg),
            catch_kg = sum(catch_gonad_kg), # this is gonad weight, the money maker
            catch_num = sum(catch_n),
            cost_mxn = sum(fuel_costs_mxn),
            .groups = "drop") %>% 
  mutate(revenue_mxn = catch_kg * gonad_price_mxn_kg,
         profits_mxn = revenue_mxn - cost_mxn) %>% 
  select(date,
         boat,
         site_name,
         min_depth_m,
         max_depth_m,
         mean_depth_m,
         time_hrs,
         total_dives,
         target_spp,
         catch_total_kg,
         catch_kg,
         catch_num,
         revenue_mxn,
         cost_mxn,
         profits_mxn)

ss_ero_2021_urchin_clean <- ero_2021_urchin_clean %>% 
  select(target_spp, site_name, date, boat, dive_number, size:x51) %>% 
  pivot_longer(cols = c(size:x51), names_to = "urchin", values_to = "size_mm") %>% 
  mutate(size_cm = size_mm / 10) %>% 
  select(date, 
         site_name,
         target_spp,
         size_cm)

coords_ero_2021_urchin_clean <- ero_2021_urchin_clean %>%
  filter(site_name == "La Caracolera") %>% 
  select(date,
         fisher,
         boat,
         dive_number,
         lat = lat,
         long = long) %>% 
  distinct()

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = lb_ero_2021_urchin_clean,
        file = here("data/processed/lb_ero_2021_urchin_clean.rds"))
saveRDS(object = ss_ero_2021_urchin_clean,
        file = here("data/processed/ss_ero_2021_urchin_clean.rds"))
saveRDS(object = coords_ero_2021_urchin_clean,
        file = here("data/processed/coords_ero_2021_urchin_clean.rds"))

