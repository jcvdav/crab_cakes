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
ero_2020_abalone <- here("data/raw/BD_Pesca Reserva SportFish_abulón azul_junio2020.XLSX")

# Load data --------------------------------------------------------------------
lb_ero_2020_abalone <- read_excel(ero_2020_abalone, sheet = 1) %>% 
  clean_names()
ss_ero_2020_abalone <- read_excel(ero_2020_abalone, sheet = 2) %>% 
  clean_names()

## PROCESSING ##################################################################

make_numeric <- function(x) {as.numeric(str_extract(x, "[:digit:]+"))}

catch_price_mxn_kg <- 800

# Clean ERO 2020 Abalone logbook data ------------------------------------------
lb_ero_2020_abalone_clean <- lb_ero_2020_abalone %>% 
  select(date = fecha,
         fisher = scpp_pescador,
         boat = embarcacion_capitan,
         n_fishers = no_de_pescadores,
         site_name = sitio_de_pesca_nombre,
         min_depth = profundidad_min,
         max_depth = profundidad_max,
         time_hrs = duracion_del_dia_de_pesca,
         total_dives = number_de_buceos,
         catch_kg = captura_kg,
         catch_num = number_de_organismos,
         fuel_consumption_l = consumo_de_gasolina_litros,
         fuel_price_mxn_l = precio_litro_de_gasolina) %>% 
  mutate(target_spp = "green_abalone") %>% 
  # Date
  mutate(date = date(date)) %>% 
  filter(site_name == "Sport Fish") %>% 
  # Make numeric
  mutate_at(.vars = c("min_depth",
                      "max_depth",
                      "time_hrs",
                      "total_dives",
                      "catch_kg",
                      "catch_num",
                      "fuel_consumption_l",
                      "fuel_price_mxn_l"),
            .funs = make_numeric) %>% 
  # Depth from fathoms to meters
  mutate(min_depth_m = 1.85 * min_depth,
         max_depth_m = 1.85 * max_depth,
         mean_depth_m = (min_depth_m + max_depth_m) / 2) %>% 
  # Revenues, costs, profits
  mutate(revenue_mxn = catch_kg * catch_price_mxn_kg,
         cost_mxn = fuel_consumption_l * fuel_price_mxn_l,
         profits_mxn = revenue_mxn - cost_mxn,
         catch_total_kg = catch_kg) %>% 
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

# Clean ERO 2020 Abalone size structure data -----------------------------------
ss_ero_2020_abalone_clean <- ss_ero_2020_abalone %>% 
  select(date = fecha,
         boat = embarcacion,
         dive_number = number_buceo,
         catch_n_text = no_de_individuos_totales,
         size_cm = talla,
         abundance = abundancia) %>% 
  mutate(catch_n_over = as.numeric(str_extract(catch_n_text, "[:digit:]+(?= talla)")),
         catch_n_sub = as.numeric(str_extract(catch_n_text, "[:digit:]+(?= chicos)")),
         catch_n_sub = ifelse(is.na(catch_n_sub), 0, catch_n_sub),
         catch_n_tot = catch_n_over + catch_n_sub) %>% 
  mutate(target_spp = "green_abalone",
         site_name = "Sport Fish") %>% 
  select(date, 
         site_name,
         target_spp,
         size_cm,
         abundance) %>% 
  filter(!(is.na(abundance) | abundance == 0)) %>% 
  uncount(abundance)

coords_ero_2020_abalone_clean <- ss_ero_2020_abalone %>% 
  mutate(target_spp = "green_abalone") %>% 
  select(date = fecha,
         boat = embarcacion,
         dive_number = number_buceo,
         target_spp,
         lat = latitud,
         long = longitud) %>% 
  distinct() %>% 
  mutate(lat_deg = as.numeric(str_extract(lat, "[:digit:]+(?=°)")),
         lat_min = as.numeric(str_extract(lat, "(?<=° )[:digit:]+")),
         lat_seg = as.numeric(str_extract(lat, "\\.[:digit:]+")),
         lon_deg = as.numeric(str_extract(long, "[:digit:]+(?=°)")),
         lon_min = as.numeric(str_extract(long, "(?<=° )[:digit:]+")),
         lon_seg = as.numeric(str_extract(long, "\\.[:digit:]+"))) %>% 
  mutate(lat = lat_deg + (lat_min + lat_seg / 60),
         lon = lon_deg + (lon_min + lon_seg / 60))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = lb_ero_2020_abalone_clean,
        file = here("data/processed/lb_ero_2020_abalone_clean.rds"))
saveRDS(object = ss_ero_2020_abalone_clean,
        file = here("data/processed/ss_ero_2020_abalone_clean.rds"))
saveRDS(object = coords_ero_2020_abalone_clean,
        file = here("data/processed/coords_ero_2020_abalone_clean.rds"))

