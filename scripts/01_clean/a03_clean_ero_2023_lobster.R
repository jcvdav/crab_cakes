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
ero_2023_lob <- here("data/raw/BD_pesca_reservas_el_rosario_langosta_2023.XLSX")

# Load data --------------------------------------------------------------------
effort_2023_lob <- read_excel(path = ero_2023_lob,
                              sheet = 1) %>% 
  clean_names()

catch_2023_lob <- read_excel(path = ero_2023_lob,
                             sheet = 2) %>% 
  clean_names()
## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------

combined_2023_lob <- catch_2023_lob %>% 
  left_join(effort_2023_lob, by = join_by("nombre_equipo_de_pesca" == "nombre_del_equipo_de_pesca")) %>% 
  mutate(target_spp = "red_lobster",
         site_name = "Sport Fish",
         date = ymd("2023-9-17")) %>% 
  select(date,
         boat = nombre_equipo_de_pesca,
         site_name,
         total_traps = total_de_trampas,
         target_spp,
         catch_num = captura_pza_langosta,
         cost_mxn = gasto_gasolina_23_85_litro) %>% 
  mutate(cost_mxn = as.numeric(str_extract(str_remove(cost_mxn, ","), "[:digit:]+")))
  
## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = combined_2023_lob,
        file = here("data/processed/lb_ero_2023_lobster_clean.rds"))
