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

# Load data --------------------------------------------------------------------
lb_data <- list.files(here("data", "processed"), pattern = "lb_n|lb_e", full.names = T) %>% 
  map_dfr(readRDS) %>% 
  mutate(location  = ifelse(site_name %in% c("La Caracolera", "Sport Fish", "TURF"), "ERO", "NAT"),
         year = year(date),
         code = paste(location, year, site_name, sep = ", ")) %>% 
  mutate(revenue_usd = revenue_mxn / 18,
         cost_usd = cost_mxn / 18,
         profits_usd = profits_mxn / 18) %>% 
  mutate(species_short = case_when(target_spp == "pink_abalone" ~ "H. corrugata",
                                   target_spp == "green_abalone" ~ "H. fulgens",
                                   target_spp == "red_urchin" ~ "M. franciscanus",
                                   target_spp == "red_lobster" ~ "P. interruptus")) %>% 
  select(date, boat, site_name, min_depth_m, max_depth_m, mean_depth_m,
         time_hrs, total_dives, target_spp, species_short, total_traps,catch_total_kg, catch_kg, catch_num,
         revenue_mxn, cost_mxn, profits_mxn, location, year,
         code, revenue_usd, cost_usd, profits_usd)

ss_data <- list.files(here("data", "processed"), pattern = "ss_n|ss_e", full.names = T) %>% 
  map_dfr(readRDS) %>% 
  mutate(location  = ifelse(site_name %in% c("La Caracolera", "Sport Fish"), "ERO", "NAT"),
         year = year(date),
         code = paste(location, year, site_name, sep = ", ")) %>% 
  mutate(species_short = case_when(target_spp == "pink_abalone" ~ "H. corrugata",
                                   target_spp == "green_abalone" ~ "H. fulgens",
                                   target_spp == "red_urchin" ~ "M. franciscanus",
                                   target_spp == "red_lobster" ~ "P. interruptus"))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = lb_data,
        file = here("data/processed/lb_data_all.rds"))
saveRDS(object = ss_data,
        file = here("data/processed/ss_data_all.rds"))


