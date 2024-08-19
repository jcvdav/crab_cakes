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
Inv_2013_2021 <- read_excel(here("data/raw/COBI _Invrt_2006-2021_12jul2021.xlsx")) %>% 
  clean_names() %>% 
  select(-contains("hora")) %>% 
  select(-contains("temp"))
Inv_2022 <- read_csv(here("data/raw/COBI_Invertebrates_2022_19may2023.csv")) %>% 
  clean_names() %>% 
  mutate(abundancia = as.numeric(abundancia))
Inv_2023 <- read_csv(here("data/raw/COBI Invertebrados PBC 2023(23feb2023).csv")) %>% 
  clean_names() %>% 
  mutate(abundancia = as.numeric(abundancia))

Inv <- bind_rows(Inv_2013_2021, Inv_2022, Inv_2023)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
target_spp <- c("Haliotis corrugata",
                "Haliotis fulgens",
                "Mesocentrotus franciscanus",
                "Panulirus interruptus")

a <- Inv %>% 
  select(id,
         year = anio,
         site_name = sitio,
         community = comunidad,
         transect = transecto,
         species = especie,
         abundance = abundancia) %>% 
  filter(community %in% c("El Rosario", "Isla Natividad")) %>% 
  select(-community)

comm_sites <- Inv %>% 
  select(site_name = sitio,
         community = comunidad) %>% 
  filter(community %in% c("El Rosario", "Isla Natividad")) %>%
  select(community, site_name) %>%
  distinct()

b <- a %>%
  complete(nesting(site_name, id, year), transect, species, fill = list(abundance = 0)) %>% 
  filter(species %in% target_spp)

final <- left_join(b, comm_sites, by = join_by("site_name")) %>% 
  select(id, community, site_name, year, species, abundance) %>% 
  mutate(density = abundance / 60)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = final,
        file = here("data", "processed", "clean_inverts.rds"))












truth <- expand_grid(year = 1:4,
                     site = c("A", "B"),
                     transect = 1:3,
                     species = c("x", "y", "z")) %>% 
  mutate(abundance = rpois(year, lambda = 1)) %>% 
  select(year, site, transect, species, abundance) %>% 
  arrange(year, site, transect, species, abundance)

obs <- truth %>% 
  filter(abundance > 0)

obs_comp <- complete(obs, year, transect, species, nesting(site), fill = list(abundance = 0L)) %>% 
  select(year, site, transect, species, abundance) %>% 
  arrange(year, site, transect, species, abundance)

obs_comp

identical(truth, obs_comp)



b %>% 
  filter(species == "Mesocentrotus franciscanus",
         abundance > 0,
         site_name %in% c("Chinatown", "Punta Baja", "La Lobera", "Sportfish", "Caracolera")) %>% 
  ggplot(aes(x = year, y = abundance, color = site_name)) + 
  stat_summary(geom = "pointrange", fun.data = "mean_se") +
  stat_summary(geom = "line", fun = "mean") +
  facet_wrap(~site_name, ncol = 2, scales = "free_y") +
  geom_vline(xintercept = 2021)
