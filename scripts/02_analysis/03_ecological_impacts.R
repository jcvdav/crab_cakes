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
  modelsummary,
  fixest,
  ggfixest,
  broom,
  tidyverse
)

theme_set(theme_minimal(base_size = 10))

# Load data --------------------------------------------------------------------
invert_transects <- readRDS(file = here("data", "processed", "clean_inverts.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
ero_2020_sportfish_abalone <- invert_transects %>% 
  filter(community == "El Rosario",
         species == "Haliotis fulgens") %>% 
  mutate(post = 1 * (year >= 2020), # They fished on June 10, 11, and 29 of 2020. That year's monitoring was between 2020-07-27 and 2020-08-05 so it was post-harvest
         event = year - 2020,
         treated = 1 * (site_name == "Sportfish"))

ero_2021_caracolera_urchin <- invert_transects %>% 
  filter(community == "El Rosario",
         species == "Mesocentrotus franciscanus") %>% 
  mutate(post = 1 * (year >= 2022), # They fished on Nov 1, 2, 7, 8 and 12 of 2021. That year's monitoring was between 2021-08-03 and 2021-08-12 so it was pre-harvest
         event = year - 2022,
         treated = 1 * (site_name == "Caracolera"))

ero_2023_lobster <- invert_transects |> 
  filter(community == "El Rosario",
         species == "Panulirus interruptus") %>% 
  mutate(post = 1 * (year >= 2024), # They fished between Sept 17 and Oct 1, 2023. That year's monitoring was between 2023-08-02 and 2023-08-11
         event = year - 2024,
         treated = 1 * (site_name == "Sportfish"))

ero <- bind_rows(ero_2020_sportfish_abalone,
                 ero_2021_caracolera_urchin,
                 ero_2023_lobster)

nat_2023_abalone <- invert_transects %>% 
  filter(community == "Isla Natividad",
         species %in% c("Haliotis corrugata", "Haliotis fulgens")) %>% 
  mutate(post = 1 * (year >= 2023), # They fished July 8, and 10-13 of 2023. That year's monitoring was conducted between 2023-09-12 and 2023-09-22, so it was post-harvest
         event = year - 2023,
         treated = 1 * (site_name %in% c("La Plana/Las Cuevas", "Punta Prieta")))

nat_2022_lobster <- invert_transects %>% 
  filter(community == "Isla Natividad",
         species == "Panulirus interruptus") %>% 
  mutate(post = 1 * (year >= 2023), # Not sure when they fished, but lobster reason is typically after the monitorings
         event = year - 2023,
         treated = 1 * (site_name == "La Plana/Las Cuevas"))

nat <- bind_rows(nat_2022_lobster,
                 nat_2023_abalone)

data <- bind_rows(ero, nat) %>% 
  mutate(site_type = ifelse(treated == 1, "Treatment", "Control")) %>% 
  mutate(species_short = str_replace(species, "Haliotis", "H."),
         species_short = str_replace(species_short, "Mesocentrotus", "M."),
         species_short = str_replace(species_short, "Panulirus","P."),
         community = fct_relevel(community, "Isla Natividad", "El Rosario"),
         species_short = fct_relevel(species_short, "H. fulgens", "H. corrugata", "M. franciscanus", "P. interruptus"))

## VISUALIZE ###################################################################
my_color_scale <- c("H. corrugata" = "#F781BF",
                    "H. fulgens" = "#4DAF4A",
                    "M. franciscanus" = "#E41A1C",
                    "P. interruptus" = "#A65628")

# X ----------------------------------------------------------------------------

ggplot(data = data,
       aes(x = event, y = density, group = site_type,
           linetype = site_type, shape = site_type, color = species_short)) +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  stat_summary(geom = "line", fun = "mean") + 
  stat_summary(geom = "point", fun = "mean") + 
  facet_wrap(community ~ species_short, scales = "free_y", as.table = T) +
  scale_color_manual(values = my_color_scale) +
  theme(legend.position = "bottom",
        # legend.justification.inside = c(1, 0),
        # legend.position.inside = c(0.9, 0),
        legend.title.position = "top") +
  labs(x = "Time-to-harvesting event (years)",
       y = "Density (org / m2)",
       color = "Harvested species",
       linetype = "Harvested site",
       shape = "Harvested site")



mod1 <- feols(asinh(density) ~ i(event, treated, -1) | site_name + year + species_short,
             data = data,
             cluster = ~site_name)

mod2 <- feols(asinh(density) ~ i(event, treated, -1) | site_name + year,
              data = data,
              cluster = ~site_name,
              split = ~paste(community, species_short))

p1 <- ggiplot(object = mod1) +
  labs(x = "Time-to-harvesting event") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "None")

p2 <- map_dfr(mod2, tidy, conf.int = T, .id = "sample") %>% 
  mutate(x = as.numeric(str_extract(term, "-?[:digit:]+")),
         species = str_extract(sample, "H. fulgens|H. corrugata|M. franciscanus|P. interruptus"),
         community = str_extract(sample, "El Rosario|Isla Natividad"),
         community = fct_relevel(community, "Isla Natividad", "El Rosario"),
         species = fct_relevel(species, "H. fulgens", "H. corrugata", "M. franciscanus", "P. interruptus")) %>% 
  ggplot(aes(x = x, y = estimate, color = species)) + 
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_vline(xintercept = -1, linewidth = 0.5, linetype = "dashed") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_color_manual(values = my_color_scale) +
  facet_wrap(community~species, scales = "free_y") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom",
        # legend.justification.inside = c(1, 0),
        # legend.position.inside = c(0.9, 0),
        legend.title.position = "top") +
  labs(x = "Time-to-harvesting event (years)",
       y = "Estimate and 95% Conf. Int.",
       color = "Harvested species",
       linetype = "Harvested site",
       shape = "Harvested site")


cowplot::plot_grid(p1, p2,
                   ncol = 1,
                   labels = "AUTO")

mod <- feols(asinh(density) ~ post*treated | site_name + year,
             data = data,
             cluster = ~site_name,
             split = ~paste(community, species_short))

modelsummary(mod,
             gof_omit = c("IC|W|RMSE|Std|FE"),
             stars = panelsummary:::econ_stars())

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
