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
  mutate(post = 1 * (year >= 2024), # They fished between Sept 17 and Oct 1, 2023. That year's monitoring was between 2023-08-02 and 2023-08-11, so it was before
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
         community = fct_relevel(community, "Isla Natividad", "El Rosario")) |> 
  mutate(target_spp = str_to_sentence(str_replace_all(target_spp, "_", " ")))

## VISUALIZE ###################################################################
my_color_scale <- c("Pink abalone" = "#c995c7",
                    "Green abalone" = "#65805d",
                    "Red urchin" = scales::muted("red"),
                    "Red lobster" = "#805d5d")

# X ----------------------------------------------------------------------------
ggplot(data = data,
       aes(x = event, y = density, group = site_type,
           linetype = site_type, shape = site_type, color = target_spp)) +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  stat_summary(geom = "line", fun = "mean") + 
  stat_summary(geom = "point", fun = "mean") + 
  facet_wrap(community ~ target_spp, scales = "free_y", as.table = T) +
  scale_color_manual(values = my_color_scale) +
  theme(legend.position = "bottom",
        legend.title.position = "top") +
  labs(x = "Time-to-harvesting event (years)",
       y = "Density (org / m2)",
       color = "Harvested species",
       linetype = "Harvested site",
       shape = "Harvested site")

mod1 <- feols(asinh(density) ~ i(event, treated, -1) | site_name + year,
              data = data,
              cluster = ~site_name,
              split = ~paste(community, target_spp))

p1 <- map_dfr(mod1, tidy, conf.int = T, .id = "sample") %>% 
  mutate(x = as.numeric(str_extract(term, "-?[:digit:]+")),
         species = str_extract(sample, "Pink abalone|Green abalone|Red urchin|Red lobster"),
         community = str_extract(sample, "El Rosario|Isla Natividad"),
         community = fct_relevel(community, "Isla Natividad", "El Rosario")) |> 
  ggplot(aes(x = x, y = estimate, color = species)) + 
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_vline(xintercept = -1, linewidth = 0.5, linetype = "dashed") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_point(x = -1, y = 0, color = "black") +
  scale_color_manual(values = my_color_scale) +
  facet_wrap(community~species, scales = "free_y") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom",
        legend.title.position = "top") +
  labs(x = "Time-to-harvesting event (years)",
       y = "Estimate and 95% Conf. Int.",
       color = "Harvested species",
       linetype = "Harvested site",
       shape = "Harvested site")


p1



agg_mod <- feols(log(density) ~ post*treated | site_name + year,
                 data = data,
                 cluster = ~site_name,
                 split = ~paste(community, target_spp)) |> 
  set_names(c("El Rosario Green abalone",
              "El Rosario Red lobster",
              "El Rosario Red urchin",
              "Isla Natividad Green abalone",
              "Isla Natividad Pink abalone",
              "Isla Natividad Red lobster"))

modelsummary(agg_mod,
             gof_omit = c("IC|W|RMSE|FE"),
             stars = panelsummary:::econ_stars())

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
