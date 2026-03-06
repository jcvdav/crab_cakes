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

theme_set(theme_linedraw(base_size = 10))

# Load data --------------------------------------------------------------------
invert_transects <- readRDS(file = here("data", "processed", "clean_inverts.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
ero_2020_sportfish_abalone <- invert_transects %>% 
  filter(community == "El Rosario",
         species == "Haliotis fulgens") %>% 
  mutate(post = 1 * (year >= 2020), # They fished on June 10, 11, and 29 of 2020. That year's monitoring was between 2020-07-27 and 2020-08-05 so it was post-harvest
         event = year - 2020,
         treated = 1 * (site_name == "Sportfish"),
         first_treat = 2020)

ero_2023_lobster <- invert_transects |> 
  filter(community == "El Rosario",
         species == "Panulirus interruptus") %>% 
  mutate(post = 1 * (year >= 2024), # They fished between Sept 17 and Oct 1, 2023. That year's monitoring was between 2023-08-02 and 2023-08-11, so it was before
         event = year - 2024,
         treated = 1 * (site_name == "Sportfish"),
         first_treat = 2023)

ero <- bind_rows(ero_2020_sportfish_abalone,
                 ero_2023_lobster)

nat_2023_abalone <- invert_transects %>% 
  filter(community == "Isla Natividad",
         species %in% c("Haliotis corrugata", "Haliotis fulgens")) %>% 
  mutate(post = 1 * (year >= 2023), # They fished July 8, and 10-13 of 2023. That year's monitoring was conducted between 2023-09-12 and 2023-09-22, so it was post-harvest
         event = year - 2023,
         treated = 1 * (site_name %in% c("La Plana/Las Cuevas", "Punta Prieta")),
         firt_treat = 2023)

nat_2022_lobster <- invert_transects %>% 
  filter(community == "Isla Natividad",
         species == "Panulirus interruptus") %>% 
  mutate(post = 1 * (year >= 2023), # Not sure when they fished, but lobster season is typically after the monitorings
         event = year - 2023,
         treated = 1 * (site_name == "La Plana/Las Cuevas"),
         first_treat = 2023)

nat <- bind_rows(nat_2022_lobster,
                 nat_2023_abalone)

data <- bind_rows(ero, nat) %>% 
  mutate(site_type = ifelse(treated == 1, "Treatment", "Control")) %>% 
  mutate(species_short = str_replace(species, "Haliotis", "H."),
         species_short = str_replace(species_short, "Panulirus","P."),
         community = fct_relevel(community, "Isla Natividad", "El Rosario")) |> 
  mutate(target_spp = str_to_sentence(str_replace_all(target_spp, "_", " ")),
         target_spp = fct_relevel(target_spp, c("Red lobster",
                                                "Green abalone")))

## VISUALIZE ###################################################################
my_color_scale <- c(
  "Pink abalone"  = "#B65F73",
  "Green abalone" = "#4F8A7D",
  "Red urchin"    = "#CB181D",
  "Red lobster"   = "#D55E00"
)

# X ----------------------------------------------------------------------------
p1 <- ggplot(data = data,
       aes(x = event, y = density, group = site_type,
           linetype = site_type, shape = site_type, color = target_spp)) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_vline(xintercept = -0.5, linewidth = 0.5, linetype = "dashed") +
  stat_summary(geom = "linerange", fun.data = "mean_se") +
  stat_summary(geom = "line", fun = "mean") + 
  stat_summary(geom = "point", fun = "mean", size = 3) + 
  facet_wrap(community ~ target_spp, scales = "free_y") +
  scale_color_manual(values = my_color_scale) +
  guides(color = "none") +
  theme(legend.position = "inside",
        legend.justification.inside = c(1, 0),
        legend.position.inside = c(0.9, 0.2)) +
  labs(x = "Time-to-harvesting event (years)",
       y = "Density (org / m2)",,
       linetype = "Harvested site",
       shape = "Harvested site")

## ESTIMATION ##################################################################
mod1 <- feols(asinh(density) ~ i(event, treated, -1) | site_name + year,
              data = data,
              cluster = ~site_name,
              split = ~paste(community, target_spp))

p2 <- map_dfr(mod1, tidy, conf.int = T, .id = "sample") %>% 
  mutate(x = as.numeric(str_extract(term, "-?[:digit:]+")),
         species = str_extract(sample, "Pink abalone|Green abalone|Red lobster"),
         species = fct_relevel(species, c("Red lobster",
                                                "Green abalone")),
         community = str_extract(sample, "El Rosario|Isla Natividad"),
         community = fct_relevel(community, "Isla Natividad", "El Rosario")) |> 
  ggplot(aes(x = x, y = estimate, color = species)) + 
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_vline(xintercept = -0.5, linewidth = 0.5, linetype = "dashed") +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), color = "black") +
  geom_linerange(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                  linewidth = 1.5) +
  geom_point(size = 3) +
  geom_point(x = -1, y = 0, color = "black", size = 3) +
  scale_color_manual(values = my_color_scale) +
  guides(color = "none") +
  facet_wrap(community ~ species, scales = "free_y") +
  theme(legend.position = "bottom",
        legend.title.position = "top") +
  labs(x = "Time-to-harvesting event (years)",
       y = "Estimate ± SE and 95% Conf. Int.")


agg_mod <- feols(log(density) ~ post*treated | site_name + year,
                 data = data |> 
                   filter(between(event, -1, 0)),
                 cluster = ~site_name,
                 split = ~paste(community, target_spp)) |> 
  set_names(c("El Rosario Green abalone",
              "El Rosario Red lobster",
              "Isla Natividad Green abalone",
              "Isla Natividad Pink abalone",
              "Isla Natividad Red lobster"))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

modelsummary(agg_mod,
             gof_omit = c("R2$|IC|W|RMSE|FE"),
             stars = panelsummary:::econ_stars(),
             output = here("results/tab/regression_table.tex"))

ggsave(plot = p1,
       filename = here("results/figs/ts_abundances.png"),
       width = 9,
       height = 6)

ggsave(plot = p2,
       filename = here("results/figs/es_abundances.png"),
       width = 9,
       height = 6)









