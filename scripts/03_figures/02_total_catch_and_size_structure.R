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
  cowplot,
  tidyverse
)

theme_set(theme_minimal(base_size = 7))

# Load data --------------------------------------------------------------------
lb_data <- read_rds(here("data", "processed", "lb_data_all.rds"))
ss_data <- readRDS(here("data/processed/ss_data_all.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
main_lb_data <- lb_data %>%
  filter(!site_name == "TURF") %>% 
  group_by(code, target_spp) %>% 
  summarize(catch_total_kg = sum(catch_total_kg, na.rm = T),
            catch_num = sum(catch_num, na.rm = T),
            .groups = "drop") |> 
  mutate(target_spp = str_to_sentence(str_replace_all(target_spp, "_", " ")))

main_ss_data <- ss_data %>%
  filter(!site_name == "TURF") |> 
  mutate(target_spp = str_to_sentence(str_replace_all(target_spp, "_", " ")))

## VISUALIZE ###################################################################
my_color_scale <- c("Pink abalone" = "#c995c7",
                    "Green abalone" = "#65805d",
                    "Red urchin" = scales::muted("red"),
                    "Red lobster" = "#805d5d")

# X ----------------------------------------------------------------------------
extraction <- ggplot(data = main_lb_data,
       mapping = aes(x = code, y = catch_total_kg, fill = target_spp)) +
  geom_col(color = "black",
           linewidth = 0.5) +
  scale_fill_manual(values = my_color_scale) +
  theme(legend.position = "inside",
        legend.position.inside = c(1, 1),
        legend.justification = c(1, 1)) +
  labs(x = "Event",
       y = "Total catch (Kg)",
       fill = "Species") +
  coord_flip()

size_dist <- ggplot(data = main_ss_data,
       mapping = aes(x = size_cm, fill = target_spp)) + 
  geom_histogram(binwidth = 1,
                 position = "dodge",
                 color = "black",
                 linewidth = 0.1) +
  scale_fill_manual(values = my_color_scale) +
  facet_wrap(code ~ target_spp, scales = "free_y", ncol = 2, as.table = F) +
  theme(legend.position = "None") +
  labs(x = "Length (cm)",
       y = "Number of individuals")


cowplot::plot_grid(extraction, size_dist,
                   ncol = 2,
                   labels = c("AUTO"))

