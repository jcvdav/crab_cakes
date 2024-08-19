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
lb_data <- read_rds(here("data", "processed", "lb_data_all.rds"))
ss_data <- readRDS(here("data/processed/ss_data_all.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
urchin_gonad_index <- lb_data %>% 
  filter(target_spp == "red_urchin") %>% 
  mutate(gonad_index = (catch_kg / catch_total_kg) * 100) %>% 
  mutate(zone = ifelse(site_name == "TURF", "TURF", "Reserve"),
         zone = fct_relevel(zone, "Reserve", "TURF"))

urchin_size_structure <- ss_data %>% 
  filter(target_spp == "red_urchin") %>% 
  mutate(zone = ifelse(site_name == "TURF", "TURF", "Reserve"),
         zone = fct_relevel(zone, "Reserve", "TURF"))

urchin_size_structure %>% 
  group_by(zone) %>% 
  summarize(mean = mean(size_cm),
            sd = sd(size_cm))

urchin_gonad_index %>% 
  group_by(zone) %>% 
  summarize(mean = mean(gonad_index),
            sd = sd(gonad_index))

t.test(formula = size_cm ~ zone,
       data = urchin_size_structure)

t.test(formula = gonad_index ~ zone,
       data = urchin_gonad_index,
       alternative = "greater")

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
size <- ggplot(urchin_size_structure,
       aes(x = zone, y = size_cm)) +
  geom_jitter(height = 0, width = 0.1, size = 0.1, color = "gray") +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "transparent") +
  stat_summary(geom = "pointrange", fun.data = "mean_se", fun.args = list(mult = 1)) +
  labs(x = "",
       y = "Test diameter (cm)")

gonads <- ggplot(urchin_gonad_index,
       aes(x = zone, y = gonad_index)) +
  geom_jitter(height = 0, width = 0.1, size = 0.1, color = "gray") +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "transparent") +
  stat_summary(geom = "pointrange", fun.data = "mean_se", fun.args = list(mult = 1)) +
  labs(x = "Source",
       y = "Gonadosomatic index (%)")

plot_grid(size, gonads,
          ncol = 1,
          labels = "AUTO")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------