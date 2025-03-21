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
  kableExtra,
  tidyverse
)

# Load data --------------------------------------------------------------------
lb_data <- read_rds(here("data", "processed", "lb_data_all.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
profitability_metrics <- lb_data %>% 
  filter(!site_name == "TURF") %>% 
  group_by(location, year, site_name, target_spp) %>% 
  summarize(n_days = n_distinct(date),
            catch_total_kg = sum(catch_total_kg, na.rm = T),
            catch_num = sum(catch_num, na.rm = T),
            revenue_usd = sum(revenue_usd, na.rm = T) / 1000,
            cost_usd = sum(cost_usd, na.rm = T) / 1000,
            profits_usd = sum(profits_usd, na.rm = T) / 1000,
            .groups = "drop") %>% 
  mutate(n_days = ifelse(target_spp == "red_lobster", 15, n_days),
         profits_per_effort = profits_usd / n_days) %>% 
  mutate(target_spp = str_to_sentence(str_replace_all(target_spp, "_", " ")),
         location = ifelse(location == "NAT", "Isla Natividad", "El Rosario"))


# X ----------------------------------------------------------------------------
table <- kbl(profitability_metrics,
    col.names = c("Location", "Year", "Reserve", "Target species",
                  "Days fishing", "Total Catch (Kg)", "Total Catch (#)",
                  "Revenue (K $USD)", "Cost (K $USD)", "Profits (K $USD)",
                  "Daily profits(K $USD/day)"),
    digits = 2)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
save_kable(x = table,
           file = here("results", "tab", "harvest_summary.pdf"))
save_kable(x = table,
           file = here("results", "tab", "harvest_summary.png"))
save_kable(x = table,
           file = here("results", "tab", "harvest_summary.html"))
