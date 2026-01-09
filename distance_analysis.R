library(dplyr)
library(tidyr)
library(readr)
library(here)
library(stringr)

invert_dist <- read.csv(
  file = here("data", "raw", "datos_invert_50_all_distance.csv"),
  fileEncoding = "latin1",
  stringsAsFactors = FALSE
)

invert_dist <- invert_dist %>%
  # Change all values that have no distance value to be NA
  mutate(distance = na_if(Distancia, "Sin información")) %>%
  select(-Distancia) %>%
  mutate(distance = as.numeric(distance)) %>%
  # Add new column of extrapolated count based on the count distance and abundance 
  mutate(extrap_count = (50 / distance) * abundance)

# Proportion table of count of valid and na distance values by species 
species_distance_summary <- invert_dist %>%
  group_by(species) %>%
  summarise(
    n_total = n(),
    n_with_value = sum(!is.na(distance)),
    n_na = sum(is.na(distance))
  ) %>%
  ungroup()

species_distance_summary

# Filtering to contain only paper relevant species - abalone, urchin, and lobster 
  # No surveys with count of abalone greater than 50
  # 1 red_lobster entry with valid recorded distance out of total of 1 surveys containing over 50 red lobster 
  # 65 purple_urchin entries with valid recorded distance out of total 121 surveys
  # 23 red urchin entries with valid recorded distance of of total 54 surveys

filtered_invert_dist <- invert_dist %>%
  filter(species %in% c('Strongylocentrotus purpuratus', 
                        'Mesocentrotus franciscanus',
                        'Panulirus interruptus')) %>%
  drop_na(extrap_count) %>%
  mutate(target_spp = case_match(species,
                                 "Strongylocentrotus purpuratus" ~ "purple_urchin", # 65 entries 
                                 "Mesocentrotus franciscanus" ~ "red_urchin", # 23 entries 
                                 "Panulirus interruptus" ~ "red_lobster")) %>% # 1 entry 
  mutate(density = (extrap_count / 60))

# Using site ID to input zone status for extrapolated rows
unique_site <- clean_inverts %>% 
  mutate(site_id = str_remove(id, "\\s+\\d+$")) %>%
  distinct(site_id, .keep_all = TRUE) %>%
  select(site_id, zone)

filtered_invert_dist <- filtered_invert_dist %>%
  mutate(site_id = str_remove(id, "\\s+\\d+$")) %>% 
  left_join(
    unique_site,
    by = "site_id" ) %>%
  select(-site_id)

# Removing the rows with errors (both with distance and not with distance) from clean_inverts 
no_errors <- clean_inverts %>%
  anti_join(
    invert_dist %>% select(id, species),
    by = c("id", "species")
  )

# Renaming columns for row bind
clean_invert_dist <- filtered_invert_dist %>%
  select(-distance, -abundance) %>%
  rename(abundance = extrap_count)
  
clean_final <- bind_rows(
  no_errors,
  clean_invert_dist
)


## Potential issue with original cleaned df containing duplicates for certain transect species combinations 
potential_duplicated <- clean_inverts %>% 
  count(id, species) %>%
  filter (n > 1)