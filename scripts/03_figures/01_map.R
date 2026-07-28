################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
# date
#
# Description
#
################################################################################
  
# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
pacman::p_load(here,
               tidyverse,
               sf,
               rnaturalearth,
               ggplot2,
               ggspatial,
               cowplot,
               patchwork)

## Load data -------------------------------------------------------------------
# This contains the vertices for all the polygons. You will need to convert them to actual polygons with SF, or 
# be creative in how you represent them (e.g. see ?geom_polygon)
data <- read_csv("https://github.com/jcvdav/MAREAmanuscript/raw/refs/heads/master/Data/Spatial/baja_coordinates.csv")


# PROCESSING ###################################################################

## Converting polygon vertices to a geometry -------------------------------------------------------------------

### Closing polygon vertices
vertices <- data %>%
  group_by(Id) %>%
  mutate(n = n()) %>%
  filter(Order == 1) %>%
  select(-Order) %>%
  mutate(Order = n +1 
  ) %>%
  bind_rows(data) %>%
  ungroup() %>%
  select(-n) %>%
  arrange(Id, Order)

# Creating polygons using st_polygon() inside group_map()
polygons <- vertices %>%
  arrange(Id, Order) %>%
  group_by(Id) %>%
  group_map(~ st_polygon(list(as.matrix(.x[, c("Longitude", "Latitude")])))) %>%
  st_sfc(crs = 4326) 

# VISUALIZE ####################################################################

## Producing referenc ----------------------------------------------------------------
mexico <- ne_countries(scale = "large", country = "mexico", returnclass = "sf")
world  <- ne_countries(scale = "small", returnclass = "sf")

## Reference map  ----------------------------------------------------------------
label_coordinates <- data.frame(
  letter = c("B", "C"),
  lon    = c(-115.9, -115.3), 
  lat    = c(29.9, 27.9)  
)

label_sf <- st_as_sf(label_coordinates, coords = c("lon", "lat"), crs = 4326)

context_map <- ggplot() +
  geom_sf(data = mexico, fill = "grey90", color = "black", size = 0.3) +
  coord_sf(xlim = c(-117, -113), ylim = c(26, 31)) +
  labs(x = NULL, y = NULL)+
  scale_x_continuous(breaks = -117:-113) +
  annotation_scale(location = "br",
                   width_hint = 0.3) +
  theme_linedraw() +
  theme(panel.background = element_rect(fill = "#def3fa", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.border = element_rect(color = "gray40", fill = NA),
        axis.text = element_text(color = "gray20")) + 
  geom_sf_text(data = label_sf, 
               aes(label = letter),
               color = "black",
               size = 5)


### Inset map 
box <- st_as_sfc(st_bbox(polygons))

inset_map <- ggplot() +
  geom_sf(data = world, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = box, fill = NA, color = "red", size = 0.5) +
  coord_sf(xlim = c(-120, -86), ylim = c(13, 34)) +
  labs(x = NULL, y = NULL)+
  theme_void() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA)) 

map_with_inset <- ggdraw() +
  draw_plot(context_map) +
  draw_plot(inset_map, x = 0.175, y = 0.022, width = 0.3, height = 0.3)

## Site maps  ----------------------------------------------------------------
 
### El Rosario
elrosario <- ggplot() +
  geom_sf(data = mexico, fill = "grey90", color = "black", size = 0.3) +
  geom_sf(data = polygons, fill = "#114b85", color = "#114b85", size = 0.3) +
  coord_sf(xlim = c(-115.9, -115.5), ylim = c(29.7, 30.05)) +
  scale_x_continuous(breaks = seq(-115.9, -115.5, by = 0.1)) +
  scale_y_continuous(breaks = seq(29.7, 30.05, by = 0.1)) +
  annotation_scale(location = "bl") +
  theme_linedraw() +
  theme(panel.background = element_rect(fill = "#def3fa", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.border = element_rect(color = "gray40", fill = NA),
        axis.text = element_text(color = "gray20")) 

### Isla Natividad
islanatividad <- ggplot() +
  geom_sf(data = mexico, fill = "grey90", color = "black", size = 0.3) +
  geom_sf(data = polygons, fill = "#114b85", color = "#114b85", size = 0.3) +
  coord_sf(xlim = c(-115.3, -114.9), ylim = c(27.75, 28.1), expand = FALSE) +
  scale_x_continuous(breaks = seq(-115.3, -114.9, by = 0.2)) +
  scale_y_continuous(breaks = seq(27.75, 28.1, by = 0.1)) +
  annotation_scale(location = "bl") +
  theme_linedraw() +
  theme(panel.background = element_rect(fill = "#def3fa", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.border = element_rect(color = "gray40", fill = NA),
        axis.text = element_text(color = "gray20")) 

## Combining map layouts  ----------------------------------------------------------------
community_plots <- plot_grid(elrosario, 
                             islanatividad, 
                             ncol = 1, 
                             labels = c("B", "C"),
                             align = "hv")

map_layout <- plot_grid(map_with_inset, 
                        community_plots,
                        ncol = 2,
                        rel_widths = c(1.25, 1),
                        labels = c("A", ""))

# EXPORT #######################################################################

## Export to a file called map.png using a 9X6 aspect ratio --------------------
  
ggsave(plot = map_layout,
       filename = here("results", "figs", "map.png"),bg = "white",
       width = 9,
       height = 6)

