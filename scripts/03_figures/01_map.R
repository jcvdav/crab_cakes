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
world  <- ne_countries(scale = "large", returnclass = "sf")

## Reference map  ----------------------------------------------------------------
label_coordinates <- data.frame(
  letter = c("B", "C"),
  lon    = c(-115.7, -115.05), 
  lat    = c(29.85, 27.87)  
)

label_sf <- st_as_sf(label_coordinates, coords = c("lon", "lat"), crs = 4326)

context_map <- ggplot() +
  geom_sf(data = mexico, fill = "grey90", color = "grey80", size = 0.3) +
  geom_sf(data = polygons, fill = "#114b85", color = "#114b85", size = 0.3) +
  coord_sf(xlim = c(-117, -113), ylim = c(26, 31), expand = FALSE) +
  labs(x = NULL, y = NULL)+
  annotation_scale(location = "br", width_hint = 0.3, text_col = "black", bar_cols = "white", text_family = "serif") +
  theme(
    text = element_text(size = 12, family = "serif"), 
    plot.title = element_text(size = 14, face = "bold", color = "black"),
    axis.title = element_blank(),
    legend.position = "right",
    panel.background = element_rect(fill = "#def3fa", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color = "gray40", fill = NA),
    axis.text = element_text(color = "gray20"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) + 
  geom_sf_text(
    data = label_sf, 
    aes(label = letter),
    color = "black",       
    fontface = "bold",  
    family = "serif",
    size = 8              
  )


### Inset map 
box <- st_as_sfc(st_bbox(polygons))

inset_map <- ggplot() +
  geom_sf(data = mexico, fill = "grey80", color = "grey80", size = 0.3) +
  geom_sf(data = world, fill = "gray95", color = "grey80", size = 0.3) +
  geom_sf(data = box, fill = NA, color = "red", size = 1.2) +
  coord_sf(xlim = c(-120, -86), ylim = c(13, 34), expand = FALSE) +
  labs(x = NULL, y = NULL)+
  theme(
    text = element_text(size = 12, family = "serif"), 
    plot.title = element_text(size = 14, face = "bold", color = "black"),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    plot.background = element_blank(),
    panel.border = element_rect(color = "gray40", fill = NA),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) 

map_with_inset <- ggdraw() +
  draw_plot(context_map) +
  draw_plot(inset_map, x = 0.22, y = 0.08, width = 0.3, height = 0.3)

## Site maps  ----------------------------------------------------------------
 
### El Rosario
elrosario <- ggplot() +
  geom_sf(data = mexico, fill = "grey90", color = "grey80", size = 0.3) +
  geom_sf(data = polygons, fill = "#114b85", color = "#114b85", size = 0.3) +
  coord_sf(xlim = c(-115.9, -115.5), ylim = c(29.7, 30.05), expand = FALSE) +
  labs(title = "El Rosario", x = NULL, y = NULL)+
  annotation_scale(location = "bl", width_hint = 0.3, text_col = "black", bar_cols = "white", text_family = "serif") +
  theme(
    text = element_text(size = 12, family = "serif"), 
    plot.title = element_text(size = 14, face = "bold", color = "black"),
    axis.title = element_blank(),
    legend.position = "right",
    panel.background = element_rect(fill = "#def3fa", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color = "gray40", fill = NA),
    axis.text = element_text(color = "gray20"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 

### Isla Natividad
islanatividad <- ggplot() +
  geom_sf(data = mexico, fill = "grey90", color = "grey80", size = 0.3) +
  geom_sf(data = polygons, fill = "#114b85", color = "#114b85", size = 0.3) +
  coord_sf(xlim = c(-115.3, -114.9), ylim = c(27.75, 28.1), expand = FALSE) +
  labs(title = "Isla Natividad", x = NULL, y = NULL)+
  annotation_scale(location = "bl", width_hint = 0.3, text_col = "black", bar_cols = "white", text_family = "serif") +
  theme(
    text = element_text(size = 12, family = "serif"), 
    plot.title = element_text(size = 14, face = "bold", color = "black"),
    axis.title = element_blank(),
    legend.position = "right",
    panel.background = element_rect(fill = "#def3fa", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color = "gray40", fill = NA),
    axis.text = element_text(color = "gray20"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 

## Combining map layouts  ----------------------------------------------------------------
community_plots <- (elrosario / islanatividad)

map_layout <- wrap_elements(map_with_inset) | community_plots + 
  plot_layout(widths = c(2, 1))

final_map <- map_layout + 
  plot_annotation(
    tag_levels = list(c("A", "B", "C")), 
    title = "Location of community based marine reserves open to fishing", 
    theme = theme( 
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5, family = "serif"),
      plot.tag = element_text(size = 14, face = "bold", family = "serif"),
  #    plot.margin = margin(10, 10, 5, 10),
      text = element_text(family = "serif")
    )
  )

# EXPORT #######################################################################

## Export to a file called map.png using a 9X6 aspect ratio --------------------
  
ggsave(here("results", "figs", "map.png"), plot = final_map, width = 9, height = 6)

