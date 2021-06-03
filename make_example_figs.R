library(tidyverse)
library(h3)
library(sf)

# download h3_8_compact, expand to res 5, convert to sf
h3_5 <- readRDS('us_h3_8_compact_hex_ids.rds') %>%
  map(h3_to_children, res = 5) %>%
  unlist()

h3_5 <-
  bind_cols(tibble(h3 = h3_5),
            as_tibble(h3::h3_to_geo_boundary_sf(h3_5))) %>%
  sf::st_as_sf() %>%
  sf::st_transform(5072)

zcta <- tigris::zctas(year = 2010, starts_with = '45229') %>%
  st_transform(5072) %>%
  select(zcta = ZCTA5CE10)

# subset to only h3_5s that overlap zcta
h3_5_zcta <- st_join(h3_5, zcta, left = FALSE)
mapview::mapview(h3_5_zcta, alpha.regions = 0.3, legend = NULL) + mapview::mapview(zcta, alpha.regions = 1)

# expand h3_5 to h3_8
h3_8 <- h3_5_zcta$h3 %>%
  map(h3_to_children, res = 8) %>%
  unlist()

h3_8 <- bind_cols(tibble(h3 = h3_8),
                  as_tibble(h3::h3_to_geo_boundary_sf(h3_8))) %>%
  sf::st_as_sf() %>%
  sf::st_transform(5072)

mapview::mapview(h3_8, col.regions = 'blue', alpha.regions = 0.3, legend = NULL) +
  mapview::mapview(zcta, alpha.regions = 1)

# subset to only h3_8s that overlap zcta
h3_8_zcta <- st_join(h3_8, zcta, left = FALSE)

zcta <- zcta %>% mutate(weight = 1) %>% select(-zcta)

# st_interpolate_aw, x = zcta, y = h3, extensive = TRUE makes weights sum to 1
d_zcta_aw <- suppressWarnings( sf::st_interpolate_aw(zcta, h3_8_zcta, extensive = TRUE) )

# we lose h3 ids, so add back
d_zcta_aw <- bind_cols(h3_8_zcta %>% st_drop_geometry(), d_zcta_aw) %>%
  st_as_sf()

mapview::mapview(d_zcta_aw, zcol = "weight", alpha.regions = 0.7,
                 col.regions = RColorBrewer::brewer.pal(9, "Blues")[3:9],
                 layer.name = 'weight') +
  mapview::mapview(zcta, alpha.regions = 0, lwd = 2, alpha = 1, legend = NULL)
