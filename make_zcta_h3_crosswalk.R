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

overlay_h3_8 <- function(h3_5_zcta, zcta) {
  # expand h3_5 to h3_8
  h3_8 <- h3_5_zcta %>%
    map(h3_to_children, res = 8) %>%
    unlist()

  h3_8 <- bind_cols(tibble(h3 = h3_8),
                    as_tibble(h3::h3_to_geo_boundary_sf(h3_8))) %>%
    sf::st_as_sf() %>%
    sf::st_transform(5072)

  # subset to only h3_8s that overlap zcta
  h3_8_zcta <- st_join(h3_8, zcta, left = FALSE)
  return(h3_8_zcta)
}

get_zcta_weights <- function(h3_8_zcta, zcta) {
  # must have numeric value for st_interpolate_aw, so make val = 1
  # also remove ZCTA column, as st_interpolate_aw does not allow for any other non-numeric columns
  zcta <- zcta %>% mutate(weight = 1) %>% select(-zcta)

  # st_interpolate_aw, x = zcta, y = h3, extensive = TRUE makes weights sum to 1
  d_zcta_aw <- suppressWarnings( sf::st_interpolate_aw(zcta, h3_8_zcta, extensive = TRUE) )

  # we lose h3 ids, so add back
  d_zcta_aw <- bind_cols(h3_8_zcta %>% st_drop_geometry(), d_zcta_aw)
  return(d_zcta_aw)
}

get_h3_8_zcta_cw <- function(x, zctas) {
  zcta <- zctas[x,]
  print(zcta$zcta)
  # subset to only h3_5s that overlap zcta
  h3_5_zcta <- st_join(h3_5, zcta, left = FALSE) %>% st_drop_geometry()

  h3_8_zcta <- overlay_h3_8(h3_5_zcta$h3, zcta)

  # if zcta is outside all h3_8 cells...
  if (nrow(h3_8_zcta) < 1) {
    # find all neighboring h3_5 cells
    h3_5_zcta <- map(h3_5_zcta$h3, k_ring) %>% unlist()
    h3_5_zcta <- unique(h3_5_zcta)
    h3_8_zcta <- overlay_h3_8(h3_5_zcta, zcta)
  }

  d_zcta_aw <- get_zcta_weights(h3_8_zcta, zcta)

  if (sum(d_zcta_aw$weight) < 0.99) {
    # find all neighboring h3_5 cells
    h3_5_zcta <- map(h3_5_zcta$h3, k_ring) %>% unlist()
    h3_5_zcta <- unique(h3_5_zcta)
    h3_8_zcta <- overlay_h3_8(h3_5_zcta, zcta)
    d_zcta_aw <- get_zcta_weights(h3_8_zcta, zcta)
  }

  return(d_zcta_aw %>% select(zcta, h3, weight))
}

# 2010
zctas <- s3::s3_get('s3://geomarker/geometries/zctas_2010_contig_us_5072.rds') %>%
  readRDS() %>%
  rename(zcta = ZCTA5)

zcta_to_h3_2010 <- map(1:nrow(zctas), ~get_h3_8_zcta_cw(.x, zctas))
zcta_to_h3_2010 <- bind_rows(zcta_to_h3_2010)
saveRDS(zcta_to_h3_2010, 'zcta_2010_to_h3.rds')

# 2000
zctas <- s3::s3_get('s3://geomarker/geometries/zctas_2000_contig_us_5072.rds') %>%
  readRDS() %>%
  rename(zcta = ZCTA5)

zcta_to_h3_2000 <- map(1:nrow(zctas), ~get_h3_8_zcta_cw(.x, zctas))
zcta_to_h3_2000 <- bind_rows(zcta_to_h3_2000)
saveRDS(zcta_to_h3_2000, 'zcta_2000_to_h3.rds')

# 2020
zctas <- s3::s3_get('s3://geomarker/geometries/zctas_2020_contig_us_5072.rds') %>%
  readRDS() %>%
  rename(zcta = ZCTA5)

zcta_to_h3_2020 <- map(1:nrow(zctas), ~get_h3_8_zcta_cw(.x, zctas))
zcta_to_h3_2020 <- bind_rows(zcta_to_h3_2020)
saveRDS(zcta_to_h3_2020, 'zcta_2020_to_h3.rds')
