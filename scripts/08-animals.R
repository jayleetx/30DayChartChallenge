# memes
# https://www.facebook.com/turneriannightmare/photos
# https://scontent.fhio2-1.fna.fbcdn.net/v/t1.6435-9/52396379_1985875685051344_5065593439467339776_n.jpg?_nc_cat=102&ccb=1-3&_nc_sid=730e14&_nc_ohc=VKw50vkHbe8AX_wDVx_&_nc_ht=scontent.fhio2-1.fna&oh=c23712f9cd2397afc0a0c47160026e8a&oe=60932075
# https://scontent.fhio2-2.fna.fbcdn.net/v/t1.18169-9/11255009_1477108349261416_2196735587869267584_n.jpg?_nc_cat=110&ccb=1-3&_nc_sid=9267fe&_nc_ohc=o3UocMldF_AAX-MoxVH&_nc_ht=scontent.fhio2-2.fna&oh=ca31c1b7c69cf49aa385c0b2e8ae3ffb&oe=6093FC8E
# https://scontent.fhio2-2.fna.fbcdn.net/v/t31.18172-8/12466144_1499137260391858_1733183537553541732_o.jpg?_nc_cat=100&ccb=1-3&_nc_sid=9267fe&_nc_ohc=BPvtVomFP_cAX8M1S8P&_nc_oc=AQkFoNpOD3kMs_fF_RMsIGnJJVA2T5k_v1jVv9ALltjDTyqPEcsQV3BEw8JCYhJ0VCI&_nc_ht=scontent.fhio2-2.fna&oh=e0b2ecf6deff24252ff4bc19c85e4b3d&oe=60958AE4
# https://scontent.fhio2-2.fna.fbcdn.net/v/t31.18172-8/12471610_1499137290391855_3905899023989326688_o.jpg?_nc_cat=110&ccb=1-3&_nc_sid=9267fe&_nc_ohc=mrRq19R_vCEAX879p2g&_nc_ht=scontent.fhio2-2.fna&oh=dc366ab3275dac62c76298d1260bd443&oe=60937FD9
# https://scontent.fhio2-2.fna.fbcdn.net/v/t31.18172-8/15972754_1627738264198423_7453609266474720144_o.jpg?_nc_cat=107&ccb=1-3&_nc_sid=9267fe&_nc_ohc=q4PhwBTu-RcAX93Fz-u&_nc_ht=scontent.fhio2-2.fna&oh=cf0403d4d27723b3e7172b00a248bee6&oe=60947EE8
# https://scontent.fhio2-2.fna.fbcdn.net/v/t1.18169-9/17190390_1650505868588329_5087812215883932310_n.jpg?_nc_cat=104&ccb=1-3&_nc_sid=9267fe&_nc_ohc=F_cJazViGYkAX-vsMw3&_nc_ht=scontent.fhio2-2.fna&oh=cf27402b7c2d9803281be78c323b9c39&oe=6094D4B8

# setup packages ----

library(tidyverse)
library(sf)
library(here)

# functions ----

# get one bounding box for multiple objects
# basically sticks them all together and gets the relevant min/max
get_minimal_bbox <- function(...) {
  list(...) %>%
    lapply(sf::st_bbox) %>%
    lapply(as.list) %>%
    lapply(data.frame) %>%
    bind_rows() %>%
    summarize(dplyr::across(.cols = ends_with("min"), min),
              dplyr::across(.cols = ends_with("max"), max)) %>%
    unlist(x = .[1, ])
}

# pad bounding box so content isn't right up on edge
# and also cali isn't cut off funky
pad_bbox <- function(box, pct, direction = c("xmin", "ymin", "xmax", "ymax")) {
  
  pad <- c(pct*(box["xmin"] - box["xmax"]),
           pct*(box["ymin"] - box["ymax"]),
           pct*(box["xmax"] - box["xmin"]),
           pct*(box["ymax"] - box["ymin"]))
  
  box[direction] <- box[direction] + pad[direction]
  
  box
}

# data and layers ----

# grouse habitats from USGS

# https://www.sciencebase.gov/catalog/item/56f96693e4b0a6037df06034
current_habitat <- st_read(here('data', '08-animals', 
                                'grsg_current_range'))

# https://www.sciencebase.gov/catalog/item/57e415cce4b090825005b6ac
historic_habitat <- st_read(here('data', '08-animals', 
                                 'grsg_historic_range')) %>%
  st_transform(st_crs(current_habitat))

# https://www.sciencebase.gov/catalog/item/56f96b30e4b0a6037df06216
# management_zones <- st_read(here('data', '08-animals', 
#                                  'grsg_management_zones')) %>%
#   st_transform(st_crs(current_habitat))

# https://www.sciencebase.gov/catalog/item/5f6e3d3382ce38aaa249defc
# clip these polygons to the higher-res current range boundaries
subpops <- st_read(here('data', '08-animals', 
                               'grsg_subpopulations')) %>%
  group_by(SUBPOP, SUBPOP_NO, SG_SP_ID) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup() %>%
  st_transform(st_crs(current_habitat)) %>%
  st_intersection(st_make_valid(current_habitat))

# get a minimal bounding box for these layers
bounding <- get_minimal_bbox(current_habitat,
                             historic_habitat,
                             management_zones) %>%
  pad_bbox(.1)

#state/province layers, bounded to GRSG habitat area

# https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
us_states <- st_read(here('data', '08-animals', 
                          'us_state_boundaries')) %>%
  st_transform(st_crs(current_habitat)) %>%
  st_crop(bounding)

# https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2016-eng.cfm
canada_provterr <- st_read(here('data', '08-animals', 
                                'ca_provterr_boundaries')) %>%
  st_transform(st_crs(current_habitat)) %>%
  st_crop(bounding)

ggplot() + 
  geom_sf(data = historic_habitat, fill = "gray",
          color = NA) +
  geom_sf(data = current_habitat, fill = "forestgreen",
          color = NA) +
  geom_sf(data = subpops, aes(fill = SUBPOP), show.legend = FALSE) +
  geom_sf(data = us_states, fill = NA) +
  geom_sf(data = canada_provterr, fill = NA) +
  # geom_sf(data = management_zones, fill = NA,
  #         color = "magenta", lwd = 1.25, alpha = 0.1) +
  labs(title = "Subpopulations of Greater Sage-grouse",
       subtitle = "Historical habitat range in gray, current habitat range\nwith no associated subpopulation in dark green",
       caption = paste("Base maps from the U.S. Census Bureau and Statistics Canada",
                       "Habitat and subpopulation data from the U.S. Geological Survey",
                       "",
                       sep = "\n")) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption.position = "plot")

# save images
ggsave(here('img', '08-animals.svg'), 
       width = 6, height = 6)
ggsave(here('img', '08-animals.png'), 
       width = 6, height = 6)
