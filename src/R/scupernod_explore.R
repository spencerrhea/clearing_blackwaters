library(tidyverse)
library(sf)

# Load in flowlines
scuppernong_flowlines <- st_read('clearing_blackwaters/data/flowlines_of_interest/scuppernong') %>%
    st_zm()

# Load in landsat data
all_fils <- list.files('clearing_blackwaters/data/coastal_blackwater_ls', full.names = T)
landsat_pix <- grep('[.]feather', all_fils, value = T)

dswe_fils <- list.files('clearing_blackwaters/data/coastal_blackwater_ls_dswe/', full.names = T)

scuppernong_dswe_files <- grep(paste0(paste0(scuppernong_flowlines$comid, '.feather'), collapse = '|'), dswe_fils,
     value = T)

scuppernong_dswe <- map_dfr(scuppernong_dswe_files, arrow::read_feather)

scuppernong_dswe_sum <-scuppernong_dswe %>%
    group_by(latitude, longitude) %>%
    filter(dswe != 0) %>%
    summarise(mean_dswe = mean(dswe, na.rm = T),
              min_dswe = min(dswe, na.rm = T)) %>%
    mutate(min_dswe = ifelse(is.infinite(min_dswe), NA, min_dswe)) %>%
    filter(mean_dswe <= 1.8) %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)

mapview::mapview(scuppernong_dswe_sum, zcol = 'mean_dswe')

start_point <- tibble(lat = 35.944194,
                      long = -76.322361) %>%
    st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
    st_transform(., st_crs(white_oak_flowlines))

# Create column for distance on line 
scuppernong_centroid <- scuppernong_flowlines %>%
    mutate(geometry = st_centroid(geometry))

for(i in 1:nrow(scuppernong_flowlines)) {
    scuppernong_centroid[i,'position'] <- st_distance(start_point, scuppernong_centroid[i,])
}

scuppernong_centroid <- scuppernong_centroid %>%
    as.data.frame() %>%
    select(comid, position)

new_tib <- tibble(comid = scuppernong_centroid$comid,
                  position = scuppernong_centroid$position[,1])

scuppernong_flowlines <- left_join(scuppernong_flowlines, new_tib) %>%
    mutate(position = position/1000) %>%
    rename(nhd_id = comid)

landsat_nhd_wo <- landsat_nhd %>%
    filter(nhd_id %in% scuppernong_flowlines$nhd_id) %>%
    mutate(nhd_id = as.numeric(nhd_id))

mapview::mapview(scuppernong_flowlines, zcol = 'position')


landsat_nhd_scup <- landsat_nhd %>%
    filter(nhd_id %in% scuppernong_flowlines$nhd_id) %>%
    mutate(nhd_id = as.numeric(nhd_id))

nhd_landsat_sum <- left_join(scuppernong_flowlines, landsat_nhd_scup)

nhd_landsat_sum %>%
    filter(position <= 18) %>%
    # filter(nhd_id %in% c(141603111, 167451376, 166782446, 140599358)) %>%
    mutate(position = round(position, 2)) %>%
    mutate(position_ = as.factor(position)) %>%
    filter(!is.na(Blue)) %>%
    # filter(year(date) %in% 2016:2019
    #        # month(date) %in% 6:10
    # ) %>%
    mutate(swii = Green/(Blue+Red)) %>% 
    filter(swii >= -2 & swii <= 2) %>%
    ggplot(aes(date, swii, color = position, group = position_)) +
    geom_line() +
    # scale_x_date(date_labels="%y-%b",date_breaks  ="3 month") +
    viridis::scale_color_viridis() +
    theme_few()


nhd_landsat_sum_ <- nhd_landsat_sum %>%
    filter(position <= 18) %>%
    # filter(nhd_id %in% c(141603111, 167451376, 166782446, 140599358)) %>%
    mutate(position = round(position, 2)) %>%
    mutate(position_ = as.factor(position)) %>%
    filter(!is.na(Blue)) %>%
    filter(year(date) %in% 2016:2019
           # month(date) %in% 6:10
    ) %>%
    mutate(swii = Green/(Blue+Red)) %>% 
    filter(swii >= -2 & swii <= 2) %>%
    group_by(nhd_id) %>%
    summarise(mean_swii = mean(swii, na.rm = T)) 

mapview::mapview(nhd_landsat_sum_, zcol = 'mean_swii')


