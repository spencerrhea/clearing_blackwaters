# Explore extracted pixels using python script 
library(tidyverse)
library(lubridate)
library(sf)
library(ggthemes)


#### Load in and filter pixels ####
all_fils <- list.files('clearing_blackwaters/data/coastal_blackwater_ls')
nhd_id_files <- grep('[.]feather', all_fils, value = T)

dswe_fils <- list.files('clearing_blackwaters/data/coastal_blackwater_ls_dswe/')


# function to append flowline id to dataframes
read_feather_pull_nhd_id <- function(file_path) {
    # flowline_id <- str_extract(file_path, 'icw_masked[0-9]*')
    flowline_id <- str_split_fixed(file_path, '/', n = Inf)[,4]
    
    # flowline_id <- str_remove(flowline_id, 'icw_masked')
    flowline_id <- str_remove(flowline_id, '[.]feather')
    
    file <- arrow::read_feather(file_path) %>%
        mutate(nhd_id = !!flowline_id)
    
    return(file)
}

read_sum_feather_by_nhd_id <- function(file_path) {
    
    flowline_id <- str_remove(file_path, '[.]feather')
    
    full_path_ls <- paste0('clearing_blackwaters/data/coastal_blackwater_ls/',
                          file_path)
    
    full_path_dswe <- paste0('clearing_blackwaters/data/coastal_blackwater_ls_dswe/',
                            file_path)
    
    file_dswe <- arrow::read_feather(full_path_dswe) %>%
        filter(!is.na(dswe)) %>%
        group_by(longitude, latitude) %>%
        summarise(mean_dswe = mean(dswe),
                  min_dswe = min(dswe)) %>%
        filter(min_dswe != 0) %>%
        # Selected this threshold by looking at the icw, should do a more systamitc look at 
        # this. BUt should beok for now 
        filter(mean_dswe <= 1.8) %>%
        ungroup()
    
    pixels_keep <- file_dswe %>%
        select(longitude, latitude)
    
    file_ls <- arrow::read_feather(full_path_ls) 
    
    final_sum_file <- left_join(pixels_keep, file_ls) %>%
        mutate(mission =  str_match(id, 'LT0[:digit:]{1}|LE0[:digit:]{1}|LC0[:digit:]{1}')) %>%
        mutate(date = ifelse(mission == 'LC08',
                             substr(as.character(id), 15, nchar(id)),
                             substr(as.character(id), 17, nchar(id)))) %>%
        mutate(date = as_date(date, '%Y%m%d')) %>%
        select(-longitude, -latitude) %>%
        mutate(nas = ifelse(is.na(Blue), 1, 0)) %>%
        group_by(date, mission) %>%
        summarise(Blue = mean(Blue, na.rm = T),
                  Green = mean(Green, na.rm = T),
                  Red = mean(Red, na.rm = T),
                  Nir = mean(Nir, na.rm = T),
                  Swir1 = mean(Swir1, na.rm = T),
                  Swir2 = mean(Swir2, na.rm = T),
                  occurrence = mean(occurrence, na.rm = T),
                  nas = sum(nas, na.rm = T),
                  n = n()) %>%
        mutate(percent_impared = (nas/n)*100) %>%
        mutate(nhd_id = !!flowline_id)

    
    return(final_sum_file)
}

# Read in spectral and DSME data
landsat_nhd <- map_dfr(nhd_id_files, read_sum_feather_by_nhd_id)

# Rivers that are looking good 
# scuppernong, milltail

#### Load in NHD ####
# Load flowlines 
white_oak_flowlines <- st_read('clearing_blackwaters/data/flowlines_of_interest/scuppernong') %>%
    st_zm()

mapview::mapview(white_oak_flowlines)

# Create column for distance on line 
white_oakflowlines_centroid <- white_oak_flowlines %>%
    mutate(geometry = st_centroid(geometry))

# WHITE OAK
# start_point <- tibble(lat = 34.681944,
#                       long = -77.119722) %>%
#     st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
#     st_transform(., st_crs(white_oak_flowlines))
# 
# st_write(start_point, 'clearing_blackwaters/data/starting_point/white_oak',
#          driver = 'ESRI Shapefile')

# scuppernong
start_point <- tibble(lat = 35.944194,
                      long = -76.322361) %>%
    st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
    st_transform(., st_crs(white_oak_flowlines))
# 
# st_write(start_point, 'clearing_blackwaters/data/starting_point/scuppernong',
#          driver = 'ESRI Shapefile')

# alligaor_river
# start_point <- tibble(lat = 35.890361,
#                       long = -76.009694) %>%
#     st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
#     st_transform(., st_crs(white_oak_flowlines))
# 
# st_write(start_point, 'clearing_blackwaters/data/starting_point/alligaor_river',
#          driver = 'ESRI Shapefile')

# cashie_river
# start_point <- tibble(lat = 35.937889,
#                       long = -76.730139) %>%
#     st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
#     st_transform(., st_crs(white_oak_flowlines))
# st_write(start_point, 'clearing_blackwaters/data/starting_point/cashie_river',
#          driver = 'ESRI Shapefile')

# miltail
# start_point <- tibble(lat = 35.874056,
#                       long = -76.007667) %>%
#     st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
#     st_transform(., st_crs(white_oak_flowlines))
# st_write(start_point, 'clearing_blackwaters/data/starting_point/milltail_creek',
#          driver = 'ESRI Shapefile')

# neuse_river
# start_point <- tibble(lat = 35.023917,
#                       long = -76.952417) %>%
#     st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
#     st_transform(., st_crs(white_oak_flowlines))
# st_write(start_point, 'clearing_blackwaters/data/starting_point/neuse_river',
#          driver = 'ESRI Shapefile')

# new_river
# start_point <- tibble(lat = 34.542250,
#                       long = -77.345139) %>%
#     st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
#     st_transform(., st_crs(white_oak_flowlines))
# st_write(start_point, 'clearing_blackwaters/data/starting_point/new_river',
#          driver = 'ESRI Shapefile')

# north_river
# start_point <- tibble(lat = 36.143028,
#                       long = -75.890194) %>%
#     st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
#     st_transform(., st_crs(white_oak_flowlines))
# st_write(start_point, 'clearing_blackwaters/data/starting_point/north_river',
#          driver = 'ESRI Shapefile')

# chowan_river
# start_point <- tibble(lat = 36.012639,
#                       long = -76.658917) %>%
#     st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
#     st_transform(., st_crs(white_oak_flowlines))
# st_write(start_point, 'clearing_blackwaters/data/starting_point/chowan_river',
#          driver = 'ESRI Shapefile')

# pamlico_river
# start_point <- tibble(lat = 35.428583,
#                       long = -76.817250) %>%
#     st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
#     st_transform(., st_crs(white_oak_flowlines))
# st_write(start_point, 'clearing_blackwaters/data/starting_point/pamlico_river',
#          driver = 'ESRI Shapefile')

# # pasquotank_river
# start_point <- tibble(lat = 36.139972,
#                       long = -76.001694) %>%
#     st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
#     st_transform(., st_crs(white_oak_flowlines))
# st_write(start_point, 'clearing_blackwaters/data/starting_point/pasquotank_river',
#          driver = 'ESRI Shapefile')

# perquimans_river
# start_point <- tibble(lat = 36.098806,
#                       long = -76.276500) %>%
#     st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
#     st_transform(., st_crs(white_oak_flowlines))
# st_write(start_point, 'clearing_blackwaters/data/starting_point/perquimans_river',
#          driver = 'ESRI Shapefile')

# pungo_river 
# start_point <- tibble(lat = 35.388972,
#                       long = -76.542833) %>%
#     st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
#     st_transform(., st_crs(white_oak_flowlines))
# st_write(start_point, 'clearing_blackwaters/data/starting_point/pungo_river',
#          driver = 'ESRI Shapefile')

# queens_creek
# start_point <- tibble(lat = 34.665194,
#                       long = -77.143778) %>%
#     st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
#     st_transform(., st_crs(white_oak_flowlines))
# st_write(start_point, 'clearing_blackwaters/data/starting_point/queens_creek',
#          driver = 'ESRI Shapefile')

# trent_river
# start_point <- tibble(lat = 35.099944,
#                       long = -77.031750) %>%
#     st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
#     st_transform(., st_crs(white_oak_flowlines))
# st_write(start_point, 'clearing_blackwaters/data/starting_point/trent_river',
#          driver = 'ESRI Shapefile')

# icw
start_point <- tibble(lat = 35.547944,
                      long = -76.477917) %>%
    st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
    st_transform(., st_crs(white_oak_flowlines))
st_write(start_point, 'clearing_blackwaters/data/starting_point/icw',
         driver = 'ESRI Shapefile')

for(i in 1:nrow(white_oak_flowlines)) {
    white_oakflowlines_centroid[i,'position'] <- st_distance(start_point, white_oakflowlines_centroid[i,])
}

white_oakflowlines_centroid <- white_oakflowlines_centroid %>%
    as.data.frame() %>%
    select(comid, position)

new_tib <- tibble(comid = white_oakflowlines_centroid$comid,
                  position = white_oakflowlines_centroid$position[,1])

white_oak_flowlines <- left_join(white_oak_flowlines, new_tib) %>%
    mutate(position = position/1000) %>%
    rename(nhd_id = comid)

landsat_nhd_wo <- landsat_nhd %>%
    filter(nhd_id %in% white_oak_flowlines$nhd_id) %>%
    mutate(nhd_id = as.numeric(nhd_id))

mapview::mapview(white_oak_flowlines, zcol = 'position')

#### Join flowlines to landsat data ####
nhd_landsat_sum <- left_join(white_oak_flowlines, landsat_nhd_wo)

# nhd_landsat_sum %>%
#     group_by(nhd_id, position) %>%
#     summarise(Blue = mean(Blue, na.rm = T),
#               Green = mean(Green, na.rm = T),
#               Red = mean(Red, na.rm = T),
#               Nir = mean(Nir, na.rm = T),
#               Swir1 = mean(Swir1, na.rm = T),
#               Swir2 = mean(Swir2, na.rm = T),
#               occurrence = mean(occurrence, na.rm = T)) %>%
#     ggplot(aes(position, Green)) +
#     geom_point()

nhd_landsat_sum %>%
    # filter(position <= 18) %>%
    # filter(nhd_id %in% c(141603111, 167451376, 166782446, 140599358)) %>%
    mutate(position = round(position, 3)) %>%
    mutate(position_ = as.factor(position)) %>%
    filter(!is.na(Blue)) %>%
    # filter(year(date) >= 2017,
    #        year(date) < 2018
    #        # month(date) %in% 6:10
    #        ) %>%
    filter(month(date) >= 8,
           month(date) < 11) %>%
    mutate(swii = Green/(Blue+Red)) %>% 
    filter(swii >= -2 & swii <= 2) %>%
    ggplot(aes(date, swii, color = position, group = position_)) +
    geom_line() +
    # scale_x_date(date_labels="%y-%b",date_breaks  ="1 month") +
    # scale_x_date(date_labels="%y",date_breaks  ="1 years") +
    viridis::scale_color_viridis() +
    theme_few()

nhd_landsat_sum %>%
    filter(position <= 18) %>%
    # filter(nhd_id %in% c(141603111, 167451376, 166782446, 140599358)) %>%
    mutate(position = round(position, 2)) %>%
    filter(!is.na(Blue)) %>%
    # filter(year(date) > 2010
    #        # month(date) %in% 6:10
    #        ) %>%
    mutate(swii = Green/(Blue+Red)) %>% 
    filter(swii >= -2 & swii <= 2) %>%
    mutate(month = month(date),
           year = year(date)) %>%
    group_by(position, month, year) %>%
    summarise(swii_mean = mean(swii, na.rm = T),
              green_mean = mean(Green, na.rm = T),
              blue_mean = mean(Blue, na.rm = T),
              red_mean = mean(Red, na.rm = T)) %>%
    mutate(position_ = as.factor(position)) %>%
    mutate(epoch = case_when(year >= 1985 & year <= 1995 ~ 1985,
                             year %in% 1996:2005 ~ 1995,
                             year %in% 2006:2015 ~ 2005,
                             year %in% 2016:2025 ~ 2015)) %>%
    group_by(position, epoch, month) %>%
    summarise(swii_mean = mean(swii_mean, na.rm = T),
              green_mean = mean(green_mean, na.rm = T),
              blue_mean = mean(blue_mean, na.rm = T),
              red_mean = mean(red_mean, na.rm = T)) %>%
    filter(position == 10.993926) %>%
    ggplot(aes(month, swii_mean, color = epoch, group = epoch)) +
    geom_line() +
    # geom_boxplot() +
    # scale_x_date(date_labels="%y-%b",date_breaks  ="3 month") +
    viridis::scale_color_viridis() +
    theme_few()


### Old crap ####

look <- terra::rast('clearing_blackwaters/TEST_MASK_2.tif')
look_ <- raster::raster('clearing_blackwaters/TEST_MASK_2.tif')

look <- arrow::read_feather('clearing_blackwaters/data/icw_masked/ ')

terra::plot(look)
mapview::mapview(look_, zcol = 'Green')

all_fils <- list.files('clearing_blackwaters/data/')
landsat_pix <- grep('[.]feather', all_fils, value = T)
landsat_pix <- grep('^icw', landsat_pix, value = T)

all_landsat_pix <- map_dfr(paste0('clearing_blackwaters/data/', landsat_pix), arrow::read_feather)

all_landsat_pix_dates <- all_landsat_pix %>%
    mutate(date = substr(id, 13, nchar(id))) %>%
    mutate(date = as_date(date, '%y%m%d')) %>%
    mutate(mission = substr(id, 1, 4))

all_landsat_pix_dates %>%
    filter(longitude == all_landsat_pix_dates$longitude[1]) %>%
    filter(year(date) %in% c(2016, 2015)) %>%
    mutate(ndci = (Nir - Red)/(Nir+Red)) %>%
    ggplot(aes(date, Green, color = latitude)) +
    geom_point() +
    scale_x_date(date_labels =  "%b %Y")

icw_pixels <- read_csv('clearing_blackwaters/data/icw_pixels.csv')

icw_pixels_sf <- all_landsat_pix_dates %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)

mapview::mapview(icw_pixels_sf) +icw_lines

look <- arrow::read_feather('clearing_blackwaters/data/136046395.feather')

look %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>%
    mapview::mapview(., zcol = 'Green')

