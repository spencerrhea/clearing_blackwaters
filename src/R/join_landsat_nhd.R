# Explore extracted pixels using python script 
library(tidyverse)
library(lubridate)
library(sf)
library(ggthemes)


#### Load in and filter pixels ####
all_fils <- list.files('clearing_blackwaters/data/icw_masked', full.names = T)
landsat_pix <- grep('[.]feather', all_fils, value = T)

dswe_fils <- list.files('clearing_blackwaters/data/icw_masked_dswe', full.names = T)


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

# Read in spectral and DSME data
landsat_pixes <- map_dfr(landsat_pix, read_feather_pull_nhd_id)
landsat_pixes_dswe <- map_dfr(dswe_fils, read_feather_pull_nhd_id) %>%
    select(-time)

landsat_pixes <- left_join(landsat_pixes, landsat_pixes_dswe)

# DSWE 0 = not water, 1 = open water (high confides), 2 = open water (low confides)
# 3 = vegetated water (high confides), 4 = vegetated water (low confides)
landsat_pix_dates <- landsat_pixes %>%
    mutate(date = substr(id, 13, nchar(id))) %>%
    mutate(date = as_date(date, '%y%m%d')) %>%
    mutate(mission = substr(id, 1, 4)) %>%
    mutate(ndvi = (Nir - Red) / (Nir + Red)) 

# NDVI by DSWE
landsat_pix_dates %>%
    filter(!is.na(dswe)) %>%
    mutate(dswe = as.character(dswe)) %>%
    ggplot(aes(x = dswe, y = ndvi)) +
    geom_boxplot()

# Water occurense v ndvi
landsat_pix_dates %>%
    filter(!is.na(dswe)) %>%
    mutate(dswe = as.numeric(dswe)) %>%
    ggplot(aes(x = dswe, y = occurrence)) +
    geom_boxplot()

# Map DSWE
# Seems like filtering out 0 pixels and mean DSWE greater than 1.8 removes edge pixels 
# removes about 16% of pixels
landsat_pix_keep <- landsat_pix_dates %>%
    filter(!is.na(dswe)) %>%
    filter(dswe != 0) %>%
    group_by(latitude, longitude) %>%
    summarise(dswe_mean = mean(dswe)) %>%
    filter(dswe_mean <= 1.8)

landsat_pix_keep_map <- landsat_pix_keep %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)

mapview::mapview(landsat_pix_keep_map, zcol = 'dswe_mean')

# Pixels are impacted by clouds, shadows, etc. about ~50% of the time 
landsat_pix_sum_by_pix <- landsat_pix_dates %>%
    mutate(nas = ifelse(is.na(Blue), 1, 0)) %>%
    group_by(latitude, longitude) %>%
    summarise(Blue = mean(Blue, na.rm = T),
              Green = mean(Green, na.rm = T),
              Red = mean(Red, na.rm = T),
              Nir = mean(Nir, na.rm = T),
              Swir1 = mean(Swir1, na.rm = T),
              Swir2 = mean(Swir2, na.rm = T),
              ndvi = mean(ndvi, na.rm = T),
              occurrence = mean(occurrence, na.rm = T),
              nas = sum(nas, na.rm = T),
              n = n()) %>%
    mutate(percent_impared = (nas/n)*100)

mean(landsat_pix_sum_by_pix$percent_impared)

hist(landsat_pix_sum_by_pix$percent_impared)

# Summarize by date and flowline 
landsat_pix_sum_by_nhd <- landsat_pix_dates %>%
    mutate(nas = ifelse(is.na(Blue), 1, 0)) %>%
    group_by(date, nhd_id) %>%
    summarise(Blue = mean(Blue, na.rm = T),
              Green = mean(Green, na.rm = T),
              Red = mean(Red, na.rm = T),
              Nir = mean(Nir, na.rm = T),
              Swir1 = mean(Swir1, na.rm = T),
              Swir2 = mean(Swir2, na.rm = T),
              ndvi = mean(ndvi, na.rm = T),
              occurrence = mean(occurrence, na.rm = T),
              nas = sum(nas, na.rm = T),
              n = n()) %>%
    mutate(percent_impared = (nas/n)*100)

#### Load in NHD ####
# Load flowlines 
icw_flowlines <- st_read('clearing_blackwaters/data/icw_flowlines') %>%
    st_zm()

# Create column for distance on line 
icw_flowlines_centroid <- icw_flowlines %>%
    mutate(geometry = st_centroid(geometry))

start_point <- tibble(lat = 35.550167,
                      long = -76.479833) %>%
    st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
    st_transform(., st_crs(icw_flowlines))

for(i in 1:nrow(icw_flowlines)) {
    icw_flowlines_centroid[i,'position'] <- st_distance(start_point, icw_flowlines_centroid[i,])
}

icw_flowlines_centroid <- icw_flowlines_centroid %>%
    as.data.frame() %>%
    select(permanent_, position)

new_tib <- tibble(permanent_ = icw_flowlines_centroid$permanent_,
                  position = icw_flowlines_centroid$position[,1])

icw_flowlines <- left_join(icw_flowlines, new_tib) %>%
    mutate(position = position/1000) %>%
    rename(nhd_id = permanent_)

# mapview::mapview(icw_flowlines, zcol = 'position')

#### Join flowlines to landsat data ####
nhd_landsat_sum <- left_join(icw_flowlines, landsat_pix_sum_by_nhd)

nhd_landsat_sum %>%
    group_by(nhd_id, position) %>%
    summarise(Blue = mean(Blue, na.rm = T),
              Green = mean(Green, na.rm = T),
              Red = mean(Red, na.rm = T),
              Nir = mean(Nir, na.rm = T),
              Swir1 = mean(Swir1, na.rm = T),
              Swir2 = mean(Swir2, na.rm = T),
              ndvi = mean(ndvi, na.rm = T),
              occurrence = mean(occurrence, na.rm = T)) %>%
    ggplot(aes(position, Green)) +
    geom_point()

nhd_landsat_sum %>%
    # filter(year(date) >= 2020) %>%
    # filter(nhd_id %in% c(141603111, 167451376, 166782446, 140599358)) %>%
    mutate(position = round(position, 2)) %>%
    mutate(position_ = as.factor(position)) %>%
    filter(!is.na(Blue)) %>%
    # filter(year(date) %in% 2019,
    #        month(date) %in% 6:10) %>%
    mutate(swii = Green/(Blue+Red)) %>% 
    filter(swii >= -2 & swii <= 2) %>%
    ggplot(aes(date, swii, color = position, group = position_)) +
    geom_line() +
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

