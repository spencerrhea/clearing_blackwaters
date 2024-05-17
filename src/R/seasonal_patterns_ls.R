# Explore seasaonl pattern in reflecrtacens

# Explore extracted pixels using python script 
library(tidyverse)
library(lubridate)
library(sf)
library(ggthemes)


#### Load in and filter pixels ####
all_fils <- list.files('clearing_blackwaters/data/coastal_blackwater_ls')
nhd_id_files <- grep('[.]feather', all_fils, value = T)

dswe_fils <- list.files('clearing_blackwaters/data/coastal_blackwater_ls_dswe/', full.names = T)


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




rivers <- list.files('clearing_blackwaters/data/flowlines_of_interest')

# all_years_months <- tibble(year = sort(rep(1984:2020, 12)),
#                            month = rep(1:12, length(1984:2020)))

nhd_ids <- tibble(river = c('alligaor_river', 'cashie_river', 'chowan_river',
                            'icw', 
                            'milltail_creek', 'neuse_river', 'new_river',
                            'north_river', 'pamlico_river', 'pasquotank_river',
                            'perquimans_river', 'pungo_river', 'queens_creek',
                            'scuppernong', 'trent_river', 'white_oak'),
                  nhd_id = c(10499579, 10452246, 10462171, 167451376, 10482271, 
                             11239391, 10519208, 1025584, 166737440,
                             10479641, 10479885, 1968301, 1008270,
                             10484529, 10977233, 10961498))

for(i in 1:length(rivers)){
    
    this_river <- rivers[i]
    
    # Read in flowlines 
    flowlines <- st_read(paste0('clearing_blackwaters/data/flowlines_of_interest/',
                                this_river))
    
    # Readin starting point to calc distance from 
    start_point <- st_read(paste0('clearing_blackwaters/data/starting_point/',
                                  this_river))
    
    # Create centroids to calc ditance between starting point and lines 
    flowlines_centroid <- flowlines %>%
        mutate(geometry = st_centroid(geometry))
    
    # Get distance from each flowlines centroid and starting point 
    for(p in 1:nrow(flowlines)) {
        flowlines_centroid[p,'position'] <- st_distance(start_point, flowlines_centroid[p,])
    }
    
    # create table with distances 
    flowlines_centroid <- flowlines_centroid %>%
        as.data.frame() %>%
        select(comid, position)
    
    new_tib <- tibble(comid = flowlines_centroid$comid,
                      position = flowlines_centroid$position[,1])
    
    flowlines <- left_join(flowlines, new_tib) %>%
        mutate(position = position/1000) %>%
        rename(nhd_id = comid) %>%
        mutate(position = round(position, 2))
    
    landsat_nhd_rel <- landsat_nhd %>%
        filter(nhd_id %in% flowlines$nhd_id) %>%
        mutate(nhd_id = as.numeric(nhd_id))
    
    mapview::mapview(flowlines, zcol = 'position')
    
    if(this_river == 'icw'){
        flowlines <- flowlines %>%
            mutate(nhd_id = as.numeric(nhd_id))
    }
    # Join flowliens to landsat data 
    nhd_landsat_sum <- left_join(flowlines, landsat_nhd_rel)
    
    # Remove lines that have little to no measuremtns 
    remove_lines <- nhd_landsat_sum %>%
        as.data.frame() %>%
        select(-geometry) %>%
        group_by(position, nhd_id) %>%
        summarise(n = n()) 
    
    keep_lines <- remove_lines %>%
        filter(n >= max(remove_lines$n)/2) %>%
        pull(nhd_id)
    
    # Whole time series
    time_series <- nhd_landsat_sum %>%
        # filter(position <= 18) %>%
        # filter(nhd_id %in% !!keep_lines) %>%
        # filter(nhd_id %in% c(141603111, 167451376, 166782446, 140599358)) %>%
        mutate(position = round(position, 2)) %>%
        mutate(position_ = as.factor(position)) %>%
        filter(!is.na(Blue)) %>%
        # filter(year(date) > 2010
        #        # month(date) %in% 6:10
        #        ) %>%
        mutate(swii = Green/(Blue+Red)) %>% 
        filter(swii >= -2 & swii <= 2) %>%
        mutate(month = month(date),
               year = year(date)) %>%
        # filter(year(date) %in% 2010:2020) %>%
        # full_join(., all_years_months) %>%
        ggplot(aes(date, swii, color = position, group = position_)) +
        geom_line() +
        # scale_x_date(date_labels="%y-%b",date_breaks  ="3 month") +
        scale_x_date(date_labels="%Y",date_breaks  ="2 years") +
        viridis::scale_color_viridis() +
        theme_few() +
        theme(axis.text.x = element_text(angle = 90)) +
        ggtitle(this_river) +
        labs(x = 'Year',
             y = 'Salinity Index \n [Green/(Blue+Red)]')
    
    png(paste0('clearing_blackwaters/plots/SWI_timeseries/', this_river, '.png'),
        width = 800, height =400, units = 'px')
    print(time_series)
    dev.off()
    
    # Short time series 
    time_series <- nhd_landsat_sum %>%
        # filter(position <= 18) %>%
        # filter(nhd_id %in% !!keep_lines) %>%
        # filter(nhd_id %in% c(141603111, 167451376, 166782446, 140599358)) %>%
        mutate(position = round(position, 2)) %>%
        mutate(position_ = as.factor(position)) %>%
        filter(!is.na(Blue)) %>%
        # filter(year(date) > 2010
        #        # month(date) %in% 6:10
        #        ) %>%
        mutate(swii = Green/(Blue+Red)) %>% 
        filter(swii >= -2 & swii <= 2) %>%
        mutate(month = month(date),
               year = year(date)) %>%
        filter(year %in% 2015:2020) %>%
        # filter(year(date) %in% 2010:2020) %>%
        # full_join(., all_years_months) %>%
        ggplot(aes(date, swii, color = position, group = position_)) +
        geom_line() +
        # scale_x_date(date_labels="%y-%b",date_breaks  ="3 month") +
        scale_x_date(date_labels="%Y",date_breaks  ="2 years") +
        viridis::scale_color_viridis() +
        theme_few() +
        theme(axis.text.x = element_text(angle = 90, size = 12)) +
        ggtitle(this_river) +
        labs(x = 'Year',
             y = 'Salinity Index \n [Green/(Blue+Red)]')
    
    png(paste0('clearing_blackwaters/plots/SWI_timeseries_short/', this_river, '.png'),
        width = 800, height =400, units = 'px')
    print(time_series)
    dev.off()
    
    
    seasons <- nhd_landsat_sum %>%
        filter(nhd_id == !! nhd_ids$nhd_id[i]) %>%
        # filter(position <= 18) %>%
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
        mutate(epoch = as.factor(epoch)) %>%
        group_by(position, epoch, month) %>%
        summarise(swii_mean = mean(swii_mean, na.rm = T),
                  green_mean = mean(green_mean, na.rm = T),
                  blue_mean = mean(blue_mean, na.rm = T),
                  red_mean = mean(red_mean, na.rm = T)) %>%
        mutate(month = as.factor(month)) %>%
        ggplot(aes(month, swii_mean, color = epoch, group = epoch)) +
        geom_line() +
        # geom_boxplot() +
        # scale_x_date(date_labels="%b",date_breaks  ="1 month") +
        scale_color_brewer(palette = 'Set1') +
        theme_few()
    
    png(paste0('clearing_blackwaters/plots/SWI_seasons/', this_river, '.png'),
        width = 800, height =400, units = 'px')
    print(seasons)
    dev.off()
}

#### Play ####
nhd_landsat_sum %>%
    # filter(position <= 18) %>%
    # filter(nhd_id %in% !!keep_lines) %>%
    # filter(nhd_id %in% c(141603111, 167451376, 166782446, 140599358)) %>%
    mutate(position = round(position, 2)) %>%
    mutate(position_ = as.factor(position)) %>%
    filter(!is.na(Blue)) %>%
    # filter(year(date) > 2010
    #        # month(date) %in% 6:10
    #        ) %>%
    mutate(swii = Green/(Blue+Red)) %>% 
    filter(swii >= -2 & swii <= 2) %>%
    mutate(month = month(date),
           year = year(date)) %>%
    # filter(year(date) %in% 2010:2020) %>%
    # full_join(., all_years_months) %>%
    ggplot(aes(date, swii, color = position, group = position_)) +
    geom_line() +
    # scale_x_date(date_labels="%y-%b",date_breaks  ="3 month") +
    scale_x_date(date_labels="%Y",date_breaks  ="2 years") +
    viridis::scale_color_viridis() +
    theme_few() +
    theme(axis.text.x = element_text(angle = 90)) +
    ggtitle(this_river) +
    labs(x = 'Year',
         y = 'Salinity Index \n [Green/(Blue+Red)]')
