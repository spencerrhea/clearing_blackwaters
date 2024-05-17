library(nhdplusTools)
library(sf)
#> Linking to GEOS 3.11.2, GDAL 3.7.2, PROJ 9.3.0; sf_use_s2() is TRUE

#### Scuppernong ####
start_point <- st_sfc(st_point(c(-76.315000, 35.940806)), crs = 4326)
start_comid <- discover_nhdplus_id(start_point)

flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 1000)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

scuppernong <- subset$NHDFlowline_Network %>%
    filter(comid %in% get_UM(subset$NHDFlowline_Network, start_comid))

st_write(scuppernong,
         'clearing_blackwaters/data/flowlines_of_interest/scuppernong',
         driver = 'ESRI Shapefile',
         delete_dsn = T)

#### ICW ####
# Had to do this diffrenlty becuase I could not pull it with nhdplustools 
icw_flowlines <- st_read('clearing_blackwaters/data/icw_flowlines') %>%
    st_zm()

icw <- matrix(nrow = nrow(icw_flowlines),
              ncol = length(names(scuppernong))) %>%
    as.tibble()

colnames(icw) <- names(scuppernong)

icw$id <- icw_flowlines$permanent_
icw$fdate <- icw_flowlines$fdate
icw$resolution <- icw_flowlines$resolution
icw$gnis_id <- icw_flowlines$gnis_id
icw$gnis_name <- icw_flowlines$gnis_name
icw$lengthkm <- icw_flowlines$lengthkm
icw$reachcode <- icw_flowlines$reachcode
icw$flowdir <- icw_flowlines$flowdir
icw$ftype <- icw_flowlines$ftype
icw$fcode <- icw_flowlines$fcode
icw$geometry <- icw_flowlines$geometry
icw <- st_as_sf(icw)

icw <- icw %>%
    mutate(comid = ifelse(nchar(id) == 9, id, substr(id, 2, 10)))

icw$comid[8] <- '111111111'
icw$comid[9] <- '111111112'
icw$comid[10] <- '111111113'

mapview::mapview(icw)

st_write(icw,
         'clearing_blackwaters/data/flowlines_of_interest/icw',
         driver = 'ESRI Shapefile',
         delete_dsn = T)

#### White Oak ####
start_point <- st_sfc(st_point(c(-77.110861, 34.686556)), crs = 4326)
start_comid <- discover_nhdplus_id(start_point)

flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 1000)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

white_oak <- subset$NHDFlowline_Network %>%
    filter(comid %in% get_UM(subset$NHDFlowline_Network, start_comid))

st_write(white_oak,
         'clearing_blackwaters/data/flowlines_of_interest/white_oak',
         driver = 'ESRI Shapefile',
         delete_dsn = T)

#### New River ####
start_point <- st_sfc(st_point(c(-77.362972, 34.562306)), crs = 4326)
start_comid <- discover_nhdplus_id(start_point)

flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 1000)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

new_river <- subset$NHDFlowline_Network %>%
    filter(comid %in% get_UM(subset$NHDFlowline_Network, start_comid))

st_write(new_river,
         'clearing_blackwaters/data/flowlines_of_interest/new_river',
         driver = 'ESRI Shapefile',
         delete_dsn = T)

# Queen Creek
start_point <- st_sfc(st_point(c(-77.151222, 34.669333)), crs = 4326)
start_comid <- discover_nhdplus_id(start_point)

flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 1000)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

queens_creek <- subset$NHDFlowline_Network %>%
    filter(comid %in% get_UM(subset$NHDFlowline_Network, start_comid))

st_write(queens_creek,
         'clearing_blackwaters/data/flowlines_of_interest/queens_creek',
         driver = 'ESRI Shapefile',
         delete_dsn = T)

# Adam Creek (bad)
start_point <- st_sfc(st_point(c(-76.685028, 34.779222)), crs = 4326)
start_comid <- discover_nhdplus_id(start_point)

flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 1000)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

adams_creek <- subset$NHDFlowline_Network %>%
    filter(comid %in% get_UM(subset$NHDFlowline_Network, start_comid))

# st_write(queens_creek,
#          'clearing_blackwaters/data/flowlines_of_interest/queens_creek',
#          driver = 'ESRI Shapefile')

# Trenr river
start_point <- st_sfc(st_point(c(-77.040333, 35.100889)), crs = 4326)
start_comid <- discover_nhdplus_id(start_point)

flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 1000)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

trent_river <- subset$NHDFlowline_Network %>%
    filter(comid %in% get_UM(subset$NHDFlowline_Network, start_comid))

st_write(trent_river,
         'clearing_blackwaters/data/flowlines_of_interest/trent_river',
         driver = 'ESRI Shapefile',
         delete_dsn = T)

# Pamlico/Tar river
start_point <- st_sfc(st_point(c(-76.879361, 35.442056)), crs = 4326)
start_comid <- discover_nhdplus_id(start_point)

flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 80)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

pamlico_river <- subset$NHDFlowline_Network %>%
    filter(comid %in% get_UM(subset$NHDFlowline_Network, start_comid))

# mapview::mapview(pamlico_river)

st_write(pamlico_river,
         'clearing_blackwaters/data/flowlines_of_interest/pamlico_river',
         driver = 'ESRI Shapefile',
         delete_dsn = T)

# Neuse
start_point <- st_sfc(st_point(c(-76.991694, 35.059444)), crs = 4326)
start_comid <- discover_nhdplus_id(start_point)

flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 80)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

neuse_river <- subset$NHDFlowline_Network %>%
    filter(comid %in% get_UM(subset$NHDFlowline_Network, start_comid))

# mapview::mapview(neuse_river)

st_write(neuse_river,
         'clearing_blackwaters/data/flowlines_of_interest/neuse_river',
         driver = 'ESRI Shapefile',
         delete_dsn = T)

# Pungo
start_point <- st_sfc(st_point(c(-76.553500, 35.402333)), crs = 4326)
start_comid <- discover_nhdplus_id(start_point)

flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 1000)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

pungo_river <- subset$NHDFlowline_Network %>%
    filter(comid %in% get_UM(subset$NHDFlowline_Network, start_comid))

st_write(pungo_river,
         'clearing_blackwaters/data/flowlines_of_interest/pungo_river',
         driver = 'ESRI Shapefile',
         delete_dsn = T)

# Aligator 
start_point <- st_sfc(st_point(c(-76.020750, 35.866028)), crs = 4326)
start_comid <- discover_nhdplus_id(start_point)

flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 1000)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

alligaor_river <- subset$NHDFlowline_Network %>%
    filter(comid %in% get_UM(subset$NHDFlowline_Network, start_comid))

st_write(alligaor_river,
         'clearing_blackwaters/data/flowlines_of_interest/alligaor_river',
         driver = 'ESRI Shapefile',
         delete_dsn = T)

# Milltail Creek 
start_point <- st_sfc(st_point(c(-75.978333, 35.841528)), crs = 4326)
start_comid <- discover_nhdplus_id(start_point)

flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 1000)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

milltail_creek <- subset$NHDFlowline_Network %>%
    filter(comid %in% get_UM(subset$NHDFlowline_Network, start_comid))

st_write(milltail_creek,
         'clearing_blackwaters/data/flowlines_of_interest/milltail_creek',
         driver = 'ESRI Shapefile',
         delete_dsn = T)

# Cashie River
start_point <- st_sfc(st_point(c(-76.802944, 35.902056)), crs = 4326)
start_comid <- discover_nhdplus_id(start_point)

flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 1000)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

cashie_river <- subset$NHDFlowline_Network %>%
    filter(comid %in% get_UM(subset$NHDFlowline_Network, start_comid))

start_point <- st_sfc(st_point(c(-76.734500, 35.936167)), crs = 4326)
start_comid <- discover_nhdplus_id(start_point)

flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 1000)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(c(10452246, 10452226, 10455798, 10452206,
                                               10452220)),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

cashie_river_fin <- rbind(cashie_river, subset$NHDFlowline_Network)

st_write(cashie_river_fin,
         'clearing_blackwaters/data/flowlines_of_interest/cashie_river',
         driver = 'ESRI Shapefile',
         delete_dsn = T)

# Chowan
start_point <- st_sfc(st_point(c(-76.690389, 36.037861)), crs = 4326)
start_comid <- discover_nhdplus_id(start_point)

flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 80)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

chowan_river <- subset$NHDFlowline_Network %>%
    filter(comid %in% get_UM(subset$NHDFlowline_Network, start_comid))

st_write(chowan_river,
         'clearing_blackwaters/data/flowlines_of_interest/chowan_river',
         driver = 'ESRI Shapefile',
         delete_dsn = T)

# Perquimans
start_point <- st_sfc(st_point(c(-76.315472, 36.115361)), crs = 4326)
start_comid <- discover_nhdplus_id(start_point)

flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 80)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

perquimans_river <- subset$NHDFlowline_Network %>%
    filter(comid %in% get_UM(subset$NHDFlowline_Network, start_comid))

st_write(perquimans_river,
         'clearing_blackwaters/data/flowlines_of_interest/perquimans_river',
         driver = 'ESRI Shapefile',
         delete_dsn = T)

# Pasquotank
start_point <- st_sfc(st_point(c(-76.043083, 36.180694)), crs = 4326)
start_comid <- discover_nhdplus_id(start_point)

flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 80)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

pasquotank_river <- subset$NHDFlowline_Network %>%
    filter(comid %in% get_UM(subset$NHDFlowline_Network, start_comid))

st_write(pasquotank_river,
         'clearing_blackwaters/data/flowlines_of_interest/pasquotank_river',
         driver = 'ESRI Shapefile',
         delete_dsn = T)

# North River 
start_point <- st_sfc(st_point(c(-75.893778, 36.171167)), crs = 4326)
start_comid <- discover_nhdplus_id(start_point)

flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 80)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

north_river <- subset$NHDFlowline_Network %>%
    filter(comid %in% get_UM(subset$NHDFlowline_Network, start_comid))

st_write(north_river,
         'clearing_blackwaters/data/flowlines_of_interest/north_river',
         driver = 'ESRI Shapefile',
         delete_dsn = T)

read_and_append <- function(file_path){
    river_name <- str_split_fixed(file_path, '/', n = Inf)[,4]
    
    file <- st_read(file_path) %>%
        mutate(river = !!river_name)
    
    return(file)
}

all_fils <- list.files('clearing_blackwaters/data/flowlines_of_interest/',
                       full.names = TRUE)

dsdsds <- tibble()
for(i in 1:length(all_fils)) {
    thsisis <- st_read(all_fils[i])
    
    thsisis <- thsisis %>%
        mutate(river = str_split_fixed(all_fils[i], '/', n = Inf)[,5])
    
    dsdsds <- rbind(dsdsds, thsisis)
}

rivers <- map_dfr(all_fils, read_and_append)

icw <- st_read('clearing_blackwaters/data/icw_flowlines') %>%
    mutate(river = 'icw')

rivers <- rbind(dsdsds, icw)

dsdsds %>%
    select(names(icw))

mapview::mapview(dsdsds, zcol = 'river')



#### LOOKSY ####
ls_llok <- arrow::read_feather('clearing_blackwaters/data/coastal_blackwater_ls/10484569.feather')
dswe_llok <- arrow::read_feather('clearing_blackwaters/data/coastal_blackwater_ls_dswe/10484569.feather') %>%
    select(-time)

ls_1 <- left_join(ls_llok, dswe_llok) %>%
    group_by(longitude, latitude) %>%
    summarise(dswe = mean(dswe, na.rm = T),
              min_dswe = min(dswe, na.rm = T)) %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) 

mapview::mapview(ls_1, zcol = 'dswe')

ls_llok_ <- arrow::read_feather('clearing_blackwaters/data/coastal_blackwater_ls/26815205.feather')
dswe_llok_ <- arrow::read_feather('clearing_blackwaters/data/coastal_blackwater_ls_dswe/26815205.feather') %>%
    select(-time)

ls_2 <- left_join(ls_llok_, dswe_llok_) %>%
    group_by(longitude, latitude) %>%
    summarise(dswe = mean(dswe, na.rm = T),
              min_dswe = min(dswe, na.rm = T)) %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) 

mapview::mapview(ls_2, zcol = 'dswe')


landsat_pix_dates <- ls_llok %>%
    mutate(date = substr(id, 17, nchar(id))) %>%
    mutate(date = as_date(date, '%y%m%d')) %>%
    mutate(mission = substr(id, 5, 8)) %>%
    mutate(ndvi = (Nir - Red) / (Nir + Red)) 

ggplot(landsat_pix_dates, aes(date, Green)) +
    geom_point()
