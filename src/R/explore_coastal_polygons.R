library(tidyverse)
library(sf)

gardner_nhd_GRWL <- st_read('space_ghosts//data/gardner_nhd_GRWL')

mapview::mapview(gardner_nhd_GRWL)

# NHD Area
nhd_area <- st_read('clearing_blackwaters/data/Shape/NHDArea.shp')

nhd_area_minus_ocean <- nhd_area %>%
    filter(! ObjectID %in% c('3326', '3337', '3044'))
mapview::mapview(nhd_area_minus_ocean)

# NHD Flowlines
nhd_flowline_0 <- st_read('clearing_blackwaters/data/NHD/NHDFlowline_0.shp')
nhd_flowline_1 <- st_read('clearing_blackwaters/data/NHD/NHDFlowline_1.shp')

nhd_flowline <- rbind(nhd_flowline_0, nhd_flowline_1)

costal_bbox <- st_bbox(nhd_flowline)
costal_bbox[4] <- 35.318152
costal_bbox[3] <- -77.013614
costal_bbox[1] <- -75.55359
costal_bbox[2] <- 36.26

mapview::mapview(costal_bbox)

nhd_flowlines_APP <- st_crop(nhd_flowline, costal_bbox)
mapview::mapview(nhd_flowlines_APP)

look_pungo <- filter(nhd_flowlines_APP) %>%
    filter(gnis_id == '01022107')

look_icw <- filter(nhd_flowlines_APP) %>%
    filter(gnis_id == '00992912')

look_icw <- filter(nhd_flowlines_APP) %>%
    filter(gnis_id == '00992912')

look_icw_2 <- nhd_flowlines_APP %>%
    filter(permanent_ %in% c('141603111', '141602936', '141602921', '141602886', '141602661', 
                          '141602627', '141602617', '141602412', '141602202', '141602191', 
                          '141602163', '141601846', '141601814', '141601548', '141601418', 
                          '141601357', '141601097', '141600901', '167451376', '141600782', 
                          '167451368', '167451381', '136046395', '136046396', '136046397'))

mapview::mapview(look_icw_2) + look_icw

icw_lines <- rbind(look_icw, look_icw_2)
st_write(icw_lines, 'clearing_blackwaters/data/icw_flowlines',
         driver = 'ESRI Shapefile')

# NHD waterbodies
# Water bodies are odd. They include alligator river but not the Scuppernong
# May have to do some manual cvonstruction of the water bodies of interest 
# from flowlines (select and buffer), water bodies, and maybe area 
nhd_waterbodies <- st_read('clearing_blackwaters/data/NHD/NHDWaterbody.shp')
nhd_waterbodies_APP <- st_crop(nhd_waterbodies, costal_bbox)
mapview::mapview(nhd_waterbodies_APP)

# GRWL
# Grids
grwl <- st_read('clearing_blackwaters/data/GRWL')
mapview::mapview(grwl)
# We want # IMW_ID 	739
# TILE_ID 	NJ18
# AND 
# IMW_ID 	799
# TILE_ID 	NI18

grwl_nj18 <- st_read('clearing_blackwaters/data/GRWL_vector_V01.01/NJ18.shp')
grwl_ni18 <- st_read('clearing_blackwaters/data/GRWL_vector_V01.01/NI18.shp')
mapview::mapview(grwl_nj18) + grwl_ni18

# NHD HR
nhd_hr <- st_read('clearing_blackwaters/data/NHDPLUS_H_0302_HU4_GDB.gdb',
                  layer = 'NHDWaterbody')

costal_bbox <- st_bbox(nhd_hr)
costal_bbox[4] <- 35.318152
costal_bbox[3] <- -77.013614
costal_bbox[1] <- -75.55359
costal_bbox[2] <- 36.26

mapview::mapview(nhd_hr)

# Plot RBG 
image <- rast('clearing_blackwaters/data/exportExample.tif')
image$Blue[values(image$Blue) < 0] <-0

terra::plotRGB(image, 
               r = 3,
               g = 2,
               b = 1,
               scale = 8000) + terra::plot(mask)

mask <- rast('clearing_blackwaters/data/exportExample_mask.tif')
mask[values(mask) < 0.5] <- NA
terra::plot(mask)
