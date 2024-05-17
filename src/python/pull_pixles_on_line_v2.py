### This script pulls individual pixel values from Landsat images 
### for a given line. In this case we are using NHD flowlines of the intra-
### costal waterway in NC

### load libraries
import time
import ee
import geemap
import os
import numpy
import pandas as pd
import ee.mapclient


# Connect to GEE
ee.Initialize(project = 'clearing-blackwaters')

# Load in helpers from Gardner et al alnysis 
exec(open("src/python/rhea_GEE_pull_functions_rivers.py").read())

# Load in Pekel water occurance Layer and Landsat Collections.
# choose Pekel = False to have dynamic DWSE water mask
PekelMask = False
pekel = ee.Image('JRC/GSW1_0/GlobalSurfaceWater')

# load in landsat datasets
l8 = ee.ImageCollection('LANDSAT/LC08/C01/T1_SR')
l7 = ee.ImageCollection('LANDSAT/LE07/C01/T1_SR')
l5 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')

# Identify collection for use in sourced functions.
collection = 'SR'

# Standardize band names between the various collections and aggregate 
# them into one image collection

bn8 = ['B2','B3', 'B4', 'B5', 'B6','B7', 'pixel_qa']
bn57 = ['B1', 'B2', 'B3', 'B4', 'B5','B7', 'pixel_qa']
bns = ['Blue', 'Green', 'Red', 'Nir', 'Swir1', 'Swir2', 'qa']
  
ls5 = l5.select(bn57, bns)
ls7 = l7.select(bn57, bns)
ls8 = l8.select(bn8, bns)

# set cloud threshold. Should turn down to 50 since I filter to that later
ls = ee.ImageCollection(ls5.merge(ls7).merge(ls8))\
.filter(ee.Filter.lt('CLOUD_COVER', 50))

# Load in flowline of interest 
all_flowlines = ee.data.listAssets({'parent': 'users/spencerrhea/coastal_blackwaters'})

# Get flowline ids
flowlinesID = []
for x in range(0, len(all_flowlines['assets'])):
    flowline_id = all_flowlines['assets'][x]['id']
    flowlinesID.append(flowline_id)


flowlines = ee.FeatureCollection("users/spencerrhea/icw_flowlines")

# make a folder in your google drive manually to output data
dlDir = 'data/WQP_RiverExport_v2' # Dis this manually  
filesDown = os.listdir(dlDir)  # -->
filesDown = [int(i.replace(".csv", "")) for i in filesDown]

for i in range(0, len(flowlinesID)):

    # Select individual flowline
    flowline = ee.FeatureCollection(flowlinesID[i])

    # Get all comids 
    flowline_comids = flowline.aggregate_array('comid').getInfo()
    
    # Loop through comids 
    for x in range(0,len(flowline_comids)):
        
        # get individual comid 
        this_line = flowline.filter(ee.Filter.eq('comid', flowline_comids[x]))
        
        # Filter landsat image to comid 
        ls_masked = ls.filterBounds(this_line).map(masking)
        
        # Apply DSWE algorithm 
        ls_masked_dswe = ls_masked.map(Dswe)
        # ls8_masked_dswe_fin = ls8_masked.merge(ls8_masked_dswe)
        
        # Pull data
        # For image
        data = ls_masked.getRegion(this_line, 30)
        keys = ee.List(data.get(0)).getInfo()
        data_info = data.getInfo() 
        save_path = "data/coastal_blackwater_ls/" + str(flowline_comids[x]) + ".feather"
        
        pd.DataFrame(data_info[1:len(data_info)],
                     columns = keys).to_feather(save_path)
        
        # For DSWE 
        data_dswe = ls_masked_dswe.getRegion(this_line, 30)
        keys_dswe = ee.List(data_dswe.get(0)).getInfo()
        
        data_dswe_info = data_dswe.getInfo() 
        
        save_path_dswe = "data/coastal_blackwater_ls_dswe/" + str(flowline_comids[x]) + ".feather"
        
        pd.DataFrame(data_dswe_info[1:len(data_dswe_info)],
                     columns = keys_dswe).to_feather(save_path_dswe)




# OLD CODE
# Trying to pull a whole image (I think it works)

# # Define the visualization parameters.
# image_viz_params = {
#     'bands': ['Nir', 'Red', 'Green'],
#     'min': 0,
#     'max': 0.5,
#     'gamma': [0.95, 1.1, 1],
# }

# ee.mapclient.addToMap(ls8_masked.first().clip(aoi),image_viz_params, "mymap")


# fin_table = ([keys])
# for x in range(1,data.length().getInfo()):
#     this_row = data_info[x]

#     fin_table = (fin_table, this_row)


# df2 = pandas.DataFrame(columns = keys, data = this_row)

# fin_table.to_feather("file.feather") 


# data.get(2).getInfo() 

# dataOut = ee.batch.Export.image.toDrive(image = ls8_masked.first().clip(aoi).select(['qa']), 
#                                         description = 'TEST_MASK_2_qa',
#                                         folder = 'WQP_RiverExport_v2',
#                                         crs = 'EPSG:4326',
#                                         scale = 30,
#                                         maxPixels = 1000000000000,
#                                         region = ls8_masked.geometry(),
#                                         # selectors = ['id', 'longitude', 'latitude', 'time', 'Blue', 'Green', 'Red', 'Nir', 'Swir1', 'Swir2', 'qa'],
#                                         fileFormat = 'GeoTIFF')
# dataOut.start()


## keys = ee.List(data.get(0))

##featureCollection = ee.FeatureCollection(data.slice(1).map(
##    def list_to_table(singleData):
##        singleData = ee.List(singleData)
##        dict = ee.Dictionary.fromLists(keys, singleData)
##        point = ee.Geometry.Point([dict.get('longitude'), dict.get('latitude')])
##        timeFormat = ee.Date(dict.get('time')).format('YYYY-MM-dd')
##    return ee.Feature(point, dict).set('timeFormat', timeFormat)))



##def list_to_table(Data):
##    singleData = ee.List(Data)
##    dict = ee.Dictionary.fromLists(keys, singleData)
##    point = ee.Geometry.Point([dict.get('longitude'), dict.get('latitude')])
##    timeFormat = ee.Date(dict.get('time')).format('YYYY-MM-dd')
##    return ee.Feature(point, dict).set('timeFormat', timeFormat)