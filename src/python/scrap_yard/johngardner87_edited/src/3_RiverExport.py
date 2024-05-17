# -*- coding: utf-8 -*-
"""
Created on Mon May 13 14:16:42 2019

@author: john

"""

### This script combined with GEE_pull_functions_rivers extracts 
### Landsat surface reflectance over a channel water mask and
### caculates reach medians over all bands.
### Inputs: centerlines (NHDplusV2), reach polygons (made in previous script),
###         and a DEM (MERIT)

### load libraries
import time
import ee
import os
import numpy
import pandas
#import feather

ee.Initialize(project = 'clearing-blackwaters')

# Note: This script uses python 3 which has syntax differences from python 2.7

# Source necessary functions.
#execfile('D:/Dropbox/projects/TSS/code/GEE_pull_functions_rivers_ST_0513.py')
#execfile('GEE_pull_functions_rivers_ST_0513.py')
exec(open("src/johngardner87/src/GEE_pull_functions_rivers.py").read())    

# Load in Pekel water occurance Layer and Landsat Collections.
# choose Pekel = False to have dynamic DWSE water mask
PekelMask = False
pekel = ee.Image('JRC/GSW1_0/GlobalSurfaceWater')

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
# ls = ee.ImageCollection(ls5.merge(ls7).merge(ls8))\
# .filter(ee.Filter.lt('CLOUD_COVER', 60))

ls = ls8.filter(ee.Filter.lt('CLOUD_COVER', 60))

# Select the occurence layer in the pekel mask, which is just the 
# percentage of water occurence over a given pixel from 1985-2015.
# Set the percent occurance threshold and create a watermask from the result.
threshold = 50
water = pekel.select('occurrence').gt(threshold)
water = water.updateMask(water)

# load reach polygons with same ID and footprint as NHD reach, but wider.
# rivers = ee.FeatureCollection("users/johngardner87/CONUS_nhd_reach_poly_collapse");
# Change to AR/ICW ROI
rivers = ee.FeatureCollection("users/spencerrhea/AR_ICW");

# load nhd centerlines
# centerline = ee.FeatureCollection("users/johngardner87/nhd_grwl_collapse");

# Filter rivers that have already been downloaded
# riversort = rivers.sort('ID')

# riverID = riversort.aggregate_array('ID').getInfo() 

# make a folder in your google drive manually to output data
dlDir = 'data/WQP_RiverExport_v2' # Dis this manually  
filesDown = os.listdir(dlDir)  # -->
filesDown = [int(i.replace(".csv", "")) for i in filesDown]
                    
# for x in range(0,len(riverID)):
# river = ee.Feature(rivers.filter(ee.Filter.eq('ID', riverID[x])).first())
# reach = ee.Feature(centerline.filter(ee.Filter.eq('ID', riverID[x])).first())
shell = ee.Feature(None)

#FilterBounds for river, update masks for water occurence, clouds, roads, etc.
#Remove any images with clouds directly over the waterbody
lsover = ls.filterBounds(rivers.geometry())\
.map(clipImage).first()

# Load dem to attach elevation to each reach. IF using GRWL lines, do NOT do this.
# GRWL has good elevation and this is slow to run. 
# And this only needs to be run once if you want elevation. 
DEMout = ee.Image("users/eeProject/MERIT").reduceRegion(ee.Reducer.min(), rivers.geometry(), 30)
DEMdata = ee.Feature(None, {'elevation': DEMout.get('elevation'),'ID':rivers.get('ID')})
# Map over sites within specific path/row and pull reflectance values, change cSCore for rivers threshold to 1000
# data = lsover.map(lakePull).filter(ee.Filter.lt('cScore', 1000))
# data = lakePull(lsover)



def waterOnly(image):
  f = AddFmask(image).select('fmask')
  cScore = f.eq(2).Or(f.eq(3)).Or(f.eq(4))
  cScore = cScore.reduceRegion(ee.Reducer.sum(),rivers.geometry(),30).get('fmask')
  d = Dswe(image).select('dswe')
  h = CalcHillShades(image, rivers.geometry()).select('hillShade')
  hs = CalcHillShadows(image, rivers.geometry()).select('hillShadow')
  return image.addBands(f).addBands(d).addBands(h).addBands(hs).updateMask(f.lte(2)).updateMask(d.eq(1).Or(d.eq(2))).set({'cScore':cScore})

waterOut = waterOnly(lsover)
#This selects only high confidence DSWE==1. Can relax to 1 and 2 for open water. 3 for vegetated water.
#Also, the max cumulative cost distance is set to 750 m. Can increase to get more pixels.
waterMask = waterOut.select('dswe')
# waterChannel = ExtractChannel(waterMask, reach, lake.geometry(), 750)
# waterOut = waterOut.updateMask(waterChannel)

#Collect median reflectance and occurence values
# lsout = waterOut.reduceRegion(ee.Reducer.median(), rivers.geometry(), 30)

#Collect reflectance and occurence st.dev.
# lsdev = waterOut.reduceRegion(ee.Reducer.stdDev(), rivers.geometry(), 30)

  #Pull Sat info
if collection == 'SR':
  mission = ee.String(lsover.get('SATELLITE')).split('_').get(1)
else:
  mission = ee.String(lsover.get('SPACECRAFT_ID')).split('_').get(1)

# Export one image 
task = ee.batch.Export.image(lsover.select(['Blue', 'Green', 'Red', 'Nir', 'Swir1', 'Swir2'])
, 'exportExample')

task.start()

# Export one water mask
task_2 = ee.batch.Export.image(waterMask
, 'exportExample_mask')

task_2.start()
