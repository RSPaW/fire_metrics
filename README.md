# fire_metrics
Current R code that replicates patch, frequency, time since fire, distance and age class statistics previously generated using ArcGIS model builder. 

## Intended usage:

* Use R Studio.
* Create a new R Project and check out this repo to a local directory on your computer. All scripts will recognise relative file paths if this is adhered to.
* Run the `00` scripts to pre-process data.
* Run the `01-08` scripts to generate metrics.

**NOTE** - Processing time varies between scripts but raster processing of this size takes time. All scripts output messages to the console so the user can see progress.

**NOTE** - By the nature of the way the spatial data is created (cumulative stacking and calculations) the data here cannot be added to once new annual fire vectors become available. It is best to archive the raster data (if required) and start over.

## Main inputs required: 
1. Annual fire vector shape files with an attributed year mapped from satellite.
2. Reporting region as a vector shape file.

## Scripts:

* `00_preprocess_firescar_vectors_to_raster_base_data.R` - Will do all the pre-processing of the input vectors to raster, handling resolutions, extents etc. It will also produce a number of fundamental annual spatial datasets (tif) that will be used to generate metrics utilising additional scripts. The annual spatial datasets (anf - annual fires, anfm - annual fire masks, yob - annual year of burn, tsf - annual time since fire) are saved to the users workspace. 
**NOTE** Very large mapped areas can be processed. The pre-processing script will assess the extents of the mapped area and will process in chunks if necessary. If chunks are created the script can also mosaic annual spatial datasets chunks. The mosaicked annual spatial chunks are utilised in the "_metric_" scripts and are also good for visualisation.

* `00a_preprocess_regions_to_masks.R` - Reporting region vectors are rasterised.

* `01-08_metric_....R` - Various scripts that produce stated metrics. They can all be run independently (i.e. there is no inter-dependency so can be run in any order). All metric scripts output a csv file of the stats. to a created directory called `/statistics`

## Directories created:

* `/mosaics/01_annual_fires` - will contain anf spatial data as .tif
* `/mosaics/02_annual_fire_masks` - will contain anfm spatial data as .tif
* `/mosaics/03_year_of_burn` - will contain yob spatial data as .tif
* `/mosaics/04_time_since_fire` - will contain tsf spatial data as .tif
* `reporting_masks` - will contain reporting region masks as .tif
* `statistics` - will contain statistics generated from metric scripts as .csv
* `vectors` - will contain individual reporting regions as vector data (1 per region) as .shp

**NOTE** - Directories other than the above and appended with consecutive numbers are spatial data chunks prior to mosaicking. Once mosaicked datasets have been writen, the chunk directories can be deleted to save space.
