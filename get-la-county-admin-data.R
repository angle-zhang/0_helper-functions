# FILE FOR PULLING VARIOUS LA COUNTY AND CITY ADMINISTRATIVE BOUNDARIES
library(tigris)
library(arcgislayers)
library(sf)
library(httr)
library(jsonlite)

base_path <- "C:/Users/angie/OneDrive/Desktop/data-analysis/0_shared-data/raw/"

# la county boundary 
# last update: 08/03/2023
# source: https://egis-lacounty.hub.arcgis.com/datasets/0f58ddb711b84569ae0e7084c0404045_13/explore
get_county_boundary <- function() { 
  county_boundary_url <- "https://dpw.gis.lacounty.gov/dpw/rest/services/PW_Open_Data/MapServer/13"
  layer <- arc_open(county_boundary_url) 
  county_boundary <- arc_select(layer) 
  # check if valid
  valid <- st_is_valid(county_boundary)
  print(valid)
  
  if (!valid) {
  # fix
    county_boundary <- st_make_valid(county_boundary)
  }
  
  return (county_boundary)
}

# -------------------------------------------------------------------------------------------------

# La county wide statistical areas
# desc: Countywide statistical areas in Los Angeles County
# last update: 9/2016
# source: https://egis-lacounty.hub.arcgis.com/datasets/7b8a64cab4a44c0f86f12c909c5d7f1a_23/explore

# -------------------------------------------------------------------------------------------------

# la county service planning areas
# desc: Service Planning Areas in Los Angeles County
# last update: 03/25/2022
# source: https://egis-lacounty.hub.arcgis.com/datasets/service-planning-areas-2022-view/explore

# -------------------------------------------------------------------------------------------------

# LA county cities: 
# desc: Boundaries for the 88 cities and the unincorporated areas within Los Angeles County
# last update: 9/2016
# source: https://egis-lacounty.hub.arcgis.com/datasets/8ea349021cf544adb9bb079d3631df77_0/explore

# -------------------------------------------------------------------------------------------------

# LA city council districts 2021
# desc: Los Angeles City Council Districts, adopted in 2021
# last update: 12/20/2021
# source: https://geohub.lacity.org/datasets/76104f230e384f38871eb3c4782f903d

# -------------------------------------------------------------------------------------------------

# LA country census tracts
# desc: Los Angeles County Census tracts 2020 boundaries with bodies of water removed
# source: 2020 Census TIGER/Line Shapefiles
# source: https://egis-lacounty.hub.arcgis.com/datasets/la-county-census-tracts-2020/explore

download_census_tracts <- function(state="CA", county, year=2020) { 

  # remove bodies of water from census tracts
all_tracts <- tigris::tracts(state=state, county=county, year = year, class="sf") %>%
  erase_water(year=2020)

  st_write(all_tracts, paste0(base_path,  state,"_", county, "_", year,  "_census_tracts.gpkg")) 
  
}

# clip to some boundary
get_census_tracts <- function(proj_crs, state, year, county, boundary=NULL) { 
  ct <- st_read(paste0(base_path, state,"_", county, "_", year,  "_census_tracts.gpkg")) %>%
    st_transform(proj_crs)
  
  
  if (!is.null(boundary)) {
    ct$indicator <- st_within(ct, boundary) %>%
      lengths > 0 
  
    ct <- ct %>% 
      filter(indicator == TRUE) %>%
      select(-indicator)
    
    print(head(ct))
    return(ct)
  }
  
  return(ct)
}

# LA country census blocks
# desc: Los Angeles County Census blocks 2020 boundaries with bodies of water removed
# source: 2020 Census TIGER/Line Shapefiles

download_census_blocks <- function(state="CA", year=2020, county) { 
  # remove bodies of water from census tracts
  all_blocks <- tigris::blocks(state=state, county=county, year = year, class="sf") %>%
    erase_water(year=2020)
  st_write(all_blocks, paste0(base_path, state,"_", county, "_", year,  "_census_blocks.gpkg"), append=F) 
  
}

get_census_blocks <- function(proj_crs, state, year, county,boundary=NULL) { 
  cb <- st_read(paste0(base_path,  state,"_", county, "_", year, "_census_blocks.gpkg")) %>%
    st_transform(proj_crs)
  
  if (!is.null(boundary)) {
    cb$indicator <- st_within(cb, boundary) %>%
      lengths > 0 
    
    cb <- cb %>% 
      filter(indicator == TRUE) %>%
      select(-indicator)
  }
  
  cb
}

# from: https://gis.stackexchange.com/questions/151613/reading-feature-class-in-file-geodatabase-using-r
get_lac_households <- function (proj_crs) { 
  
  unzip(paste0(base_path, "ParcelData_031325_LACountyParcelsAsHH_export.gdb.zip"), exdir = base_path)
  # The input file geodatabase
  
  # List all feature classes in a file geodatabase
  dsn = paste0(base_path, "ParcelData_031325_LACountyParcelsAsHH_export.gdb")
  fgdb <- st_layers(dsn) 

  print(fgdb)
  
  # Read the feature class
  fc <- st_read(dsn,layer="LACounty_Parcels_SpatialJoin_20_10_102424") %>%
    st_transform(proj_crs)
  
}

download_osm <- function(name="Southern California", location="socal", bbox) {
  # get road network data
  require(osmextract)
  
  # get best provider match for open street map data
  oe_get(name, boundary=bbox, download_directory=paste0(base_path,  "osm_", location), download_only=T)
  
}

get_osm <- function(location="socal", bbox) {
  # get road network data
  require(osmextract)
  pbf_file <- paste0(base_path, "osm_", location, "/geofabrik_", location, "-latest.osm.pbf")  # Path to your downloaded PBF file
  gpkg_file <- paste0(base_path,  "osm_", location, "/geofabrik_",  location, "-latest.osm.gpkg")  # Path to your downloaded GPKG file
  # if no gpkg file exists, read pbf
  if (!file.exists(gpkg_file)) {
    osm_lines <- oe_read(pbf_file, provider="geofabrik", boundary=bbox)  # Change layer to "points", "polygons" as needed
 
  } else {
    osm_lines <- oe_read(gpkg_file, provider="geofabrik", boundary=bbox)  # Change layer to "points", "polygons" as needed
  }
  
  return(osm_lines)

}

download_dem <- function(boundary, location="socal") { 
  # download dem data
  require(elevatr)
  require(terra)
  dem <- get_elev_raster(boundary, z=10)
  terra::writeRaster(dem, paste0(base_path,  "osm_", location, "/", location, "_dem.tif"))
}

get_dem <- function(location="socal", proj_crs) { 
  # get dem data
  require(terra)
  dem <- terra::rast(paste0(base_path, "osm_", location, "/", location, "_dem.tif"))
  crs <- paste0("EPSG:",proj_crs)
  dem <- project(dem, crs)  # Convert to WGS 84 if necessary
  
  return(dem)
}

