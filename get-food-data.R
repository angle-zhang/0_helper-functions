


## HELPER FUNCTIONS ------------------------------------

# General function to fetch data from ArcGIS REST services
# ID of feature layer
get_arcgis_data <- function(url) {
  require(arcgislayers)
  tryCatch({
    server <- arc_open(url)
    
    # Get all layers and tables
    layers_info <- get_all_layers(server)
    
    # Extract layers and tables
    layers <- layers_info$layers
    # Display information about first layer
    # TODO multiple layers
    feature_layer <- get_layer(server, id=layers[[1]]$id)
    
    arc_select(feature_layer)
    
  }, error = function(e) print(e))
}

# General function to download files
download_file <- function(url, destfile, unzip_dir = NULL) {
  destfile <- file.path(base_path, destfile)
  download.file(url, destfile, method = "libcurl", mode = "wb")
  if (!is.null(unzip_dir)) unzip(destfile, exdir = file.path(base_path, unzip_dir))
}

# General function to read CSV files
read_csv_file <- function(filename) {
  read_csv(file.path(base_path, filename), show_col_types = FALSE)
}

## SNAP DATA ----------------------------------------- 

# Get historical SNAP data (filtered by years)
get_snap_historical <- function(years = 1930:2021, proj_crs) { 
  read_csv_file("hist_snap_retailer_final2022/hist_snap_retailer_final2022.csv") %>%
    filter(auth_year %in% years) %>%
    st_as_sf(coords = c("x", "y"), crs = proj_crs)
}

# Download historical SNAP data
download_snap_historical <- function() { 
  download_file(
    url = "https://github.com/jshannon75/snap_retailers/raw/refs/heads/master/data/hist_snap_retailer_final2022_csv.zip",
    destfile = "hist_snap_retailer_final2022_csv.zip",
    unzip_dir = "hist_snap_retailer_final2022"
  )
}

# Get current SNAP data filtered by polygon
get_snap_current <- function(polygon, proj_crs) { 
  snap_url <- "https://services1.arcgis.com/RLQu0rK7h4kbsBq5/arcgis/rest/services/snap_retailer_location_data/FeatureServer/0/"
  
  get_layer_by_poly(snap_url, polygon, sp_rel = "contains") %>%
    st_transform(st_crs(proj_crs))
}

## FOOD INSPECTION DATA ------------------------------------- 

# Download food inspection data for LA County
download_foodinsp_lacounty <- function() { 
  download_file(
    url = "https://www.arcgis.com/sharing/rest/content/items/19b6607ac82c4512b10811870975dbdc/data",
    destfile = "foodinsp_lacounty21_24.csv"
  )
}

# Load food inspection data
get_foodinsp_lacounty <- function() { 
  read_csv_file("foodinsp_lacounty21_24.csv") 
}

## RETAIL FOOD MARKETS --------------------------------------

# Function to fetch and combine retail food market data with source identifiers
download_foodins_lacounty_ssi <- function() {
  # Define URLs with corresponding source identifiers
  urls <- c(
    "Dec_2024.markets" = "https://services1.arcgis.com/ZIL9uO234SBBPGL7/arcgis/rest/services/Retail_Food_Markets_Dec_2024/FeatureServer",
    "June_2024.markets" = "https://services1.arcgis.com/ZIL9uO234SBBPGL7/arcgis/rest/services/Retail_Food_Markets_June_2024/FeatureServer",
    "March_2024.markets" = "https://services1.arcgis.com/ZIL9uO234SBBPGL7/arcgis/rest/services/Retail_Food_Markets_March_2024/FeatureServer",
    "Dec_2023.markets" = "https://services1.arcgis.com/ZIL9uO234SBBPGL7/arcgis/rest/services/Retail_Food_Markets_December_2023/FeatureServer", 
    "March_2025.res" = "https://services1.arcgis.com/ZIL9uO234SBBPGL7/arcgis/rest/services/Restaurants_042025_Final/FeatureServer",
    "Dec_2024.res" = "https://services1.arcgis.com/ZIL9uO234SBBPGL7/arcgis/rest/services/Restaurants_December_2024/FeatureServer",
    "June_2024.res" = "https://services1.arcgis.com/ZIL9uO234SBBPGL7/arcgis/rest/services/Restaurants_June_2024/FeatureServer",
    "March_2024.res" = "https://services1.arcgis.com/ZIL9uO234SBBPGL7/arcgis/rest/services/Restaurants_March_2024/FeatureServer",
    "Dec_2023.res" = "https://services1.arcgis.com/ZIL9uO234SBBPGL7/arcgis/rest/services/Restaurants_December_2023/FeatureServer"
  )     

  # Initialize an empty list to store data frames
  data_list <- list()
  
  # Iterate over URLs and their names
  for (source_id in names(urls)) {
    url <- urls[[source_id]]
    data <- get_arcgis_data(url)
    if (!is.null(data)) {
      print(url)
      print(source_id)
      txt <- base::strsplit(source_id, split = "\\.")[[1]]  # Add source identifier column
      data$source <- txt[[1]]  # Add source identifier column
      data$type <- txt[[2]]  # Add type identifier column
      data_list[[source_id]] <- data
      
      # standardize names
      for (col in 1:ncol(data_list[[source_id]])){
        colnames(data_list[[source_id]])[col] <-  sub("USER_", "", colnames(data_list[[source_id]])[col])
        colnames(data_list[[source_id]])[col] <-  sub("__", "_", colnames(data_list[[source_id]])[col])
        colnames(data_list[[source_id]])[col] <- toupper(colnames(data_list[[source_id]])[col])
      }
    }
  }
  
  combined_data <- rbindlist(data_list, use.names = TRUE, fill=TRUE) 
  
  st_write(combined_data, paste0(base_path, "foodinsp23_24_SSI.gpkg"), driver="GPKG", append=F)

}

get_foodins_lacounty_ssi <- function(proj_crs) { 

  res <- st_read(paste0(base_path, "foodinsp23_24_SSI.gpkg")) %>%
    st_transform(proj_crs)
}


# Function to download retail food market data
download_retail_food_LB_PAS <- function() {
  urls <- c("markets" = "https://services1.arcgis.com/ZIL9uO234SBBPGL7/arcgis/rest/services/Retail_Food_Markets_LB_PAS_V_2023/FeatureServer", 
            "res" = "https://services1.arcgis.com/ZIL9uO234SBBPGL7/arcgis/rest/services/Restaurants_LB_PAS_V_2023/FeatureServer")  
  
  # get data and append to it using arcgis 
  data <- data.frame()
  
  for (type in names(urls)) { 
    data <- rbind(data, get_arcgis_data(urls[[type]]) %>% mutate(type=type))  
  }
  
  # check if data was retrieved
  if (is.null(data)) {
    message("Error: No data retrieved from ArcGIS service.")
    return(NULL)
  }
  
  # Standardize column names
  colnames(data) <- gsub("USER_", "", colnames(data))
  colnames(data) <- gsub("__", "_", colnames(data))
  # change everything except last column to uppercase
  last_col <- ncol(data)
  colnames(data)[1:(last_col-1)] <- toupper(colnames(data)[1:(last_col-1)]) 

  # Save as GeoPackage
  output_path <- file.path(base_path, "retail_food_LB_PAS_V_2023.gpkg")
  
  st_write(data, output_path, driver = "GPKG", append = FALSE)
  
  message("Retail food data saved to: ", output_path)
}


# Function to load saved retail food market data
get_retail_food_LB_PAS <- function(proj_crs) {
  file_name <- "retail_food_LB_PAS_V_2023.gpkg"
  file_path <- file.path(base_path, file_name)
  
  if (!file.exists(file_path)) {
    stop("File not found! Run download_retail_food_LB_PAS() first.")
  }
  
  st_read(file_path) %>%
    st_transform(proj_crs)  # Transform to desired CRS
}

