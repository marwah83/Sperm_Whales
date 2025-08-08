library(dplyr)
library(sf)
library(purrr)
library(robis)
library(ggplot2)

# Define geometry and dataset IDs
geometry <- "POLYGON((-6 30, 36 30, 36 45, -6 45, -6 30))"
dataset_ids <- c(
  "1ea34638-560e-4eac-9d14-25a095771f55",
  "8d8b347d-4b04-41ea-a122-048fbecab4d7",
  "e92b6433-c003-47b2-8f46-6a1e87db48c9",
  "a4bb1978-5aa4-4fd2-b8f3-ffc19b6467dc",
  "9a81d405-545b-4a1b-bdaa-aff5350a0c39",
  "d5847ecb-6f9b-4599-888a-461cb26f8018",
  "19ae0b90-d0ef-426d-9918-89160ca2abaa",
  "924c4d25-6358-44a3-8f4d-24086256ad3e",
  "e23bf817-593d-40d8-a7a3-6823d741d50b",
  "6c7069ec-4541-4b66-8211-528b60a7dd3d",
  "a33f249d-83ed-4e6e-84e3-d3144153b3de",
  "6ff78b05-3413-4626-a0d1-d36e75e2b2d3",
  "2101d4c5-c20b-49c0-a44b-3d6484c4c891"
)

# Define LAEA projection
laea_crs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=km +no_defs +type=crs"

# Custom function to process a dataset
process_dataset <- function(datasetid) {
  occ <- occurrence(geometry = geometry, datasetid = datasetid)
  
  # Attempt to create 'date' from either dateIdentified or date_year
  if (!is.null(occ$dateIdentified)) {
    occ$date <- as.Date(occ$dateIdentified)
  } else if (!is.null(occ$date_year)) {
    occ$date <- occ$date_year
  } else {
    occ$date <- NA
  }

  result <- occ %>%
    group_by(decimalLongitude, decimalLatitude, date) %>%
    summarise(species = paste(species, collapse = ", "), .groups = 'drop') %>%
    mutate(
      sperm_whale = ifelse(grepl("Physeter macrocephalus", species), 1, 0)
    )
  
  sf_object <- st_as_sf(result, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
  st_transform(sf_object, crs = laea_crs)
}

# Apply processing function to each datasetid
sf_projected_list <- map(dataset_ids, process_dataset)

# Optionally name the list for easier access later
names(sf_projected_list) <- paste0("sf_projected", seq_along(dataset_ids))
