# data to get the SST https://neo.gsfc.nasa.gov/view.php?datasetId=MYD28M

# data to get bathemtry https://neo.gsfc.nasa.gov/view.php?datasetId=GEBCO_BATHY

# data to get Slope and Aspect https://portal.opentopography.org/raster?opentopoID=OTSDEM.122023.4326.1

# data to get Traffic https://emodnet.ec.europa.eu/geoviewer/

# data to get mld  https://podaac.jpl.nasa.gov/dataset/ECCO_L4_MIXED_LAYER_DEPTH_05DEG_MONTHLY_V4R4

# data for Salinity https://www.metoffice.gov.uk/hadobs/en4/download-en4-2-2.html



#######################################covariates#########################################
library(terra)
bath<- terra::rast("~/Desktop/Diversea/bathymetry.TIFF")
plot(bath)
library(sf)
med=st_as_sfc("POLYGON((-6 30, 36 30, 36 45, -6 45, -6 30))", crs = 'WGS84')
bath = terra::crop(bath,med)

library(sf)
library(giscoR)
library(terra)
library(stars)
library(rmapshaper)
###########################################Bathmetry#########################################################
xxx <- giscoR::gisco_get_countries()

Med <- st_as_sfc('POLYGON((-6 30, 36 30, 36 45, -6 45, -6 30))', crs = 'WGS84')

over <- st_within(xxx, Med)

MedPoly <- xxx[sapply(st_intersects(xxx,Med), function(z) if (length(z)==0) NA_integer_ else z[1]) == 1, ]

MedPoly <- st_crop(MedPoly, Med)

bath <- terra::rast("~/Desktop/Diversea/bathymetry.TIFF")

bath <- crop(bath, Med)

bath1 <- read.csv("~/Desktop/Diversea/bath1.csv",header=FALSE)

#values(bath)<-as.vector(bath1)
bath <- crop(bath, Med)

bath1=bath

values(bath1) <- ifelse(values(bath == 255), 1, 0)


PolyMed <- stars:::st_as_sf.stars(stars::st_as_stars(bath1), point = FALSE, merge = TRUE, connect8 = TRUE)

PolyMedSimp <- rmapshaper::ms_simplify(PolyMed, keep = 0.10)

names(PolyMedSimp)[1] <- 'Ocean'

library(terra)

# Load the raster
bath <- terra::rast("~/Desktop/Diversea/bathymetry.TIFF")

# Inspect raster dimensions
n_cells <- ncell(bath)  # Total number of cells in the raster
print(dim(bath))        # Print rows, columns, and layers
print(n_cells)          # Print total cell count

# Load the CSV
bath1 <- t(read.csv("~/Desktop/Diversea/bath1.csv",header=FALSE))

# Convert to a numeric vector
bath1 <- as.numeric(unlist(bath1))  # Flatten the data

# Debug: Print sizes
print(length(bath1))  # Number of values in bath1
print(n_cells)        # Number of cells in the raster

values(bath) <- bath1
values(bath)[values(bath) == 99999] <- NA

# Inspect the updated raster
print(bath)


###########SST###########

xxx <- giscoR::gisco_get_countries()

Med1 <- st_as_sfc('POLYGON((-6 30, 36 30, 36 45, -6 45, -6 30))', crs = 'WGS84')

over1 <- st_within(xxx, Med1)

MedPoly1 <- xxx[sapply(st_intersects(xxx,Med1), function(z) if (length(z)==0) NA_integer_ else z[1]) == 1, ]

MedPoly1 <- st_crop(MedPoly1, Med1)

SST <- terra::rast("~/Desktop/Diversea/SST.TIFF")

SST1 <- read.csv("~/Desktop/Diversea/SST1.csv",header=FALSE)

SST <- crop(SST, Med1)

SST1=SST

values(SST1) <- ifelse(values(SST == 255), 1, 0)


PolyMed1 <- stars:::st_as_sf.stars(stars::st_as_stars(SST1), point = FALSE, merge = TRUE, connect8 = TRUE)

PolyMedSimp1 <- rmapshaper::ms_simplify(PolyMed1, keep = 0.10)

names(PolyMedSimp1)[1] <- 'Ocean1'

SST <- terra::rast("~/Desktop/Diversea/SST.TIFF")

# Inspect the raster dimensions
n_cells_SST <- ncell(SST)  # Total number of cells in the SST raster
print(dim(SST))            # Print rows, columns, and layers of the raster
print(n_cells_SST)         # Print total cell count

# Load the SST1 CSV
SST1 <- t(read.csv("~/Desktop/Diversea/SST1.csv", header = FALSE))

# Convert the data to a numeric vector
SST1 <- as.numeric(unlist(SST1))  # Flatten the data

# Debug: Print sizes for comparison
print(length(SST1))        # Number of values in SST1
print(n_cells_SST)         # Number of cells in the SST raster

values(SST) <- SST1
values(SST)[values(SST) == 99999] <- NA

# Inspect the updated raster
print(SST)

###############################Slope#################################
slope <- terra::rast("~/Desktop/Diversea/viz/viz.GEBCOIceTopo_slope.TIF")
n_cells_slope <- ncell(slope)  # Total number of cells in the SST raster
print(dim(slope))            # Print rows, columns, and layers of the raster
print(n_cells_slope) 

slope <- aggregate(slope, 10)
# Check the current CRS of the raster
crs(slope)

# Reproject the raster to WGS 84 (EPSG:4326)
slope_reprojected <- terra::project(slope, "EPSG:4326")

slope_reprojected <- crop(slope_reprojected, Ocean)

plot(slope_reprojected)

# Save the reprojected raster if needed
#terra::writeRaster(traffic_reprojected, "~/Desktop/Diversea/vesseldensity_all_2021_WGS84.tif", overwrite = TRUE)

# Verify the new CRS
crs(slope_reprojected)

# Resample traffic_reprojected to match SST's resolution and extent
slope_resampled <- terra::resample(slope_reprojected, SST, method = "bilinear")
###########################Aspect#######################################
aspect<- terra::rast("~/Desktop/Diversea/viz/viz.GEBCOIceTopo_aspect.TIF")

n_cells_aspect <- ncell(aspect)  # Total number of cells in the SST raster
print(dim(aspect))            # Print rows, columns, and layers of the raster
print(n_cells_aspect) 

aspect <- aggregate(aspect, 10)
# Check the current CRS of the raster
crs(aspect)

# Reproject the raster to WGS 84 (EPSG:4326)
aspect_reprojected <- terra::project(aspect, "EPSG:4326")

aspect_reprojected <- crop(aspect_reprojected, Ocean)

plot(aspect_reprojected)

# Save the reprojected raster if needed
#terra::writeRaster(traffic_reprojected, "~/Desktop/Diversea/vesseldensity_all_2021_WGS84.tif", overwrite = TRUE)

# Verify the new CRS
crs(aspect_reprojected)

# Resample traffic_reprojected to match SST's resolution and extent
aspect_resampled <- terra::resample(aspect_reprojected, SST, method = "bilinear")


##########################################traffic####################################################

library(terra)

# Load the raster file (ensure the path is correct)
traffic <- terra::rast("~/Desktop/Diversea/average_vessel_density_2017_2023.tif")
traffic <- aggregate(traffic, 10)
# Check the current CRS of the raster
crs(traffic)

# Reproject the raster to WGS 84 (EPSG:4326)
traffic_reprojected <- terra::project(traffic, "EPSG:4326")

traffic_reprojected <- crop(traffic_reprojected, Ocean)

plot(traffic_reprojected)

# Save the reprojected raster if needed
terra::writeRaster(traffic_reprojected, "~/Desktop/Diversea/vesseldensity_all_2017_2023_WGS84.tif", overwrite = TRUE)

# Verify the new CRS
crs(traffic_reprojected)

# Resample traffic_reprojected to match SST's resolution and extent
traffic_resampled <- terra::resample(traffic_reprojected, SST, method = "bilinear")

# Check the properties of the resampled raster
print(traffic_resampled)
plot(traffic_resampled)

# Optionally save the resampled raster
terra::writeRaster(traffic_resampled, "~/Desktop/Diversea/traffic_resampled_average_WGS84.tif", overwrite = TRUE)

# Load required libraries
library(terra)
library(sf)
library(ggplot2)

# Load the raster file (ensure the path is correct)
traffic <- terra::rast("~/Desktop/Diversea/average_vessel_density_2017_2023.tif")
traffic <- aggregate(traffic, 10)
# Reproject raster to WGS 84 (EPSG:4326)
traffic_reprojected <- terra::project(traffic, "EPSG:4326")

# Convert raster to points for easier extraction
traffic_points <- terra::as.points(traffic_reprojected)
traffic_points_sf <- st_as_sf(traffic_points)

# Ensure traffic points have the correct CRS (WGS 84)
st_crs(traffic_points_sf) <- 4326  # Explicitly set CRS for points

# Extract raster values at point locations
traffic_points_sf$value <- terra::extract(traffic_reprojected, traffic_points_sf)

# Remove points with NA values
traffic_points_sf <- traffic_points_sf[!is.na(traffic_points_sf$value), ]

# Manually define the Mediterranean Sea boundary using approximate coordinates
#mediterranean_coords <- matrix(c(
# Longitude, Latitude (clockwise direction)
#  -5.0, 45.0,   # Northwest corner
#  40.0, 45.0,   # Northeast corner
# 40.0, 30.0,   # Southeast corner
#  -5.0, 30.0,   # Southwest corner
# -5.0, 45.0    # Close the polygon by repeating the first point
#), ncol = 2, byrow = TRUE)

# Create an sf object for the boundary
#mediterranean_boundary <- st_sfc(st_polygon(list(mediterranean_coords)))

# Ensure Mediterranean boundary has the correct CRS (WGS 84)
#st_crs(mediterranean_boundary) <- 4326  # Explicitly set CRS for boundary

# Filter points within the Mediterranean boundary
traffic_points_sf <- traffic_points_sf[st_within(traffic_points_sf, Ocean, sparse = FALSE), ]
traffic_points_sf <- traffic_points_sf[traffic_points_sf$value$mean > 0.2,]
# Check the number of points in the Mediterranean region
print(paste("Number of points in Mediterranean:", nrow(traffic_points_sf)))

vessel_density=traffic_points_sf$mean

# Plot the traffic density within the Mediterranean Sea
ggplot() +
  geom_sf(data = Ocean, fill = "lightblue", color = "darkblue") +
  geom_sf(data = traffic_points_sf, aes(color = log(vessel_density)), size = 0.5) +
  scale_color_viridis_c() +
  theme_minimal() +
  ggtitle("Traffic Density in the Mediterranean Sea")


####################################################MLD###############################################
library(terra)
library(sf)

# 1. Load MLD .tif files
data_dir <- "~/Desktop/Diversea/data_nc"
mld_files <- list.files(path = data_dir, pattern = "^MLD_.*\\.tif$", full.names = TRUE)

# 2. Template raster for resampling
template_raster <- rast(mld_files[1])

# 3. Load and align all rasters
aligned_rasters <- lapply(mld_files, function(f) {
  r <- rast(f)
  r <- resample(r, template_raster, method = "bilinear")
  return(r)
})

# 4. Stack all MLD rasters
mld_stack <- rast(aligned_rasters)

# 5. Save combined stack
writeRaster(mld_stack, "MLD_stack_all_months.tif", overwrite = TRUE)

# 6. Reproject to WGS84 if needed
mld_reprojected <- terra::project(mld_stack, "EPSG:4326")

# 7. Load Mediterranean EEZ shapefile (created previously)
med_eez <- vect("mediterranean_eez.shp")

# 8. Crop and mask to the Mediterranean EEZ
mld_cropped <- crop(mld_reprojected, med_eez)
mld_masked  <- mask(mld_cropped, med_eez)

# 9. Plot to check
plot(mld_masked)

# 10. If SST raster exists and has desired resolution/alignment
# SST <- rast("your_sst_raster.tif")  # Uncomment and load your SST if available
mld_final <- resample(mld_masked, SST, method = "bilinear")

# Optional: Save masked raster stack
writeRaster(mld_masked, "MLD_Mediterranean_stack.tif", overwrite = TRUE)

library(terra)

# Load the masked MLD stack (multiple layers over time)
mld_masked <- rast("MLD_Mediterranean_stack.tif")

# Collapse to a single layer by computing the mean across all layers
mld_mean <- mean(mld_masked, na.rm = TRUE)

# Optional: resample to match SST resolution if SST is loaded
# SST <- rast("your_sst_raster.tif")
mld_mean <- resample(mld_mean, SST, method = "bilinear")

# Save the resulting single-layer raster
writeRaster(mld_mean, "MLD_Mediterranean_mean.tif", overwrite = TRUE)

# Confirm it's a single layer
print(mld_mean)

########################################## Salinity##################################

library(ncdf4)
library(terra)

# Path to NetCDF files (adjust if needed)
years <- 2020:2023
months <- 1:12
base_path <- "~/Desktop/Diversea/data_nc/"

# Load Mediterranean polygon (ensure CRS is EPSG:4326)
if (inherits(Ocean, "sf")) Ocean <- vect(Ocean)
crs(Ocean) <- "EPSG:4326"

# Initialize list to hold rasters
sal_rasters <- list()

for (year in years) {
  for (month in months) {
    file_path <- paste0(base_path, "EN.4.2.2.f.analysis.g10.", year, sprintf("%02d", month), ".nc")
    
    # Skip if file doesn't exist
    if (!file.exists(file_path)) next
    
    nc <- nc_open(file_path)
    sal <- ncvar_get(nc, "salinity")   # [lon, lat, depth]
    lon <- ncvar_get(nc, "lon")
    lat <- ncvar_get(nc, "lat")
    nc_close(nc)
    
    # Adjust longitudes if needed
    if (any(lon > 180)) lon <- ifelse(lon > 180, lon - 360, lon)
    
    # Extract surface salinity
    sal_surface <- sal[,,1]
    sal_surface <- t(sal_surface)
    
    # Create raster
    r <- rast(sal_surface)
    dx <- mean(diff(lon))
    dy <- mean(diff(lat))
    ext(r) <- ext(min(lon) - dx/2, max(lon) + dx/2,
                  min(lat) - dy/2, max(lat) + dy/2)
    crs(r) <- "EPSG:4326"
    r <- flip(r, direction = "vertical")
    
    # Crop to Mediterranean
    r_med <- crop(r, Ocean, mask = TRUE)
    sal_rasters[[length(sal_rasters) + 1]] <- r_med
  }
}

# Stack and compute average
sal_stack <- rast(sal_rasters)
sal_avg <- mean(sal_stack, na.rm = TRUE)

# Save to GeoTIFF
writeRaster(sal_avg, "average_salinity_2020_2023.tif", overwrite = TRUE)

sal_reprojected <- terra::project(sal_avg, "EPSG:4326")

sal_reprojected <- crop(sal_reprojected, Ocean)

plot(sal_reprojected)

# Verify the new CRS
crs(sal_reprojected)

# Resample traffic_reprojected to match SST's resolution and extent
sal_resampled <- terra::resample(sal_reprojected, SST, method = "bilinear")



#####################Mesh########################################################################################


projection="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=km +no_defs +type=crs"
max.edge = 0.95
# - as before
bound.outer = 4.6


Ocean <- PolyMedSimp[PolyMedSimp$Ocean == 0,]
Ocean <- Ocean[4,]

max.edge <- 100
mesh4 = fmesher::fm_mesh_2d(boundary = Ocean,
                            #loc=cbind(result$decimalLatitude, result$decimalLongitude),
                            max.edge = c(1,5)*max.edge,
                            # - use 5 times max.edge in the outer extension/offset/boundary
                            cutoff = 0.06,crs = projection,
                            offset = c(max.edge, bound.outer)*80)

plot(mesh4)

#####################################################################################################
library(PointedSDMs)
projection="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=km +no_defs +type=crs"
traffic0=crop(traffic_resampled,MedPoly1)
bath0=crop(bath,MedPoly1)
SST0=crop(SST,MedPoly1)
slope0=crop(slope_resampled,MedPoly1)
aspect0=crop(aspect_resampled,MedPoly1)
mld0=crop(mld_mean,MedPoly1)
sal0=crop(sal_resampled,MedPoly1)
traffic00=scale(log(traffic0+0.00000001))
TPI0 <- terrain(bath0, v = "TPI", neighbors = 8)


covariates=c(scale(SST0),scale(bath0), traffic00 ,scale(slope0),scale(aspect0),scale(TPI0),scale(mld0),scale(sal0))
names(covariates)=c("SST","Bathymetry","Vessel_traffic","Slope","Aspect","TPI","MLD","Salinity")


library(sf)
Ocean_sf <- sf::st_as_sf(Ocean)
Ocean <- st_transform(Ocean, crs = crs(SST0))
Ocean <- terra::project(Ocean, crs(SST0))
# Match CRS to SST0
# Convert sf to terra vector
Ocean_vect <- vect(Ocean)

# Crop and mask example (do this for each covariate if needed)
SST0 <- crop(SST0, Ocean)
SST0 <- mask(SST0, Ocean)
summary(values(SST0))
plot(SST0)

bath0 <- crop(bath0, Ocean)
bath0 <- mask(bath0, Ocean)

traffic00 <- crop(traffic00, Ocean)
traffic00 <- mask(traffic00, Ocean)

slope0 <- crop(slope0, Ocean)
slope0 <- mask(slope0, Ocean)

aspect0 <- crop(aspect0, Ocean)
aspect0 <- mask(aspect0, Ocean)

TPI0 <- crop(TPI0, Ocean)
TPI0 <- mask(TPI0, Ocean)

mld0 <- crop(mld0, Ocean)
mld0 <- mask(mld0, Ocean)

sal0 <- crop(sal0, Ocean)
sal0 <- mask(sal0, Ocean)

# Choose a template layer – e.g., SST0
template <- SST0

# Align all layers to the template
bath0     <- resample(bath0,     template, method = "bilinear")
traffic00 <- resample(traffic00, template, method = "bilinear")
slope0    <- resample(slope0,    template, method = "bilinear")
aspect0   <- resample(aspect0,   template, method = "bilinear")
TPI0      <- resample(TPI0,      template, method = "bilinear")
mld0      <- resample(mld0,      template, method = "bilinear")
sal0      <- resample(sal0,      template, method = "bilinear")

covariates <- rast(list(
  scale(SST0), 
  scale(bath0), 
  traffic00, 
  scale(slope0),
  scale(aspect0), 
  scale(TPI0), 
  scale(mld0),
  scale(sal0)
))

names(covariates) <- c(
  "SST", "Bathymetry", "Vessel_traffic", "Slope",
  "Aspect", "TPI", "MLD","Salinity"
)
covariates <- mask(covariates, Ocean)

#####################################################################################################################

datasets=list(sf_projected1,sf_projected3,sf_projected5,
              sf_projected8,sf_projected9,sf_projected10
              ,sf_projected16,sf_projected17
)


base$specifySpatial(Bias = TRUE,
                    prior.range = c(200, 0.01),
                    prior.sigma = c(1.5, 0.1))

formula_str <- ~ SST + Bathymetry + Vessel_traffic +Slope + TPI + MLD + Salinity + MLD:Vessel_traffic +
  SST:TPI+Salinity:Vessel_traffic+ I(Vessel_traffic^2)+ Bathymetry:Slope + Bathymetry:Salinity

base <- startISDM(
  datasets = datasets,
  spatialCovariates = covariates,
  Boundary = Ocean_sf,                # sf polygon of the Mediterranean boundary
  Projection = projection,         # your spatial projection, typically WGS 84
  responsePA = "sperm_whale",      # binary 0/1 presence-absence
  responseCounts = "Counts",       # count column if applicable
  Mesh = mesh4,                    # your pre-built spatial mesh
  pointsSpatial = "shared",        # assuming shared spatial structure
  Formulas = list(covariateFormula = formula_str)
)

model0 <- fitISDM(
  base,
  options = list(
    verbose = TRUE,
    control.inla = list(control.vb = list(enable = FALSE))  # Use Laplace not VB
  )
)

predictions = predict(model, mesh = mesh4, mask = st_transform(Ocean,projection), predictor = TRUE)

ggplot() +
  geom_sf(data = Ocean, fill = "lightblue", color = "darkblue") +
  geom_sf(data = predictions$predictions, aes(color = mean), size = 0.5) +
  scale_color_viridis_c() +
  theme_minimal() +
  ggtitle("Sperm Whale Predictions in the Mediterranean Sea")


############################################################################################################################


