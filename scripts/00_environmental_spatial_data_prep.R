# Uncertainty paper


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#             00. Environmental and spatial data preparation             #
# ---------------------------------------------------------------------- #

# Set working directory
setwd("/import/ecoc9z/data-zurell/holle/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/")

# Required path 
#path_imp <- file.path("/import/ecoc9z/data-zurell/holle/uncertainty_paper")

# Load needed packages
library(terra)
library(stringr)

#-------------------------------------------------------------------------------

# 1. Create a background mask --------------------------------------------------

# Background mask for pseudo-absence generation and spatial thinning with a target
# resolution and extent from climate layers

# Read in climate data and stack all the variables in the same file
Chelsa <- terra::rast(str_sort(list.files("input_data/environmental_data/Chelsa_V2", 
                    pattern = ".tif", full.names = TRUE), numeric = TRUE))

world_mask <- chelsa_variables[[1]]

# Set all raster cells that are not NA to 1
values(world_mask)[!is.na(values(world_mask))] <- 1

# Write a raster
terra::writeRaster(world_mask, "input_data/spatial_data/world_mask.tif", overwrite = TRUE)

#-------------------------------------------------------------------------------

# 2. Process edaphic data ------------------------------------------------------

# (a) --------------------------
# Read in the soil data and stack all the variables in the same file
SoilGrids <- terra::rast(str_sort(list.files("input_data/environmental_data/SoilGrids_V2_raw", 
                                             pattern = ".tif", full.names = TRUE), numeric = TRUE))

# calculate the mean of the different variable raster in their different soil depths
# (Exception: the variable organic carbon stocks is already measured from 0-30cm depth)
bulk_density <- mean(c(SoilGrids[[1]], SoilGrids[[2]], SoilGrids[[3]]), na.rm = TRUE)
names(bulk_density) <- "bulk_density"

cation_exchange_capacity <- mean(c(SoilGrids[[4]], SoilGrids[[5]], SoilGrids[[6]]), na.rm = TRUE)
names(cation_exchange_capacity) <- "cation_exchange_capacity"

coarse_fragments_content <- mean(c(SoilGrids[[7]], SoilGrids[[8]], SoilGrids[[9]]), na.rm = TRUE)
names(coarse_fragments_content) <- "coarse_fragments_content"

clay_content <- mean(c(SoilGrids[[10]], SoilGrids[[11]], SoilGrids[[12]]), na.rm = TRUE)
names(clay_content) <- "clay_content"

nitrogen <- mean(c(SoilGrids[[13]], SoilGrids[[14]], SoilGrids[[15]]), na.rm = TRUE)
names(nitrogen) <- "nitrogen"

organic_carbon_density <- mean(c(SoilGrids[[16]], SoilGrids[[17]], SoilGrids[[18]]), na.rm = TRUE)
names(organic_carbon_density) <- "organic_carbon_density"

organic_carbon_stocks <- SoilGrids[[19]]
names(organic_carbon_stocks) <- "organic_carbon_stocks"

pH <- mean(c(SoilGrids[[20]], SoilGrids[[21]], SoilGrids[[22]]), na.rm = TRUE)
names(pH) <- "pH"

sand_content <- mean(c(SoilGrids[[23]], SoilGrids[[24]], SoilGrids[[25]]), na.rm = TRUE)
names(sand_content) <- "sand_content"

silt_content <- mean(c(SoilGrids[[26]], SoilGrids[[27]], SoilGrids[[28]]), na.rm = TRUE)
names(silt_content) <- "silt_content"

organic_carbon_content <- mean(c(SoilGrids[[29]], SoilGrids[[30]], SoilGrids[[31]]), na.rm = TRUE)
names(organic_carbon_content) <- "organic_carbon_content"

water_retention_10 <- mean(c(SoilGrids[[32]], SoilGrids[[33]], SoilGrids[[34]]), na.rm = TRUE)
names(water_retention_10) <- "water_retention_10"

water_retention_33 <- mean(c(SoilGrids[[35]], SoilGrids[[36]], SoilGrids[[37]]), na.rm = TRUE)
names(water_retention_33) <- "water_retention_33"

water_retention_1500 <- mean(c(SoilGrids[[38]], SoilGrids[[39]], SoilGrids[[40]]), na.rm = TRUE)
names(water_retention_1500) <- "water_retention_1500"

# (b) --------------------------
# Change projection to the same coordinate reference system as the climate 
# variables (WGS84 datum) and to match their extents
# Read in the chelsa variables as template raster
Chelsa <- terra::rast(str_sort(list.files("input_data/environmental_data/Chelsa_V2", 
                                          pattern = ".tif", full.names = TRUE), numeric = TRUE))


bulk_density <- terra::project(bulk_density, Chelsa)

cation_exchange_capacity <- terra::project(cation_exchange_capacity, Chelsa)

coarse_fragments_content <- terra::project(coarse_fragments_content, Chelsa)

clay_content <- terra::project(clay_content, Chelsa)

nitrogen <- terra::project(nitrogen, Chelsa)

organic_carbon_density <- terra::project(organic_carbon_density, Chelsa)

organic_carbon_stocks <- terra::project(organic_carbon_stocks, Chelsa)

pH <- terra::project(pH, Chelsa)

sand_content <- terra::project(sand_content, Chelsa)

silt_content <- terra::project(silt_content, Chelsa)

organic_carbon_content <- terra::project(organic_carbon_content, Chelsa)

water_retention_10 <- terra::project(water_retention_10, Chelsa)

water_retention_33 <- terra::project(water_retention_33, Chelsa)

water_retention_1500 <- terra::project(water_retention_1500, Chelsa)

# (c) --------------------------
# Write a raster for the newly averaged and reprojected SoilGrid variables
terra::writeRaster(bulk_density, "input_data/environmental_data/SoilGrids_V2/bulk_density.tif", 
                   overwrite = TRUE)

terra::writeRaster(cation_exchange_capacity, "input_data/environmental_data/SoilGrids_V2/cation_exchange_capacity.tif", 
                   overwrite = TRUE)

terra::writeRaster(coarse_fragments_content, "input_data/environmental_data/SoilGrids_V2/coarse_fragments_content.tif", 
                   overwrite = TRUE)

terra::writeRaster(clay_content, "input_data/environmental_data/SoilGrids_V2/clay_content.tif", 
                   overwrite = TRUE)

terra::writeRaster(nitrogen, "input_data/environmental_data/SoilGrids_V2/nitrogen.tif", 
                   overwrite = TRUE)

terra::writeRaster(organic_carbon_density, "input_data/environmental_data/SoilGrids_V2/organic_carbon_density.tif", 
                   overwrite = TRUE)

terra::writeRaster(organic_carbon_stocks, "input_data/environmental_data/SoilGrids_V2/organic_carbon_stocks.tif", 
                   overwrite = TRUE)

terra::writeRaster(pH, "input_data/environmental_data/SoilGrids_V2/pH.tif", 
                   overwrite = TRUE)

terra::writeRaster(sand_content, "input_data/environmental_data/SoilGrids_V2/sand_content.tif", 
                   overwrite = TRUE)

terra::writeRaster(silt_content, "input_data/environmental_data/SoilGrids_V2/silt_content.tif", 
                   overwrite = TRUE)

terra::writeRaster(organic_carbon_content, "input_data/environmental_data/SoilGrids_V2/organic_carbon_content.tif", 
                   overwrite = TRUE)

terra::writeRaster(water_retention_10, "input_data/environmental_data/SoilGrids_V2/water_retention_10.tif", 
                   overwrite = TRUE)

terra::writeRaster(water_retention_33, "input_data/environmental_data/SoilGrids_V2/water_retention_33.tif", 
                   overwrite = TRUE)

terra::writeRaster(water_retention_1500, "input_data/environmental_data/SoilGrids_V2/water_retention_1500.tif", 
                   overwrite = TRUE)

#-------------------------------------------------------------------------------

# 3. Prepare the Pacific island coordinates for predictions --------------------

# (a) --------------------------
# Load in the island shapefiles from the authors Wohlwend, MR, Craven, D, Weigelt, P, et al. 
# Anthropogenic and environmental drivers shape diversity of naturalized plants across the Pacific. 
# Divers Distrib. 2021; 27: 1120–1133
load("input_data/spatial_data/geoentities_plus_newname.RData")

# Extract the island group names
island_group_names <- unique(geoentities@data[["new_archipelago_name"]])
island_group_names <- na.omit(island_group_names)

# Transform to terra SpatVect object
geoentities_SpatVect <- vect(geoentities, crs = "EPSG:4326")

# Get the spatial extent of each Pacific island group
pacific_islands_extent <- data.frame(expand.grid(island_group=c(paste(island_group_names))), xmin=NA, 
                                     xmax=NA, ymin=NA, ymax=NA)

for (i in island_group_names) { # Start the loop over the island groups
  
  # Create a subset of each island group
  island_group <- subset(geoentities_SpatVect, 
                         geoentities_SpatVect$new_archipelago_name == i)
  
  # Get the spatial extent
  island_group_extent <- terra::ext(island_group)
  island_group_extent_vect <- as.vector(island_group_extent)
  xmin <- island_group_extent_vect[1]
  xmax <- island_group_extent_vect[2]
  ymin <- island_group_extent_vect[3]
  ymax <- island_group_extent_vect[4]
  
  # round the coordinate values to one digit
  if (xmin < 0) { xmin_new <- floor(xmin*100) / 100
  } else if (xmin > 0) { xmin_new <- floor(xmin*100) / 100
  } 
  
  if (xmax < 0) { xmax_new <- ceiling(xmax*100) / 100
  } else if (xmax > 0) { xmax_new <- ceiling(xmax*100) / 100
  } 
  
  if (ymin < 0) { ymin_new <- floor(ymin*100) / 100
  } else if (ymin > 0) { ymin_new <- floor(ymin*100) / 100
  } 
  
  if (ymax < 0) { ymax_new <- ceiling(ymax*100) / 100
  } else if (ymax > 0) { ymax_new <- ceiling(ymax*100) / 100
  }
                         
  # create a results vector
  pacific_islands_extent[pacific_islands_extent$island_group == i, "xmin"] <- xmin_new
  pacific_islands_extent[pacific_islands_extent$island_group == i, "xmax"] <- xmax_new
  pacific_islands_extent[pacific_islands_extent$island_group == i, "ymin"] <- ymin_new
  pacific_islands_extent[pacific_islands_extent$island_group == i, "ymax"] <- ymax_new
  
}

# (b) --------------------------
# Revise shapefiles of island groups that pass the meridian or have unreliable coordinates

# Northern Palau (unreliable coordinates)
northern_palau_old <- subset(geoentities_SpatVect, 
                      geoentities_SpatVect$new_archipelago_name == "Northern Palau")

np_new_extent <- c(133.84, 134.72, 6.91, 8.10)

northern_palau <- crop(northern_palau_old, np_new_extent)

pacific_islands_extent[pacific_islands_extent$island_group == "Northern Palau", "xmin"] <- 133.84
pacific_islands_extent[pacific_islands_extent$island_group == "Northern Palau", "xmax"] <- 134.72
pacific_islands_extent[pacific_islands_extent$island_group == "Northern Palau", "ymin"] <- 6.91
pacific_islands_extent[pacific_islands_extent$island_group == "Northern Palau", "ymax"] <- 8.10

writeVector(northern_palau, "input_data/spatial_data/northern_palau.shp", overwrite = TRUE)


# Solomon Bismark (unreliable coordinates, crossing the meridian as it contains an island
# from the Galapagos Islands, this one is being excluded)
solomon_bismark_old <- subset(geoentities_SpatVect, 
                              geoentities_SpatVect$new_archipelago_name == "Solomon_Bismark")

sb_new_extent <- c(136.96, 169.85, -15.79, -0.53)

solomon_bismark <- crop(solomon_bismark_old, sb_new_extent)

pacific_islands_extent[pacific_islands_extent$island_group == "Solomon_Bismark", "xmin"] <- 136.96
pacific_islands_extent[pacific_islands_extent$island_group == "Solomon_Bismark", "xmax"] <- 169.85
pacific_islands_extent[pacific_islands_extent$island_group == "Solomon_Bismark", "ymin"] <- -15.79
pacific_islands_extent[pacific_islands_extent$island_group == "Solomon_Bismark", "ymax"] <- -0.53

writeVector(solomon_bismark, "input_data/spatial_data/solomon_bismark.shp", overwrite = TRUE)

# Fiji (the island group is crossing the meridian)
fiji_old <- subset(geoentities_SpatVect, 
                   geoentities_SpatVect$new_archipelago_name == "Fiji")

fiji_new_extent_1 <- c(176.50, 180.00, -19.22, -16.12)
fiji_new_extent_2 <- c(-180, -177.51, -19.22, -16.12)

fiji_1 <- crop(fiji_old, fiji_new_extent_1)
fiji_2 <- crop(fiji_old, fiji_new_extent_2)

writeVector(fiji_1, "input_data/spatial_data/fiji_1.shp", overwrite = TRUE)
writeVector(fiji_2, "input_data/spatial_data/fiji_2.shp", overwrite = TRUE)

vector_fiji_1 <- c("Fiji_1", 176.50, 180.00, -19.22, -16.12)
vector_fiji_2 <- c("Fiji_2", -180.00, -177.51, -19.22, -16.12)

pacific_islands_extent <- rbind(pacific_islands_extent, vector_fiji_1)
pacific_islands_extent <- rbind(pacific_islands_extent, vector_fiji_2)

pacific_islands_extent <- pacific_islands_extent[-3,]
rownames(pacific_islands_extent) <- 1:nrow(pacific_islands_extent)

# Make sure island extent numbers are numeric
pacific_islands_extent[,2:5] <- apply(pacific_islands_extent[, 2:5], 2, as.numeric)

# Save the data frame
save(pacific_islands_extent, file = "input_data/spatial_data/pacific_islands_extent.RData")

#-------------------------------------------------------------------------------

# 4. Island group environmental data coverage ----------------------------------

# (a) --------------------------
# Needed objects
load("input_data/spatial_data/pacific_islands_extent.RData") # Island group extent information
load("input_data/spatial_data/geoentities_plus_newname.RData") # Island group shapefiles
fiji_1 <- terra::vect("input_data/spatial_data/fiji_1.shp") # First part of Fiji shapefile
fiji_2 <- terra::vect("input_data/spatial_data/fiji_2.shp") # Second part of Fiji shapefile
Chelsa <- terra::rast(str_sort(list.files("input_data/environmental_data/Chelsa_V2", 
                                          pattern = ".tif", full.names = TRUE), numeric = TRUE)) # Climate variable rasters
SoilGrids <- terra::rast(str_sort(list.files("input_data/environmental_data/SoilGrids_V2", 
                                             pattern = ".tif", full.names = TRUE), numeric = TRUE)) # Edaphic variable rasters

# Get all Pacific island group names for looping
island_group <- unique(pacific_islands_extent$island_group)

# Get all Pacific island group names for the results table (Fiji instead of Fiji_1 and Fiji_2)
island_group_results <- pacific_islands_extent[-c(50:51),1]
island_group_results <- c(island_group_results, "Fiji")

# Get environmental variable names
names(Chelsa) <- c(paste('bio',1:19, sep='_')) # Change climate variable names

environmental_var_rasterstack <- c(Chelsa, SoilGrids) # Stack the climate and edaphic rasters

environmental_var <- names(environmental_var_rasterstack) # Extract the variable names

# Transform to terra SpatVect object
geoentities_SpatVect <- vect(geoentities, crs = "EPSG:4326")

# Create a data frame to store the results
data_coverage_islandgroups <- data.frame(matrix(nrow = length(island_group_results), ncol = length(environmental_var)+2))
colnames(data_coverage_islandgroups) <- c("island_group", "island_group_size", environmental_var)
data_coverage_islandgroups$island_group <- island_group_results


for (e in environmental_var) { # Start the loop over all environmental rasters
  
  print(e)
  
  for (i in island_group) { # Start the loop over all island groups
    
    print(i)
    
    if (i == "Fiji_1") { # Get the extent information of each island group
                         island_subset <- subset(pacific_islands_extent, pacific_islands_extent$island_group == i)
                         island_extent <- c(island_subset$xmin, island_subset$xmax, island_subset$ymin, island_subset$ymax)
                         
                         # Calculate island group size based on shapefile
                         island_group_area_Fiji_1 <- round(sum(expanse(fiji_1, unit = "km")), 2)
                         
                         # Plot first part of island group shapefile
                         svg(paste0("input_data/spatial_data/island_group_shapes/",i,"/",i,"_shape.svg"))
                         plot(fiji_1)
                         dev.off()
      
                         # Get the variable raster
                         raster_variable <- environmental_var_rasterstack[[e]] 
                         
                         # Crop the variable to the island group extent
                         env_var_crop <- crop(raster_variable, island_extent)
                         
                         # Rasterize the island shape based on the cropped variable raster
                         island_group_raster <- terra::rasterize(fiji_1, env_var_crop)
                         
                         # Create a mask of the island group based on the variable
                         island_group_mask <- mask(env_var_crop, island_group_raster)
                         svg(paste0("input_data/spatial_data/island_group_shapes/",i,"/",i,"_",e,"_shape.svg"))
                         plot(island_group_mask)
                         dev.off()
                         
                         # Calculate the area that is covered by data
                         covered_area_Fiji_1 <- expanse(island_group_mask, unit = "km")
                         covered_area_Fiji_1 <- round(covered_area_Fiji_1$area, 2)
                         
                         # Calculate whole island group area
                         whole_area_Fiji_1 <- expanse(island_group_raster, unit = "km")
                         whole_area_Fiji_1 <- round(whole_area_Fiji_1$area, 2)
      
      
    } else if (i == "Fiji_2") { # Get the extent information of each island group
                                island_subset <- subset(pacific_islands_extent, pacific_islands_extent$island_group == i)
                                island_extent <- c(island_subset$xmin, island_subset$xmax, island_subset$ymin, island_subset$ymax)
                                
                                # Calculate island group size based on shapefile
                                island_group_area <- round(sum(expanse(fiji_2, unit = "km")), 2)
                                island_group_area <- island_group_area_Fiji_1 + island_group_area
                                
                                # Plot second part of island group shapefile
                                svg(paste0("input_data/spatial_data/island_group_shapes/",i,"/",i,"_shape.svg"))
                                plot(fiji_2)
                                dev.off()
                                
                                # Get the variable raster
                                raster_variable <- environmental_var_rasterstack[[e]] 
                                
                                # Crop the variable to the island group extent
                                env_var_crop <- crop(raster_variable, island_extent)
                                
                                # Rasterize the island shape based on the cropped variable raster
                                island_group_raster <- terra::rasterize(fiji_2, env_var_crop)
                                
                                # Create a mask of the island group based on the variable
                                island_group_mask <- mask(env_var_crop, island_group_raster)
                                svg(paste0("input_data/spatial_data/island_group_shapes/",i,"/",i,"_",e,"_shape.svg"))
                                plot(island_group_mask)
                                dev.off()
                                
                                # Calculate the area that is covered by data
                                covered_area <- expanse(island_group_mask, unit = "km")
                                covered_area <- round(covered_area$area, 2)
                                
                                # Calculate whole island group area
                                whole_area <- expanse(island_group_raster, unit = "km")
                                whole_area <- round(whole_area$area, 2)
                                
                                # Add the area values of both island parts and enter the result in the data frame
                                covered_area <- covered_area_Fiji_1 + covered_area
                                whole_area <- whole_area_Fiji_1 + whole_area
                                data_coverage_perc <- round(((covered_area/whole_area) * 100), 2)
                                data_coverage_islandgroups[data_coverage_islandgroups$island_group == "Fiji", e] <- data_coverage_perc
                                data_coverage_islandgroups[data_coverage_islandgroups$island_group == "Fiji", "island_group_size"] <- island_group_area
      
      
      
    } else { # Get the extent information of each island group
             island_subset <- subset(pacific_islands_extent, pacific_islands_extent$island_group == i)
             island_extent <- c(island_subset$xmin, island_subset$xmax, island_subset$ymin, island_subset$ymax)
                
             # Get the shapefile of the island group
             island_shape <- subset(geoentities_SpatVect, 
                                    geoentities_SpatVect$new_archipelago_name == i)
             
             svg(paste0("input_data/spatial_data/island_group_shapes/",i,"/",i,"_shape.svg"))
             plot(island_shape)
             dev.off() 
             
             # Calculate island group size based on shapefile
             island_group_area <- round(sum(expanse(island_shape, unit = "km")), 2)
             
             # Get the variable raster
             raster_variable <- environmental_var_rasterstack[[e]] 
                
             # Crop the variable to the island group extent
             env_var_crop <- crop(raster_variable, island_extent)
                
             # Rasterize the island shape based on the cropped variable raster
             island_group_raster <- terra::rasterize(island_shape, env_var_crop)
                
             # Create a mask of the island group based on the variable
             island_group_mask <- mask(env_var_crop, island_group_raster)
             svg(paste0("input_data/spatial_data/island_group_shapes/",i,"/",i,"_",e,"_shape.svg"))
             plot(island_group_mask)
             dev.off()
                
             # Calculate the area that is covered by data
             covered_area <- expanse(island_group_mask, unit = "km")
             covered_area <- round(covered_area$area, 2)
                
             # Calculate whole island group area
             whole_area <- expanse(island_group_raster, unit = "km")
             whole_area <- round(whole_area$area, 2)
             
             # Calculate the percentage of data coverage
             data_coverage_perc <- round(((covered_area/whole_area) * 100), 2)
             
             # Enter the result in the data frame
             data_coverage_islandgroups[data_coverage_islandgroups$island_group == i, e] <- data_coverage_perc
             data_coverage_islandgroups[data_coverage_islandgroups$island_group == i, "island_group_size"] <- island_group_area
      
    }

  } # End of loop over all island groups

} # End the loop over all environmental variable names

# save the resulting data frame with all coverage values
save(data_coverage_islandgroups, file = "input_data/spatial_data/data_coverage_islandgroups.RData")


# (b) --------------------------   
# Subset the data frame to just contain island groups that have a data coverage
# >= 50 % based on purely climatic and combined climatic and edaphic data

# Climatic data
rows_remove_clim <- apply(data_coverage_islandgroups[, c(3:21)], 1, function(x) any(x < 50 | is.na(x)))
islandgroups_clim <- data_coverage_islandgroups[!rows_remove_clim,]

save(islandgroups_clim, file = "input_data/spatial_data/islandgroups_clim.RData")

# Climatic and edaphic data
rows_remove_edaclim <- apply(data_coverage_islandgroups[, c(3:35)], 1, function(x) any(x < 50 | is.na(x)))
islandgroups_edaclim <- data_coverage_islandgroups[!rows_remove_edaclim,]

save(islandgroups_edaclim, file = "input_data/spatial_data/islandgroups_edaclim.RData")

# Find the minimum value for each island group
min_values <- apply(data_coverage_islandgroups[,c(3:35)], 1, min)
