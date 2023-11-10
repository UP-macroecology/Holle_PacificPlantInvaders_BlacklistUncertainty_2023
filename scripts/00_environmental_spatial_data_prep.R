# Uncertainty paper


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#             01b. Environmental and spatial data preparation            #
# ---------------------------------------------------------------------- #

# Set working directory
setwd("/import/ecoc9z/data-zurell/holle/uncertainty_paper")

# Required path 
path_imp <- file.path("/import/ecoc9z/data-zurell/holle/uncertainty_paper")

#-------------------------------------------------------------------------------

# 1. Create a background mask --------------------------------------------------

# Background mask for pseudo-absence generation and spatial thinning with a target
# resolution and extent from climate layers

# Read in climate data and stack all the variables in the same file
chelsa_variables <- terra::rast(str_sort(list.files("environmental_data/CHELSA_V1", 
                    pattern = ".tif", full.names = TRUE), numeric = TRUE))

world_mask <- chelsa_variables[[1]]

# Set all raster cells outside the buffer to NA
values(world_mask)[!is.na(values(world_mask))] <- 1

# Write a raster
terra::writeRaster(world_mask, "data_input/world_mask.tif", overwrite = TRUE)