# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#           02a. Background data based on native occurrences             #
# ---------------------------------------------------------------------- #


# Load needed packages
library(terra)
library(sf)
library(sfheaders)
library(purrr)
library(furrr)
library(dplyr)
library(doParallel)
library(foreach)


# Load needed objects
world_mask <- terra::rast("input_data/spatial_data/world_mask.tif") # mask with 1 km resolution
load("input_data/occurrence_numbers_thinned_filtered.RData") # data frame that contains study species names
load("input_data/occurrences_Hawaii.RData") # data frame that contains coordinate and biogeographical status information
source("scripts/00_functions.R") # thin function


#-------------------------------------------------------------------------------


# 1. Background data generation ------------------------------------------------

# Retrieve names of study species
study_species <- unique(as.character(occurrence_numbers_thinned_filtered$species))

# Start parallel computing
#no_cores <- 3
#cl <- makeCluster(no_cores)
#registerDoParallel(cl)

# Loop over all species and generate background data
#foreach(sp = study_species, .packages = c("terra", "sf", "purrr", "furrr", "sfheaders")) %dopar% {


    
for (sp in study_species) { # Start of loop over species
  try({
      
  print(sp)
      
  # check if distribution data file already exists
  file_exists <- file.exists(paste0("output_data/distribution_data/native/species_occ_native_",sp,".RData"))
  
  # check the size of the file
  file_size <- file.size(paste0("output_data/presences_thinned/species_presences_thinned_",sp,".RData"))
  
  if (file_exists == FALSE && file_size <= 300000) { # just continue with background data if output distribution 
  # data does not exist yet and file is under a certain size
    
  print("start of process")
  
  # Create a subset for each species based on all occurrences 
  subset_species <- subset(occurrences_Hawaii, occurrences_Hawaii$species == sp)
  
  # Remove rows with duplicate numbers at a 1 km resolution
  cellnumbers <- terra::extract(world_mask, subset_species[c("lon", "lat")], cells = TRUE)
  subset_species_wd <- subset_species[!duplicated(cellnumbers[, "cell"]), ]
  
  # Solely retain native occurrences of the species
  subset_species_wd_native <- subset(subset_species_wd, subset_species_wd$criterion_1 == "native")
  
  # Extract the coordinates of the occurrences
  presences_coords <- subset_species_wd_native[c("lon", "lat")]
  
  # Create a points vector from the coordinates
  presences <- terra::vect(presences_coords, crs = '+proj=longlat +datum=WGS84')
  
  # Place a buffer of 200 km radius around our presence points to account for dispersal limitations
  buf_200 <- buffer(presences, width = 200000)
  
  # Create a mask with target resolution of 1 km
  mask_buf_200 <- crop(world_mask, ext(buf_200))
  values(mask_buf_200)[!is.na(values(mask_buf_200))] <- 1
  
  # Rasterize the buffer with the created mask (raster is needed for later steps)
  buf_200 <- rasterize(buf_200, mask_buf_200)
  
  # Set all raster cells outside the buffer to NA.
  buf_200 <- terra::mask(mask_buf_200, buf_200, overwrite = TRUE)
  
  # Randomly select background data within the buffer, excluding presence locations 
  # 10 times as many background data as there are thinned presences sampled
  occ_cells_200 <- terra::extract(buf_200, presences, cells = TRUE)[,"cell"]
  buf_cells_200 <- terra::extract(buf_200, crds(buf_200), cells = TRUE)[,"cell"]
  diff_cells_200 <- setdiff(buf_cells_200, occ_cells_200)
  
  abs_indices_200 <- sample(diff_cells_200, ifelse(length(diff_cells_200) < nrow(subset_species_wd_native)*10, length(diff_cells_200), nrow(subset_species_wd_native)*10))
  abs_coords_200 <- as.data.frame(xyFromCell(buf_200, abs_indices_200))
  colnames(abs_coords_200) = c("lon", "lat")
  

#-------------------------------------------------------------------------------
  
  
# 2. Background data thinning --------------------------------------------------
  
  # Transform the coordinate information into sf object
  absences_coords_sf <- st_as_sf(abs_coords_200, coords = c("lon", "lat"), crs = crs(world_mask))
  
  # Spatial thinning of background data with distance of 3 km using the thin function
  species_absences_thinned_native <- thin(absences_coords_sf, thin_dist = 3000, runs = 1, ncores = 1)
  
  # Save the thinned background data for the species
  save(species_absences_thinned_native, file = paste0("output_data/absences_thinned/native/species_absences_thinned_native_",sp,".RData"))
  
  
#-------------------------------------------------------------------------------
  
  
# 3. Join thinned presence and absence data ------------------------------------
  
  # Load in the thinned occurrence data of the species
  load(paste0("output_data/presences_thinned/species_presences_thinned_",sp,".RData"))
  
  # Subset the thinned presences by native occurrences
  species_presences_thinned_native <- subset(species_presences_thinned, species_presences_thinned$criterion_1 == "native")
  
  # Extract coordinate information
  species_presences_thinned_native_coord <- species_presences_thinned_native[c("lon", "lat")]
  
  # Prepare the thinned presences data to contain a column indicating 1 for presence
  species_presences_thinned_native_coord <- data.frame(species_presences_thinned_native_coord, occ=1)
  
  # Prepare the thinned absence data to contain a  column indicating 0 for absence
  species_absences_thinned_native$occ <- 0
  
  # Bind these two data sets
  species_occ_native <- rbind(species_presences_thinned_native_coord, species_absences_thinned_native)
  
  # Save the distribution data set of the species
  save(species_occ_native, file = paste0("output_data/distribution_data/native/species_occ_native_",sp,".RData"))
  
  } else if (file_exists == TRUE) { print("already done")
  } else if (file_size > 300000) { print("too large")
  } # end of if conditions
  
})} # end of try and for loop over species
  
#})} # end of try and foreach
  
  
#stopCluster(cl)  



#-------------------------------------------------------------------------------


# 4. Plot thinned presence and absence data ------------------------------------

for (sp in study_species) {
  try({

    print(sp)

    file_exists_1 <- file.exists(paste0("output_data/distribution_data/native/species_occ_native_",sp,".RData"))

    file_exists_2 <- file.exists(paste0("output_data/plots/presence_absence_plots/",sp,"/presence_absence_native_",sp,".svg"))

    if (file_exists_1 == TRUE && file_exists_2 == FALSE) {

      print("plot presence-absence points")

      load(paste0("output_data/distribution_data/native/species_occ_native_",sp,".RData"))

      presences <- subset(species_occ_native, species_occ_native$occ == 1)
      absences <- subset(species_occ_native, species_occ_native$occ == 0)

      dir.create(paste0("output_data/plots/presence_absence_plots/",sp))

      svg(paste0("output_data/plots/presence_absence_plots/",sp,"/presence_absence_native_",sp,".svg"))
      plot(world_mask,col='grey',legend=F, main = sp)
      points(absences$lon, absences$lat, col = "black", pch = 20)
      points(presences$lon, presences$lat, col = "red", pch = 20)

      dev.off()

      } else if (file_exists_1 == FALSE) { print("file not available yet")
      } else if (file_exists_2 == TRUE) { print("plot already generated")
      }

})}
