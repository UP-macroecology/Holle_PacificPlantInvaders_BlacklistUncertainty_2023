# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#         02b_EXTRA. Background data based on global occurrences         # 
#               in latitudinal bands to avoid memory error               #
# ---------------------------------------------------------------------- #

# Load needed packages
library(terra)
library(sf)
library(sfheaders)
library(purrr)
library(furrr)
library(dplyr)
library(stringr)
library(ggplot2)

# Load needed objects
world_mask <- terra::rast("input_data/spatial_data/world_mask.tif") # mask with 1 km resolution
load("input_data/occurrence_numbers_thinned_filtered.RData") # data frame that contains study species names
load("input_data/occurrences_Hawaii.RData") # data frame that contains coordinate and biogeographical status information
source("scripts/00_functions.R") # thin function


#-------------------------------------------------------------------------------


# 1. Create latitudinal bands --------------------------------------------------

# Create longitudinal values for the blocks based on 10 different blocks
n_blocks <- 10
lon_ranges = str_replace_all(levels(cut_interval(-180:180, n_blocks)), "\\[|\\]|\\(|\\)", "")

# Make a data frame that contains the values of the calculated bands
lon_ranges_df <- data.frame(lon_1 = numeric(10),
                            lon_2 = numeric(10))

lon_1_values <- c(-180, -144, -108, -72, -36, 0, 36, 72, 108, 144)
lon_2_values <- c(-144, -108, -72, -36, 0, 36, 72, 108, 144, 180)

lon_ranges_df$lon_1 <- lon_1_values
lon_ranges_df$lon_2 <- lon_2_values

# Calculate the longitudinal degrees of a 9 km distance which is considered as
# minimum overlap between the bands to make sure that the generated absences are
# also thinned at the band edges

# Use a latitude of 80 for calculations as the longitudinal distance per degree is
# lower than at the equator
longitudinal_distance_per_degree <- (40075 * cos(80 * pi / 180)) / 360
degrees_longitude_9km <- round((9 / longitudinal_distance_per_degree), 2)


#-------------------------------------------------------------------------------


# 2. Background data generation ------------------------------------------------

# Retrieve names of study species
# study_species <- unique(as.character(occurrence_numbers_thinned_filtered$species))

study_species <- "Cirsium_vulgare"

# Loop over all species and generate background data
for (sp in study_species) { # Start of loop over species
  try({
    
    print(sp)
    
    # check if distribution data file already exists
    file_exists_1 <- file.exists(paste0("output_data/distribution_data/global/species_occ_global_",sp,".RData"))
    
    
    if (file_exists_1 == FALSE) { # just continue with background data if output distribution 
    # data does not exist yet due to memory problems of large file size
      
      print("start of process")
      print("absence data generation")
      
      # Load in the thinned occurrence data of the species
      load(paste0("output_data/presences_thinned/species_presences_thinned_",sp,".RData"))
      
      # Extract coordinate information
      species_presences_thinned_global_coord <- species_presences_thinned[c("lon", "lat")]
      
      # Prepare the thinned presences data to contain a column indicating 1 for presence
      species_presences_thinned_global_coord <- data.frame(species_presences_thinned_global_coord, occ=1)
      
      # Prepare the data frame with the presences to add the absences later on
      species_occ_global_raw <- species_presences_thinned_global_coord
        
      # Create a subset for each species based on all occurrences 
      subset_species <- subset(occurrences_Hawaii, occurrences_Hawaii$species == sp)

      # Remove rows with duplicate numbers at a 1 km resolution of all species occurrences
      cellnumbers <- terra::extract(world_mask, subset_species[c("lon", "lat")], cells = TRUE)
      subset_species_wd <- subset_species[!duplicated(cellnumbers[, "cell"]), ]
        
      # Extract the coordinates of the occurrences
      presences_coords <- subset_species_wd[c("lon", "lat")]
        
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
        
      abs_indices_200 <- sample(diff_cells_200, ifelse(length(diff_cells_200) < nrow(subset_species_wd)*10, length(diff_cells_200), nrow(subset_species_wd)*10))
      abs_coords_200 <- as.data.frame(xyFromCell(buf_200, abs_indices_200))
      colnames(abs_coords_200) = c("lon", "lat")
        
        
#-------------------------------------------------------------------------------
        
        
# 3. Background data thinning --------------------------------------------------
    
      for (l in 1:11) { # Start of the loop over all defined latitudinal bands + one extra band
        try({
          
        # Check if file of the latitudinal band already 
        file_exists_2 <- file.exists(paste0("output_data/absences_thinned/global_EXTRA/",l,"_species_absences_thinned_global_EXTRA_",sp,".RData"))
      
        # Just retrieve the absences in the considered latitudinal band
        abs_coords_200_block <- abs_coords_200[abs_coords_200$lon >= lon_ranges_df[l,1] & abs_coords_200$lon <= lon_ranges_df[l,2], ]
        
        
        if (nrow(abs_coords_200_block) >= 1 && file_exists_2 == FALSE) { # Just continue with thinning if there
        # are absences in the considered latitudinal block and if the file does not exist yet
        
          
          if (l == 1) { # When considering the first latitudinal band
            
            print(l)
            print("thinning absences")
            
            # Transform the coordinate information into sf object
            absences_coords_sf <- st_as_sf(abs_coords_200_block, coords = c("lon", "lat"), crs = crs(world_mask))
            
            # Spatial thinning of background data with distance of 3 km using the thin function
            species_absences_thinned_global <- thin(absences_coords_sf, thin_dist = 3000, runs = 1, ncores = 1)
            
            # Save the thinned background data for the species
            save(species_absences_thinned_global, file = paste0("output_data/absences_thinned/global_EXTRA/",l,"_species_absences_thinned_global_EXTRA_",sp,".RData"))
            
            # Find the longitudinal coordinate that lays ~ 9 km away from the next block coordinate
            lon_coord_9km <- lon_ranges_df[l,2] - degrees_longitude_9km
            
            # Extract the absences within 9 km to the next block to later add them to the next thinning block
            species_absences_thinned_block_9km <- species_absences_thinned_global[species_absences_thinned_global$lon >= lon_coord_9km & species_absences_thinned_global$lon <= lon_ranges_df[l,2],]
          
            
         } else if (l >= 2 && l <= 10) { # If the latitudinal band is number 2 to 10
            
            print(l)
            print("thinning absences")
            
            # Add the thinned absences of 9 km distance within the previous block 
            # to the current block to make sure that absence are also thinned to 3 km 
            # at the block edges
            abs_coords_200_block <- rbind(abs_coords_200_block, species_absences_thinned_block_9km)
            
            # Transform the coordinate information into sf object
            absences_coords_sf <- st_as_sf(abs_coords_200_block, coords = c("lon", "lat"), crs = crs(world_mask))
            
            # Spatial thinning of background data with distance of 3 km using the thin function
            species_absences_thinned_global <- thin(absences_coords_sf, thin_dist = 3000, runs = 1, ncores = 1)
            
            # Save the thinned background data for the species
            save(species_absences_thinned_global, file = paste0("output_data/absences_thinned/global_EXTRA/",l,"_species_absences_thinned_global_EXTRA_",sp,".RData"))
            
            # Find the longitudinal coordinate that lays ~ 9 km away from the next block coordinate
            lon_coord_9km <- lon_ranges_df[l,2] - degrees_longitude_9km
            
            # Extract the absences within 9 km to the next block to later add them to the next thinning block
            species_absences_thinned_block_9km <- species_absences_thinned_global[species_absences_thinned_global$lon >= lon_coord_9km & species_absences_thinned_global$lon <= lon_ranges_df[l,2],]
            
            # Prepare the thinned absence data to contain a  column indicating 0 for absence
            species_absences_thinned_global$occ <- 0
            
            # Bind the thinned presences with thinned absences of each block
            species_occ_global_raw <- rbind(species_occ_global_raw, species_absences_thinned_global)
            
            # Save the file with the thinned absences in between
            save(species_occ_global_raw, file = paste0("output_data/distribution_data/global_EXTRA/species_occ_global_raw_",sp,".RData"))
            
            
          } else if (l == 11) { # When considering the extra block
            
            print(l)
            print("thinning absences")
            
            # Load the thinned absences from the first block
            load(paste0("output_data/absences_thinned/global_EXTRA/1_species_absences_thinned_global_EXTRA_",sp,".RData"))
            
            # Bind the thinned absences of the first block with the thinned absences of
            # 9 km distance from the tenth block
            abs_coords_200_block <- rbind(species_absences_thinned_global, species_absences_thinned_block_9km)
            
            # Transform the coordinate information into sf object
            absences_coords_sf <- st_as_sf(abs_coords_200_block, coords = c("lon", "lat"), crs = crs(world_mask))
            
            # Spatial thinning of background data with distance of 3 km using the thin function
            species_absences_thinned_global <- thin(absences_coords_sf, thin_dist = 3000, runs = 1, ncores = 1)
            
            # Save the thinned background data for the species
            save(species_absences_thinned_global, file = paste0("output_data/absences_thinned/global_EXTRA/",l,"_species_absences_thinned_global_EXTRA_",sp,".RData"))
            
            # Prepare the thinned absence data to contain a  column indicating 0 for absence
            species_absences_thinned_global$occ <- 0
            
            # Bind the thinned presences with thinned absences of each block
            species_occ_global_raw <- rbind(species_occ_global_raw, species_absences_thinned_global)
            
            # Save the file with the thinned absences in between
            save(species_occ_global_raw, file = paste0("output_data/distribution_data/global_EXTRA/species_occ_global_raw_",sp,".RData"))
            
          } # End of if conditions depending on the considered latitudinal band
          
          
        } else if (nrow(abs_coords_200_block) == 0) { print(l)
                                                      print("no absences for thinning")
                                                      species_absences_thinned_block_9km = NULL
        } else if (file_exists_2 == TRUE) { print(l)
                                            print("already done")
                                            
                                            # Load the thinned absences of that band
                                            load(paste0("output_data/absences_thinned/global_EXTRA/",l,"_species_absences_thinned_global_EXTRA_",sp,".RData"))
                                            
                                            # Extract the thinned absences in a maximum 9 km distance of the band to add it to next thinning band
                                            lon_coord_9km <- lon_ranges_df[l,2] - degrees_longitude_9km
                                            species_absences_thinned_block_9km <- species_absences_thinned_global[species_absences_thinned_global$lon >= lon_coord_9km & species_absences_thinned_global$lon <= lon_ranges_df[l,2],]
                                            
                                            if (l >= 2 && l <= 11) { # Prepare the thinned absence data to contain a  column indicating 0 for absence
                                                                     species_absences_thinned_global$occ <- 0
                                              
                                                                     # Bind the thinned presences with thinned absences of each block
                                                                     species_occ_global_raw <- rbind(species_occ_global_raw, species_absences_thinned_global)
                                                                     
                                                                     # Save the file with the thinned absences in between
                                                                     save(species_occ_global_raw, file = paste0("output_data/distribution_data/global_EXTRA/species_occ_global_raw_",sp,".RData"))
                                              
                                            } else if (l == 1) {next
                                            }
                                            
                                            
        } # End of if condition
        
        
      })} # End the loop over all latitudinal blocks

    
          
#-------------------------------------------------------------------------------
      
      
# 4. Process joined thinned presence and absence data --------------------------
    
    # Load the data frame with presences and thinned absences 
    load(paste0("output_data/distribution_data/global_EXTRA/species_occ_global_raw_",sp,".RData"))
      
    # Remove duplicate absences as these may occur in the overlapping areas
    check_dup <- terra::extract(world_mask, species_occ_global_raw[c("lon", "lat")], cells = TRUE)
    species_occ_global <- species_occ_global_raw[!duplicated(check_dup[, "cell"]), ]
    
    
    # Save the distribution data set of the species
    save(species_occ_global, file = paste0("output_data/distribution_data/global/species_occ_global_",sp,".RData"))
    
    
    } else if (file_exists_1 == TRUE) { print("already done")
    } # end of if conditions

        
})} # end of try and for loop over species



#-------------------------------------------------------------------------------


# 4. Plot thinned presence and absence data ------------------------------------

for (sp in study_species) {
  try({

    print(sp)

    file_exists_1 <- file.exists(paste0("output_data/distribution_data/global/species_occ_global_",sp,".RData"))

    file_exists_2 <- file.exists(paste0("output_data/plots/presence_absence_plots/",sp,"/presence_absence_global_",sp,".svg"))

    if (file_exists_1 == TRUE && file_exists_2 == FALSE) {

      print("plot presence-absence points")

      load(paste0("output_data/distribution_data/global/species_occ_global_",sp,".RData"))

      presences <- subset(species_occ_global, species_occ_global$occ == 1)
      absences <- subset(species_occ_global, species_occ_global$occ == 0)

      dir.create(paste0("output_data/plots/presence_absence_plots/",sp))

      svg(paste0("output_data/plots/presence_absence_plots/",sp,"/presence_absence_global_",sp,".svg"))
      plot(world_mask,col='grey',legend=F, main = sp)
      points(absences$lon, absences$lat, col = "black", pch = 20)
      points(presences$lon, presences$lat, col = "red", pch = 20)

      dev.off()

    } else if (file_exists_1 == FALSE) { print("file not available yet")
    } else if (file_exists_2 == TRUE) { print("plot already generated")
    }

})}


