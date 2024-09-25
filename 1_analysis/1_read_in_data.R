library(terra)
library(dplyr)

setwd("/exports/csce/datastore/geos/users/s1318698/FORVAC_matching")

## Read in rasters

L <- list()
L$liwale_def <- rast("0_data/1_format/2_province_clip/Lindi/aligned/forest_loss_resampled_byte_resampled.tif")
L$mc_def <- rast("0_data/1_format/2_province_clip/Lindi/aligned/mcnicol_def_resampled_byte_resampled.tif")
L$hansen_def <- rast("0_data/1_format/2_province_clip/Lindi/aligned/Tz_hansen_lossyear_resampled.tif")
L$convrt <- rast("0_data/1_format/2_province_clip/Lindi/aligned/lindi_dist_converted_37S_resampled.tif")
L$roads <- rast("0_data/1_format/2_province_clip/Lindi/aligned/lindi_dist_roads_37S_resampled.tif")
L$setl <- rast("0_data/1_format/2_province_clip/Lindi/aligned/lindi_dist_settlements_37S_resampled.tif")
L$wedge <- rast("0_data/1_format/2_province_clip/Lindi/aligned/lindi_dist_woodland_edge_37S_resampled.tif")
L$maize <- rast("0_data/1_format/2_province_clip/Lindi/aligned/maize_resampled_byte_resampled.tif")
L$agc <- rast("0_data/1_format/2_province_clip/Lindi/aligned/mcnicol_ALOS1_agc_4326_resampled_byte_resampled.tif")
L$tt <- rast("0_data/1_format/2_province_clip/Lindi/aligned/travel_time_resampled_int16_resampled.tif")
L$pop <- rast("0_data/1_format/2_province_clip/Lindi/aligned/Lindi_pop_sum_resampled.tif")
L$PA <- rast("0_data/1_format/2_province_clip/Lindi/aligned/lindi_PAs_resampled.tif")
L$FR <- rast("0_data/1_format/2_province_clip/Lindi/aligned/lindi_VFRs_resampled.tif")
L$VLFR <- rast("0_data/1_format/2_province_clip/Lindi/aligned/lindi_VLFRs_resampled.tif")
L$PA_buff <- rast("0_data/1_format/2_province_clip/Lindi/aligned/lindi_PAs_buffer_resampled.tif")
L$FR_buff <- rast("0_data/1_format/2_province_clip/Lindi/aligned/lindi_VFR_buffer_resampled.tif")
L$VLFR_buff <- rast("0_data/1_format/2_province_clip/Lindi/aligned/lindi_VLFR_buffer_resampled.tif")
L$district <- rast("0_data/1_format/2_province_clip/Lindi/aligned/lindi_districts_37S_resampled.tif")

RSt <- rast(L)
print("Rasters in")

df <- terra::as.data.frame(RSt)

## Filter to wooded (removed unncessary data)
df2 <- df %>% filter(agc > 10 & !is.na(agc))

## Save df
saveRDS(df2, "0_data/1_format/2_province_clip/Lindi/df.Rds")
