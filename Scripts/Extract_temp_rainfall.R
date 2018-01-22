###############################################################################
## Script to prepare data for plotting with Whittaker biomes.
## Extracts Annual precipitation amount [mm] and Annual mean temperature [°C] 
## for each study location in the row pollen limitation dataset.
###############################################################################

library(data.table)
library(raster)
# install.packages("RANN")
library(RANN)

# Source buffer-extraction function from GitHub
source("https://raw.githubusercontent.com/valentinitnelav/helpers/master/R/extract_FromNearCellRst.R")

# =============================================================================
# Read & prepare data
# =============================================================================
# Read latest dataset from Joanne
PL_dt <- fread("Data/PL_masters_for publication.csv",
               select = c("unique_number", 
                          "lon_decemial_unpinned", 
                          "lat_decimal_unpinned"),
               colClasses = "character")
# remove any empty rows
PL_dt <- PL_dt[unique_number != ""]

# Transform longitude to numeric
PL_dt[, lon := as.numeric(lon_decemial_unpinned)]
PL_dt[is.na(lon)] # the minus sign is not a ream minus sign
PL_dt[unique_number == "2219", lon := -7.317197]
PL_dt[, lon_decemial_unpinned := NULL]

# Transform latitude to numeric
PL_dt[, lat := as.numeric(lat_decimal_unpinned)]
PL_dt[, lat_decimal_unpinned := NULL]

# Some routine checking of coordinates values
range(PL_dt[,lon]) %between% c(-180, 180)
range(PL_dt[,lat]) %between% c(-90, 90)

# =============================================================================
# Raster extractions
# =============================================================================
# Get unique pairs of coordinates
xy_unq <- unique(PL_dt[,.(lon, lat)], by = c("lon", "lat") )

# Read in rasters from iDiv share:
# - Annual precipitation amount [mm] 
rst_pp <- raster("I:/sie/_data_VS/CHELSA/chelsa_bioclim_v1.2/CHELSA_bio_12.tif")
names(rst_pp) <- "Annual_pp_mm"
# - Annual mean temperature [°C] 
rst_temp <- raster("I:/sie/_data_VS/CHELSA/chelsa_bioclim_v1.2/CHELSA_bio_1.tif")
names(rst_temp) <- "Annual_mean_temp_C"

# Set the search buffer in meters. It will search for closest non-NA cell.
buf_m <- 50000

# Extract from list of rasters
system.time(
    extr_lst <- lapply(X   = list(rst_pp, rst_temp),
                       FUN = extract_FromNearCellRst,
                       XY  = xy_unq, 
                       my.buffer  = buf_m,
                       simplified = FALSE,
                       lib.check  = TRUE)
)
# takes approx 1 min

# Create a column for each extraction table that indicates which raster was used
lapply(extr_lst, function(dt) dt[, varb := names(dt)[4]])

# rbind all table extractions from the extraction list
extr_dt <- rbindlist(extr_lst)

# Rename column 4
data.table::setnames(extr_dt, 
                     old = 4, 
                     new = "extracted_val")

# Transform from long to wide format
extr_dt_wide <- dcast(extr_dt, 
                      lon + lat ~ varb, 
                      value.var = "extracted_val")

# Join unique extraction results with original table
extr_res <- merge(x  = PL_dt,
                  y  = extr_dt_wide,
                  by = c("lon",  "lat"),
                  all.x = TRUE, 
                  sort  = FALSE)
str(extr_res)
# save results
write.csv(extr_res, "Output/extractions_temp_pp.csv", row.names = FALSE)
