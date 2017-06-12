# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to extract Annual precipitation amount [mm] and Annual mean temperature [°C]  for each study location
# in the pollen limitation dataset.
# Extractions are used further for making the Whittaker diagram for biomes.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(data.table)
library(raster)
# install.packages("RANN")
library(RANN)

# Source buffer-extraction function from GitHub
source("https://raw.githubusercontent.com/valentinitnelav/helpers/master/R/extract_FromNearCellRst.R")

# set the temporary folder for raster package operations
rasterOptions(tmpdir = "Data/tmp_rst")

# read latest dataset from Joanne
myDT <- fread("Data/PL_ANALYSIS_8_06_2017.csv", colClasses = "character")

# get only columns of interest
studyXY <- myDT[,.(unique_number, lon_decimal_PTL_JMB, lat_decimal_PTL_JMB)]
studyXY[, lon_decimal_PTL_JMB := as.numeric(lon_decimal_PTL_JMB)]
studyXY[, lat_decimal_PTL_JMB := as.numeric(lat_decimal_PTL_JMB)]

# some routine checking of coordinates values
range(studyXY[,lon_decimal_PTL_JMB]) %between% c(-180, 180)
range(studyXY[,lat_decimal_PTL_JMB]) %between% c(-90, 90)

# get unique pairs of coordinates
studyXY.unq <- unique(studyXY[,.(lon_decimal_PTL_JMB, lat_decimal_PTL_JMB)], 
                      by = c("lon_decimal_PTL_JMB", "lat_decimal_PTL_JMB") )

# Read in rasters from iDiv share
# Annual precipitation amount [mm] 
rst.pp <- raster("I:/sie/_data_VS/CHELSA/CHELSA_bio12_1979-2013_V1_1.tif")
names(rst.pp) <- "Annual_pp_mm"
# Annual mean temperature [°C] 
rst.temp <- raster("I:/sie/_data_VS/CHELSA/CHELSA_bio1_1979-2013_V1_1.tif")
names(rst.temp) <- "Annual_mean_temp_C"

# set the buffer in meters
buf.m <- 50000

# Extract from list of rasters
system.time(
    extr.lst <- lapply(X   = list(rst.pp, rst.temp),
                       FUN = extract_FromNearCellRst,
                       XY  = studyXY.unq, 
                       my.buffer  = buf.m,
                       simplified = FALSE,
                       lib.check  = TRUE)
)

# create a column for each extraction table that indicates which raster was used
lapply(extr.lst, function(dt) dt[, varb := names(dt)[4]])

# rbind all table extractions from the extraction list
extr.dt <- rbindlist(extr.lst)

# rename column 4
data.table::setnames(extr.dt, 
                     old = 4, 
                     new = "extracted.val")

# transform from long to wide format
extr.dt.wide <- dcast(extr.dt, 
                      lon_decimal_PTL_JMB + lat_decimal_PTL_JMB  ~ varb, 
                      value.var = "extracted.val")

# join unique extraction results with data for each location
extr.res <- merge(x  = studyXY,
                  y  = extr.dt.wide,
                  by = c("lon_decimal_PTL_JMB",  "lat_decimal_PTL_JMB"),
                  all.x = TRUE, 
                  sort  = FALSE)
str(extr.res)
# save results
write.csv(extr.res, "Output/extractions_temp_pp.csv", row.names = FALSE)
