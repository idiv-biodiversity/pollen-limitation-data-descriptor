# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to extract for each study the Whittaker biomes.
# The Whittaker biomes polygons were digitized in QGIS after georeferencing
# the graph in Fig 5.5 p. 92 from Ricklefs – The Economy of Nature 
# (at: https://www.academia.edu/15092278/Ricklefs_The_Economy_of_Nature_6th_txtbk)
# see script Whittaker_biomes_prepare.R for details.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(sp)
library(rgdal)
library(data.table)

# read study data - temp & pp
# previous temp & pp extractions were done with Extract_temp_rainfall.R script
extr.dt <- fread("Output/extractions_temp_pp.csv", 
                 drop = c("lon_decimal_PTL_JMB",
                          "lat_decimal_PTL_JMB"),
                 colClasses = "character")

extr.dt[, Annual_mean_temp_C := as.numeric(Annual_mean_temp_C)]
extr.dt[, Annual_pp_mm := as.numeric(Annual_pp_mm)]

# transform from mm to cm for precipitation
extr.dt[, Annual_pp_cm := Annual_pp_mm/10] 

# read Whittaker biomes as polygons
biomes_polyg <- rgdal::readOGR(dsn    = "Whittaker biomes graph - digitize", 
                               layer  = "Whittaker_biomes")

# transform studies data frame to spatial point object
PointsSP <- SpatialPoints(coords = extr.dt[, c("Annual_mean_temp_C", 
                                               "Annual_pp_cm")])

# plot(biomes_polyg); points(PointsSP)

# extract biomes for each study location based on temperature - precipiation pair
extr.dt[, Whittaker_biomes := sp::over(PointsSP, biomes_polyg)]

# save results to csv file
write.csv(x = extr.dt, file = "Output/Studies_with_temp_pp_Whittaker_biomes.csv", row.names = FALSE)

