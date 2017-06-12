# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prepare digitized Whittaker biomes polygons for plotting in ggplot.
# Transform from shapefile to dataframe.

# The Whittaker biomes polygons were digitized in QGIS after georeferencing
# the graph in Fig 5.5 p. 92 from Ricklefs – The Economy of Nature 
# (at: https://www.academia.edu/15092278/Ricklefs_The_Economy_of_Nature_6th_txtbk)
# Georeferencing was done using the EPSG:53002 (sphere equidistant cylindrical) CRS.
# I used 4 ground control points located in 4 corners of the graph, with X within -100 and 300 
# instead of -10 and 30 in order to avoid distortions.
# Digitization was carried at scale 1:500. The file was saved as biomes.shp
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(sp)
library(rgdal)
library(broom)

# Load from GitHub function to convert from SpatialPolygonsDataFrame to data frame while keeping attributes
source("https://raw.githubusercontent.com/valentinitnelav/helpers/master/R/from_PolyDF2DF.R")

# read shapefile
biomes.poly <- rgdal::readOGR(dsn   = "Whittaker biomes graph - digitize", 
                              layer = "biomes")

# -----------------
# transform to data frame and save as csv
# -----------------
df.biomes_polyg <- from_PolyDF2DF(biomes.poly)

# rename columns
names(df.biomes_polyg)[1] <- "temp_C"
names(df.biomes_polyg)[2] <- "precp_cm"

# divide temperature by 10 to obtain real values.
# the original values multiplied by 10 had to be used for avoiding distortions in QGIS
df.biomes_polyg$temp_C <- df.biomes_polyg$temp_C/10

# save to csv file
write.csv(x = df.biomes_polyg, file = "Data/Whittaker_biomes.csv", row.names = FALSE)

# -----------------
# save to shapefile
# -----------------
# Adjust the temperature values (because of how was digitized, temp corresponds to longitude and needs to be divided by 10)
for (i in 1:length(biomes.poly@polygons)){
    biomes.poly@polygons[[i]]@Polygons[[1]]@coords[,1] <- biomes.poly@polygons[[i]]@Polygons[[1]]@coords[,1]/10
}
plot(biomes.poly)

writeOGR(obj    = biomes.poly,
         dsn    = "Whittaker biomes graph - digitize", 
         layer  = "Whittaker_biomes", 
         driver = "ESRI Shapefile")
