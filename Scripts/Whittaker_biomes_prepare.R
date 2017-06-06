# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prepare Whittaker biomes polygons for plotting in ggplot.
# Transform from shapefile to dataframe.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(sp)
library(rgdal)
library(broom)

# read shapefile
biomes.poly <- rgdal::readOGR(dsn   = "Whittaker biomes graph - digitize", 
                              layer = "biomes")

# transform to data frame
df.biomes_polyg <- broom::tidy(biomes.poly)

# save to csv file
write.csv(x = df.biomes_polyg, file = "Data/Whittaker_biomes.csv", row.names = FALSE)
