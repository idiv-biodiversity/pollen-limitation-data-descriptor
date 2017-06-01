# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Whittaker diagram for biomes and study locations
# I used the biome polygons from BIOMEplot package at https://github.com/kunstler/BIOMEplot/blob/master/R/biomes-plot.R
# Note the the package inversed the axis: temperature on OY and precipitation on OX
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(data.table)
# library(rgeos)
# library(maptools)
library(ggplot2)
library(broom)

library(devtools)
install_github("kunstler/BIOMEplot")
# source("https://raw.githubusercontent.com/kunstler/BIOMEplot/master/R/biomes-plot.R")
# plot_biome()

# =============================================
# read & prepare data
# =============================================
extr.dt <- fread("Output/extractions_temp_pp.csv")
# transform from mm to cm for precipitation
extr.dt[, Annual_pp_cm := Annual_pp_mm/10] 
# get unique pairs of coordinates of precipitation & temperature (not longitude-latitude!)
extr.dt.unq <- unique(extr.dt, 
                      by = c("Annual_mean_temp_C", "Annual_pp_cm") )

# get biome polygons with BIOMEplot package at https://github.com/kunstler/BIOMEplot/blob/master/R/biomes-plot.R
# The author referred to the biome classification:
# <<The Biomes are the Whittaker' biomes modified by Ricklefs (2008) in function
# of mean annual temperature (MAT) and mean annual precipitation (MAP)>>
# Ricklefs, R. E. (2008). The economy of nature. W. H. Freeman and Company. Chapter 5, Biological Communities, The biome concept.
biomes_polyg <- BIOMEplot::fun.poly.obj()
biomes_polyg <- biomes_polyg$poly.DF
# plot(biomes_polyg)

# biomes_polyg.rot <- maptools::elide(biomes_polyg, rotate=270, center=rgeos::gCentroid(biomes_polyg)@coords)
# plot(biomes_polyg.rot)
# BIOMEplot::plot_biome(add.legend=TRUE)

# prepare polygons for ggplot
df.biomes_polyg <- broom::tidy(biomes_polyg)

# =============================================
# plot with ggplot
# =============================================
ggplot() + 
    # add polygons
    geom_polygon(data = df.biomes_polyg,
                 aes(x      = lat,  # lat is temperature in BIOMEplot package's biome polygons
                     y      = long, # and long is precipitations (check the structure of df.biomes_polyg for clarifications)
                     fill   = id,   # fill with colour and group biome polygons by column id in given data frame
                     group  = id),
                 colour = "white",  # polygon border
                 size   = 1) + # this is the thikness of the line separating the polygons
    # adjust the coloring of the polygons (biomes)
    # colors were the ones used in BIOMEplot package
    scale_fill_manual(name   = "Biomes", # this will appear as the name of the legend as well
                      breaks = unique(df.biomes_polyg$id),
                      labels = c("Subtropical desert",
                                 "Temperate grassland/desert",
                                 "Woodland/shrubland",
                                 "Temperate seasonal forest",
                                 "Boreal forest",
                                 "Temperate rain forest",
                                 "Tropical rain forest",
                                 "Tropical seasonal forest/savanna",
                                 "Tundra"),
                      values = c("Subtropical desert"         = "#dcbb50",
                                 "Temperate grassland desert" = "#fdd67a",
                                 "Woodland shrubland"         = "#d26e3f",
                                 "Temperate forest"           = "#97b669",
                                 "Boreal forest"              = "#a5c890",
                                 "Temperate rain forest"      = "#75a95e",
                                 "Tropical rain forest"       = "#317a21",
                                 "Tropical forest savanna"    = "#a09700",
                                 "Tundra"                     = "#c2e1dd")) +
    # add unique combinations of precipitation & temperature
    geom_point(data = extr.dt.unq, 
               aes(x      = Annual_mean_temp_C, 
                   y      = Annual_pp_cm), 
               size   = 2,
               shape  = 21,   # the shape of the point is a circle
               colour = "Blue 4", bg = "Deep Sky Blue 4", # give color for the cricle edge and also for bakground (bg)
               alpha  = 1) + # set opacity level 
    # overwrite axis titles
    labs(x = "Mean annual temperature (°C)",
         y = "Mean annual precipitation (cm)") +  
    # set the general ggplot theme
    theme_classic() +
    # adjust legend position
    theme(legend.justification = c(0, 1),   # set the upper left corner of the legend box
          legend.position = c(0.05, 0.9))   # adjust the position of the corner as relative to axis

# =============================================
# Save to pdf and png file
# =============================================
ggsave(filename = file.path("Output", "Whittaker_diagram_biomes_draf3.pdf"), 
       width    = 29.7, 
       height   = 21, 
       units    = "cm")

ggsave(filename = file.path("Output", "Whittaker_diagram_biomes_draf3.png"),
       width    = 29.7, 
       height   = 21, 
       units    = "cm",
       dpi      = 600)

# =============================================
# Diagnostics & check for outliers
# =============================================
library(sp)
# library(rnaturalearth)
library(mapview)
library(rgdal)

# extr.dt[Annual_pp_cm>100 & Annual_mean_temp_C %between% c(-10,-5),]

# ----------------------
# Detect thos points outside of graph
# ----------------------
PointsSP <- SpatialPoints(coords = extr.dt[!is.na(Annual_pp_cm), 
                                           c("Annual_pp_cm", 
                                             "Annual_mean_temp_C")])
my.over <- sp::over(PointsSP, biomes_polyg)
outside <- extr.dt[!is.na(Annual_pp_cm),][is.na(my.over$biomes),]
outside.unq <- unique(outside, by = c("Annual_pp_cm","Annual_mean_temp_C"))
# setorder(outside.unq, Annual_mean_temp_C, Annual_pp_cm)
write.csv(outside.unq, "Output/outside_unque.csv")
write.csv(outside, "Output/outside_with_duplicates.csv")

# check on global map those locations
outside.WGS84 <- SpatialPointsDataFrame(coords      = outside[, c("lon_decimal_PTL_JMB", 
                                                                  "lat_decimal_PTL_JMB")],
                                        data        = outside,
                                        proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# download countries from Natural Earth
# spdf_world <- ne_download(scale = 110, type = 'countries')

mapviewOptions(basemaps = c("Esri.WorldShadedRelief", 
                            "Esri.WorldImagery",
                            "CartoDB.Positron",
                            "OpenTopoMap"))
# outside.map <- mapview(spdf_world, 
#                        alpha.regions = 0.1)
outside.map <-  mapview(outside.WGS84, color ="red")
outside.map
# save as html
mapshot(outside.map, url = "outside_map.html")

# ----------------------
# why aren't points in tundra biome ?
# ----------------------
# read latest dataset from Joanne
myDT <- fread("Data/PL_ANALYSIS_24_04_2017.csv", colClasses = "character")
myDT.sbs <- myDT[Community_Type_Author %like% "tundra",
                 .(unique_number,
                   Community_Type_Author)]
tundra.dt <- merge(myDT.sbs, extr.dt,
                   by = "unique_number")
str(tundra.dt)

write.csv(tundra.dt, file = "Output/tundra_points.csv", row.names = FALSE)

tundra.sp <- SpatialPointsDataFrame(coords      = tundra.dt[, c("lon_decimal_PTL_JMB", 
                                                              "lat_decimal_PTL_JMB")],
                                    data        = tundra.dt,
                                    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
tundra.points.map <- mapview(tundra.sp, color ="red")
# save as html
mapshot(tundra.points.map, url = "tundra_points_map.html")

# write shapefile to HDD
rgdal::writeOGR(obj=tundra.sp, dsn="Output/tundra_pts", layer="tundra_pts", driver="ESRI Shapefile")
