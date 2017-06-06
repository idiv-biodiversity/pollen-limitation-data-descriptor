# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Whittaker diagram for biomes and study locations
# I used the biome polygons Fig 5.5 p.92 in Ricklefs – The Economy of Nature, Chapter 5, Biological Communities, The biome concept.
# (book at: https://www.academia.edu/15092278/Ricklefs_The_Economy_of_Nature_6th_txtbk)
# The polygons were digitized in QGIS - see script Whittaker_biomes_prepare.R for details.
# Temperature and precipitation data extractions used for plotting were done with Extract_temp_rainfall.R script
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(ggplot2)
library(broom)
library(data.table)

# =============================================
# read & prepare data
# =============================================
# Extractions were doen with Extract_temp_rainfall.R script
extr.dt <- fread("Output/extractions_temp_pp.csv")
# transform from mm to cm for precipitation
extr.dt[, Annual_pp_cm := Annual_pp_mm/10] 
# get unique pairs of coordinates of precipitation & temperature (not longitude-latitude!)
extr.dt.unq <- unique(extr.dt, 
                      by = c("Annual_mean_temp_C", "Annual_pp_cm") )

# get Whittaker biome polygons directly as data frame ready to use in ggplot
biomes.dt <- fread("Data/Whittaker_biomes.csv")

# =============================================
# plot with ggplot
# =============================================
ggplot() + 
    # add polygons
    geom_polygon(data = biomes.dt,
                 aes(x      = temp_C,
                     y      = precp_cm,
                     fill   = Biome,
                     group  = Biome),
                 colour = "gray98", # for polygon border
                 size   = 1) +     # this is the thikness of the line separating the polygons
    # adjust the coloring of the polygons (biomes)
    # colors and labels correspond to Fig 5.5, p92, Ch5 from  Ricklefs The Economy of Nature 6th txtbk
    # at: https://www.academia.edu/15092278/Ricklefs_The_Economy_of_Nature_6th_txtbk
    scale_fill_manual(name   = "Biomes", # this will appear as the name of the legend as well
                      breaks = sort(unique(biomes.dt$Biome)),
                      labels = sort(unique(biomes.dt$Biome)),
                      values = c("Boreal forest"                   = "#a5c890",
                                 "Subtropical desert"              = "#dcbb50",
                                 "Temperate grassland/desert"      = "#fdd67a",
                                 "Temperate rain forest"           = "#75a95e",
                                 "Temperate seasonal forest"       = "#97b669",
                                 "Tropical rain forest"            = "#317a21",
                                 "Tropical seasonal forest/savana" = "#a09700",
                                 "Tundra"                          = "#c2e1dd",
                                 "Woodland/shrubland"              = "#d26e3f")) +
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
    # set range on OY axes and adjust the distance (gap) from OX axes
    scale_y_continuous(limits = c(-5, round(max(extr.dt.unq$Annual_pp_cm, na.rm = TRUE))+10), 
                       expand = c(0, 0)) +
    # set the general ggplot theme
    theme_bw() +
    # adjust legend position
    theme(legend.justification = c(0, 1),     # set the upper left corner of the legend box
          legend.position = c(0.05, 0.9),     # adjust the position of the corner as relative to axis
          # panel.grid.major = element_blank(), # eliminate major grids
          panel.grid.minor = element_blank()) # eliminate minor grids

# =============================================
# Save to pdf and png file
# =============================================
ggsave(filename = file.path("Output", "Whittaker_diagram_biomes_draf4.pdf"), 
       width    = 29.7, 
       height   = 21, 
       units    = "cm")

ggsave(filename = file.path("Output", "Whittaker_diagram_biomes_draf4.png"),
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

# write shapefile to HDD to also check in QGIS/ArcMap
rgdal::writeOGR(obj=tundra.sp, dsn="Output/tundra_pts", layer="tundra_pts", driver="ESRI Shapefile")
