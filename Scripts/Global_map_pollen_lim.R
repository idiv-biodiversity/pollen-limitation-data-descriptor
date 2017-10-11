####################################################################################
## Global map for pollen limitation dataset
####################################################################################

# List of packages for session (add here any new desired packages)
.myPackages = c("rgdal", "sp", "data.table", "ggplot2", "ggmap", "ggalt")
# Install CRAN packages (if not already installed)
.inst <- .myPackages %in% installed.packages()
if(length(.myPackages[!.inst]) > 0) install.packages(.myPackages[!.inst])
# Load packages into session 
sapply(.myPackages, require, character.only=TRUE)

# =================================================================================
# Read & prepare data
# =================================================================================
# read latest dataset (prepared by Joanne)
PL.all.dt <- fread("Data/PL_ANALYSIS_02_10_2017.csv", colClasses = "character")

# get only columns of interest
PL.dt <- PL.all.dt[,.(unique_number, lon_decimal_PTL_JMB, lat_decimal_PTL_JMB)]
PL.dt[, lon_decimal_PTL_JMB := as.numeric(lon_decimal_PTL_JMB)]
PL.dt[, lat_decimal_PTL_JMB := as.numeric(lat_decimal_PTL_JMB)]

# some routine checking of coordinates values
range(PL.dt[,lon_decimal_PTL_JMB]) %between% c(-180, 180)
range(PL.dt[,lat_decimal_PTL_JMB]) %between% c(-90, 90)

# get unique pairs of coordinates
PL.dt.unq <- unique( PL.dt[,.(lon_decimal_PTL_JMB, 
                              lat_decimal_PTL_JMB)], 
                     by = c("lon_decimal_PTL_JMB", 
                            "lat_decimal_PTL_JMB") )

# transform long-lat of data in Robinson coordinates
PL.dt.unq[, c("X.prj","Y.prj") := data.table(rgdal::project(xy   = cbind(lon_decimal_PTL_JMB,
                                                                         lat_decimal_PTL_JMB),
                                                            proj = "+init=ESRI:54030"))]

# ==================================
# Load & prepare NaturalEarth shapefiles
# ==================================
load("Data/NaturalEarth.RData")
# Details about NaturalEarth shapefiles:
#   The files were already downloaded from http://www.naturalearthdata.com/
#   Graticules were adjusted to 10 dg for latitude lines and 20 dg for longitude lines (editing was carried in ArcMap)

# Project from long-lat (unprojected data) to Robinson projection
NE_countries_rob  <- spTransform(NE_countries, CRS("+proj=robin"))
NE_graticules_rob <- spTransform(NE_graticules, CRS("+proj=robin"))
NE_box_rob        <- spTransform(NE_box, CRS("+proj=robin"))

# Project labales for graticules to Robinson
lbl.Y <- cbind(lbl.Y, project(cbind(lbl.Y$lon, lbl.Y$lat), proj="+proj=robin"))
names(lbl.Y)[5:6] <- c("X.prj","Y.prj")
lbl.X <- cbind(lbl.X, project(cbind(lbl.X$lon, lbl.X$lat), proj="+proj=robin"))
names(lbl.X)[5:6] <- c("X.prj","Y.prj")

# ==================================
# plot
# ==================================
ggplot() +
    # ___ add graticules projected to Robinson
    geom_path(data      = NE_graticules_rob, 
              aes(x     = long, 
                  y     = lat, 
                  group = group), 
              linetype  = "dotted", 
              color     = "grey50", 
              size      = 0.25) +
    # ___ add Natural Earth countries projected to Robinson
    geom_polygon(data      = NE_countries_rob, 
                 aes(x     = long,
                     y     = lat, 
                     group = group), 
                 colour    = "grey60", 
                 fill      = "gray95", 
                 size      = 0.25) +
    # ___ add graticule labels - latitude and longitude
    geom_text(data      = lbl.Y, 
              aes(x     = X.prj, 
                  y     = Y.prj, 
                  label = lbl), 
              color     = "grey50", 
              size      = 2.5) +
    geom_text(data      = lbl.X, 
              aes(x     = X.prj, 
                  y     = Y.prj, 
                  label = lbl), 
              color     = "grey50", 
              size      = 2.5) +
    # ___ add Natural Earth box projected to Robinson
    geom_polygon(data   = NE_box_rob, 
                 aes(x  = long, 
                     y  = lat), 
                 colour ="black", 
                 fill   ="transparent", 
                 size   = 0.25) +
    # "Regions defined for each Polygons" warning has to do with fortify transformation. Might get deprecated in future!
    # ___ add the XY points
    geom_point(data   = PL.dt.unq, 
               aes(x  = X.prj, 
                   y  = Y.prj), 
               size   = 2,
               shape  = 21,   # the shape of the point is a circle
               colour = "black", 
               bg     = "gray40", # give color for the cricle edge and also for bakground (bg)
               alpha  = 1) + # set opacity level 
    # ___ the default ratio = 1 in coord_fixed ensures that one unit on the x-axis is the same length as one unit on the y-axis
    coord_fixed(ratio = 1) +
    # Remove the background and default gridlines with theme_nothing() from the ggmap package
    # ___ remove the background and default gridlines
    theme_void() +
    # ___ set margins
    theme(plot.margin = unit(c(t=0, r=0, b=0, l=0), unit="cm"))

# save to pdf and png file
ggsave("Output/Global_map_draft_11Oct17.pdf", width=14, height=7, units="cm", scale=2.5)
ggsave("Output/Global_map_draft_11Oct17.png", width=14, height=7, units="cm", scale=2.5, dpi=600)
