###############################################################################
## Global map for pollen limitation dataset
###############################################################################

# List of packages for session (add here any new desired packages)
.myPackages = c("rgdal", "sp", "data.table", "ggplot2", "ggmap", "ggalt")
# Install CRAN packages (if not already installed)
.inst <- .myPackages %in% installed.packages()
if(length(.myPackages[!.inst]) > 0) install.packages(.myPackages[!.inst])
# Load packages into session 
sapply(.myPackages, require, character.only=TRUE)

# =============================================================================
# Read & prepare data
# =============================================================================
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
# "+init=ESRI:54030" same as "+proj=robin"

# =============================================================================
# Load & prepare NaturalEarth shapefiles
# =============================================================================
load("Data/NaturalEarth.RData"); setDT(lbl.Y); setDT(lbl.X)
# Details about NaturalEarth shapefiles:
#   The files were already downloaded from http://www.naturalearthdata.com/
#   Graticules were adjusted to 10 dg for latitude lines and 20 dg for longitude lines (editing was carried in ArcMap)

# Project from long-lat (unprojected) to Robinson projection
NE_countries_rob  <- spTransform(NE_countries, CRS("+proj=robin"))
NE_graticules_rob <- spTransform(NE_graticules, CRS("+proj=robin"))
NE_box_rob        <- spTransform(NE_box, CRS("+proj=robin"))

# Project labales for graticules to Robinson
lbl.Y[, c("X.prj","Y.prj") := data.table(rgdal::project(xy   = cbind(lon, lat),
                                                        proj = "+proj=robin"))]
lbl.X[, c("X.prj","Y.prj") := data.table(rgdal::project(xy   = cbind(lon, lat),
                                                        proj = "+proj=robin"))]
# Create helper columns with nudged coordinates for plotting graticule labales
# For lbl.Y nudge longitude and for lbl.X nudge latitude.
# Give nudge values in dg (if you change, re-run also the projection lines above)
my_nudge <- cbind(nudge_lon = 10, 
                  nudge_lat = 4) 
my_nudge <- rgdal::project(my_nudge, proj = "+proj=robin")
lbl.Y[, X.prj := ifelse(lon < 0, 
                        yes = X.prj - my_nudge[1,1], 
                        no = X.prj + my_nudge[1,1])]
lbl.X[, Y.prj := ifelse(lat < 0, 
                        yes = Y.prj - my_nudge[1,2], 
                        no = Y.prj + my_nudge[1,2])]

# =============================================================================
# plot
# =============================================================================
my_map <- 
    ggplot() +
    # ___ add graticules projected to Robinson
    geom_path(data = NE_graticules_rob, 
              aes(x     = long, 
                  y     = lat, 
                  group = group), 
              linetype = "dotted", 
              color    = "grey50", 
              size     = 0.1) +
    # ___ add Natural Earth countries projected to Robinson
    geom_polygon(data = NE_countries_rob, 
                 aes(x     = long,
                     y     = lat, 
                     group = group), 
                 colour = "grey50", 
                 fill   = "gray80", 
                 size   = 0.2) +
    # ___ add graticule labels - latitude and longitude
    geom_text(data = lbl.Y, 
              aes(x     = X.prj, 
                  y     = Y.prj, 
                  label = lbl), 
              color   = "grey50", 
              size    = 1) +
    geom_text(data = lbl.X, 
              aes(x     = X.prj, 
                  y     = Y.prj, 
                  label = lbl), 
              color   = "grey50", 
              size    = 1) +
    # ___ add Natural Earth box projected to Robinson
    geom_polygon(data = NE_box_rob, 
                 aes(x = long, 
                     y = lat), 
                 colour ="black", 
                 fill   ="transparent", 
                 size   = 0.2) +
    # "Regions defined for each Polygons" warning has to do with fortify transformation. Might get deprecated in future!
    # ___ add the XY points
    geom_point(data = PL.dt.unq, 
               aes(x = X.prj, 
                   y = Y.prj), 
               size   = 0.7,
               shape  = 21,   # the shape of the point is a circle
               colour = "gray30", 
               bg     = "gray50", # give color for the cricle edge and also for bakground (bg)
               alpha  = 0.5) + # set opacity level 
    # ___ the default ratio = 1 in coord_fixed ensures that one unit on the x-axis is the same length as one unit on the y-axis
    coord_fixed(ratio = 1) +
    # Remove the background and default gridlines with theme_nothing() from the ggmap package
    # ___ remove the background and default gridlines
    theme_void() +
    # ___ set margins
    theme(plot.margin = unit(c(t=-0.27, r=-0.5, b=-0.38, l=-0.6), unit="cm"))

# save to pdf and png file
ggsave(plot = my_map, filename = "Output/Global_map_draft_08Jan18.pdf", 
       width = 14, height = 7, units = "cm", scale = 1)

ggsave(plot = my_map, filename = "Output/Global_map_draft_08Jan18.png", 
       width = 14, height = 7, units = "cm", scale = 1, dpi = 600)
