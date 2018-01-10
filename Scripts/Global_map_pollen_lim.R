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
# Read latest aggregated dataset with master ES values
ES_all_dt <- fread("Data/MasterES_aggreg_pop.csv")
# Get only columns of interest
ES_dt <- ES_all_dt[!is.na(Species_accepted_names), .(unique_number,
                                                     lon_decimal_PTL_JMB, 
                                                     lat_decimal_PTL_JMB, 
                                                     Species_accepted_names, 
                                                     ES_mst.VS)]
str(ES_dt)
ES_dt[, lon_decimal_PTL_JMB := as.numeric(lon_decimal_PTL_JMB)]
ES_dt[, lat_decimal_PTL_JMB := as.numeric(lat_decimal_PTL_JMB)]
# Some routine checking of coordinates values
range(ES_dt[,lon_decimal_PTL_JMB]) %between% c(-180, 180)
range(ES_dt[,lat_decimal_PTL_JMB]) %between% c(-90, 90)

# Create ES categories
ES_dt[, ES_categ := ifelse(ES_mst.VS <= 0, "neg", "pos")]

# Get unique pairs of coordinates
ES_dt_unq <- unique(ES_dt, by = c("lon_decimal_PTL_JMB", "lat_decimal_PTL_JMB"))

# Transform unprojected long-lat in Robinson coordinates
ES_dt_unq[, c("X.prj","Y.prj") := data.table(rgdal::project(xy   = cbind(lon_decimal_PTL_JMB,
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
# Plot map
# =============================================================================

# -----------------------------------------------------------------------------
# Prepare simple map
# -----------------------------------------------------------------------------
my_base_map <- 
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
                 fill   ="transparent", # try also "lightblue" but add a separate polygon as first layer
                 size   = 0.2) +
    # "Regions defined for each Polygons" warning has to do with fortify transformation. Might get deprecated in future!
    # ___ the default ratio = 1 in coord_fixed ensures that one unit on the x-axis is the same length as one unit on the y-axis
    coord_fixed(ratio = 1) +
    # Remove the background and default gridlines with theme_nothing() from the ggmap package
    # ___ remove the background and default gridlines
    theme_void()

# -----------------------------------------------------------------------------
# Color by master ES category
# -----------------------------------------------------------------------------
my_map_ES_categ <- my_base_map +
    # ___ add the XY points
    geom_point(data = ES_dt_unq, 
               aes(x = X.prj, 
                   y = Y.prj,
                   color = ES_categ), 
               size   = 0.5,
               alpha  = 0.7) +
    # ___ adjust color
    scale_color_manual(name   = 'Effect size',
                       breaks = c("neg", "pos"),
                       values = c("neg" = "#66c2a5",
                                  "pos" = "#fc8d62"),
                       labels = c("negative", "positive")) +
    # Adjust theme components
    theme(
        # Set font size & family - affects legend only 
        # "sans" = "Arial" and is the default on Windows OS; check windowsFonts()
        text = element_text(size = 8, family = "sans"),
        # Grab bottom-right (x=1, y=0) legend corner 
        legend.justification = c(1, 0),
        # and position it in the bottom-right plot area.
        legend.position = c(1.05, 0.05),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        # Set height of legend items (keys).
        legend.key.height = unit(3, "mm"),
        # Set margin around entire plot.
        plot.margin = unit(c(t = -0.35, r = 1, b = -0.4, l = -0.5), "cm")
    )

ggsave(plot = my_map_ES_categ, filename = "Output/Global_map_ES_categ_draft_5.pdf", 
       width = 14, height = 7, units = "cm")

# -----------------------------------------------------------------------------
# Try ES colour gradient
# -----------------------------------------------------------------------------
my_map_ES_gradient <- my_base_map +
    # ___ add the XY points
    geom_point(data = ES_dt_unq, 
               aes(x = X.prj, 
                   y = Y.prj,
                   color = ES_mst.VS), 
               size   = 0.5,
               alpha  = 0.7) +
    # ___ adjust color
    scale_colour_gradient2(name   = 'Effect size',
                           low  = "#66c2a5",
                           mid  = "#CCCC00",
                           high = "#fc8d62",
                           midpoint = 0,
                           limits   = range(ES_dt_unq$ES_mst.VS)) +
    # Adjust theme components
    theme(
        # Set font size & family - affects legend only 
        # "sans" = "Arial" and is the default on Windows OS; check windowsFonts()
        text = element_text(size = 8, family = "sans"),
        # Grab bottom-right (x=1, y=0) legend corner 
        legend.justification = c(1, 0),
        # and position it in the bottom-right plot area.
        legend.position = c(1.05, 0.05),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        # Set height of legend items (keys).
        legend.key.height = unit(3, "mm"),
        # Set margin around entire plot.
        plot.margin = unit(c(t = -0.35, r = 1, b = -0.4, l = -0.5), "cm")
    )

ggsave(plot = my_map_ES_gradient, filename = "Output/Global_map_ES_gradient_draft_5.pdf", 
       width = 14, height = 7, units = "cm")

# -----------------------------------------------------------------------------
# Try ES colour classes
# -----------------------------------------------------------------------------
hist(ES_dt_unq$ES_mst.VS)
quantile(ES_dt_unq$ES_mst.VS)

ES_dt_unq[, ES_cls := cut(x = ES_mst.VS, 
                          breaks = c(min(ES_mst.VS)-1/10^6, 0, quantile(ES_mst.VS)[2:5]),
                          dig.lab = 1)]
str(ES_dt_unq)

# Experiment with color
library(RColorBrewer)
my_cols <- rev(brewer.pal(n = 5, name = "Spectral"))
names(my_cols) <- levels(ES_dt_unq$ES_cls)
scales::show_col(my_cols); my_cols

# my_cols <- grDevices::colorRampPalette(colors = c("#66c2a5", "#fc8d62"))(5)
# names(my_cols) <- levels(ES_dt_unq$ES_cls)
# scales::show_col(my_cols); my_cols

my_map_ES_cls <- my_base_map +
    # ___ add the XY points
    geom_point(data = ES_dt_unq, 
               aes(x = X.prj, 
                   y = Y.prj,
                   color = ES_cls), 
               size   = 0.5,
               alpha  = 0.7) +
    # ___ adjust color
    scale_color_manual(name = 'Effect size',
                       values = my_cols) +
    # Adjust theme components
    theme(
        # Set font size & family - affects legend only 
        # "sans" = "Arial" and is the default on Windows OS; check windowsFonts()
        text = element_text(size = 8, family = "sans"),
        # Grab bottom-right (x=1, y=0) legend corner 
        legend.justification = c(1, 0),
        # and position it in the bottom-right plot area.
        legend.position = c(1.05, 0.05),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        # Set height of legend items (keys).
        legend.key.height = unit(3, "mm"),
        # Set margin around entire plot.
        plot.margin = unit(c(t = -0.35, r = 1, b = -0.4, l = -0.5), "cm")
    )

ggsave(plot = my_map_ES_cls, filename = "Output/Global_map_ES_cls_draft_6.pdf", 
       width = 14, height = 7, units = "cm")
