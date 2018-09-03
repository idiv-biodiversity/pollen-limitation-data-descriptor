# /////////////////////////////////////////////////////////////////////////
## Whittaker diagram for biomes and study locations.
# /////////////////////////////////////////////////////////////////////////

rm(list = ls(all.names = TRUE)); gc(reset = TRUE)

# install.packages("devtools")
# devtools::install_github("valentinitnelav/plotbiomes")
# `plotbiomes` package simulates the graph from Figure 5.5 in 
# Ricklefs, R. E. (2008), The economy of nature. W. H. Freeman and Company. 
# (Chapter 5, Biological Communities, The biome concept).
# Check details at https://github.com/valentinitnelav/plotbiomes
library(plotbiomes)
library(ggplot2)
library(data.table)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read & prepare data -----------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read temperature and precipitation data (extracted from CHELSA v1.2
# http://chelsa-climate.org/; see the script Extract_temp_rainfall.R)
my_dt <- fread("Output/cache/extractions_temp_pp.csv")

# Temperature needs to be divided by 10
my_dt[, Annual_mean_temp_C := Annual_mean_temp_C/10]
# Transform from mm to cm for precipitation
my_dt[, Annual_pp_cm := Annual_pp_mm/10]
# Delete column precipitation in mm
my_dt[, Annual_pp_mm := NULL]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot biomes -------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

biomes_plot <- ggplot() +
    # Add whittaker biomes layer
    geom_polygon(data = plotbiomes::Whittaker_biomes,
                 aes(x      = temp_c,
                     y      = precp_cm,
                     fill   = biome,
                     group  = biome_id),
                 # adjust polygon border
                 colour = "gray98",
                 size   = 0.5) +
    # fill the polygons with predefined colors
    scale_fill_manual(name   = "Biomes",
                      breaks = names(plotbiomes::Ricklefs_colors),
                      labels = names(plotbiomes::Ricklefs_colors),
                      values = plotbiomes::Ricklefs_colors) +
    # Add unique combinations of precipitation & temperature
    geom_point(data = my_dt, 
               aes(x = Annual_mean_temp_C, 
                   y = Annual_pp_cm),
               size   = 0.5,
               shape  = 1,
               colour = "black",
               alpha  = 1/2) + # set opacity level 
    # set range on OX axes and adjust the distance (gap) from OY axes
    scale_x_continuous(expand = c(0.02, 0)) +
    # set range on OY axes and adjust the distance (gap) from OX axes
    scale_y_continuous(limits = c(-5, round(max(my_dt$Annual_pp_cm, na.rm = TRUE))+10), 
                       expand = c(0, 0)) +
    # overwrite axis titles
    labs(x = "Mean annual temperature (°C)",
         y = "Mean annual precipitation (cm)") + 
    # set the general ggplot theme
    theme_bw() +
    # adjust legend position
    theme(
        panel.grid.major = element_line(size = 0.3, 
                                        linetype = "longdash"),
        panel.grid.minor = element_blank(), # eliminate minor grids,
        # set font family for all text within the plot ("sans" should work as "Arial");
        # note that this can be overridden with other adjustment functions below
        text = element_text(family = "sans", size = 8),
        # set the upper left corner of the legend box
        legend.justification = c(0, 1),  
        # adjust the position of the corner as relative to axis
        legend.position = c(0.01, 0.99), 
        # Set height of legend items (keys).
        legend.key.height = unit(3, "mm"),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        # set margins around entire plot ( https://goo.gl/zdgLMt )
        plot.margin = unit(c(t = 0.4, 
                             r = 1.1, 
                             b = 0, 
                             l = 0.1), 
                           "mm")
    )


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save as pdf and png file ------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggsave(biomes_plot,
       filename = "Output/Whittaker_diagram_biomes_draf_5.pdf", 
       width    = 9, 
       height   = 7, 
       units    = "cm")

ggsave(biomes_plot,
       filename = "Output/Whittaker_diagram_biomes_draf_5.png",
       width    = 9, 
       height   = 7, 
       units    = "cm",
       dpi      = 1000)
