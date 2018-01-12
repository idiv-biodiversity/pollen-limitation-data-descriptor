####################################################################################
## Whittaker diagram for biomes and study locations
## I used the biome polygons Fig 5.5 p.92 in Ricklefs, 
## "The Economy of Nature, Chapter 5, Biological Communities, The biome concept."
####################################################################################

# install.packages("devtools")
# devtools::install_github("valentinitnelav/plotbiomes")
# Check details at https://github.com/valentinitnelav/plotbiomes
library(plotbiomes)
# `plotbiomes` package simulates the graph from Figure 5.5 in 
# Ricklefs, R. E. (2008), The economy of nature. W. H. Freeman and Company. 
# (Chapter 5, Biological Communities, The biome concept).
library(data.table)

# =================================================================================
# Read & prepare data
# =================================================================================
PL_agg_all <- fread("Data/PL_ANALYSIS_02_10_2017_MasterES_aggreg_pop_Extractions.csv")
# Get only columns of interest
PL_agg <- PL_agg_all[,.(unique_number, Annual_mean_temp_C, Annual_pp_mm)]
str(PL_agg)
PL_agg[, Annual_mean_temp_C := as.numeric(Annual_mean_temp_C)]
PL_agg[, Annual_pp_mm := as.numeric(Annual_pp_mm)]
# transform from mm to cm for precipitation
PL_agg[, Annual_pp_cm := Annual_pp_mm/10]
# delete column precipitation in mm
PL_agg[, Annual_pp_mm := NULL]

# Can tru using unique combinations of precipitation & temperature
# PL_agg <- unique(PL_agg, by = c("Annual_mean_temp_C", "Annual_pp_cm"))

# =================================================================================
# Plot biomes
# =================================================================================

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
    geom_point(data = PL_agg, 
               aes(x = Annual_mean_temp_C, 
                   y = Annual_pp_cm,
                   # text will be ignored in ggplot but will be used for hovering purposes in plotly
                   # this makes point identification interactive
                   text = paste0('unique_number: ', unique_number)),
               position = position_jitter(width = 0.15, height = 0),
               size   = 0.4,
               shape  = 1,
               colour = "dodgerblue4",
               alpha  = 0.6) + # set opacity level 
    # set range on OX axes and adjust the distance (gap) from OY axes
    scale_x_continuous(expand = c(0.02, 0)) +
    # set range on OY axes and adjust the distance (gap) from OX axes
    scale_y_continuous(limits = c(-5, round(max(PL_agg$Annual_pp_cm, na.rm = TRUE))+10), 
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
        # set font family for all text within the plot ("sans" should work as "Arial")
        # note that this can be overridden with other adjustment functions below
        text = element_text(family = "sans", size = 8),
        legend.justification = c(0, 1),  # set the upper left corner of the legend box
        legend.position = c(0.01, 0.99), # adjust the position of the corner as relative to axis
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

set.seed(1) # for jittering each time in the same way
biomes_plot

# =================================================================================
# Save to pdf and png file
# =================================================================================
set.seed(1) # for jittering each time in the same way
ggsave(biomes_plot,
       filename = file.path("Output", "Whittaker_diagram_biomes_draf_1.pdf"), 
       width    = 9, 
       height   = 7, 
       units    = "cm")

set.seed(1) # for jittering each time in the same way
ggsave(biomes_plot,
       filename = file.path("Output", "Whittaker_diagram_biomes_draf_1.png"),
       width    = 9, 
       height   = 7, 
       units    = "cm",
       dpi      = 1000)