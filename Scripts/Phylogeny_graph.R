# /////////////////////////////////////////////////////////////////////////
# Phylogenetic tree figure
# /////////////////////////////////////////////////////////////////////////

rm(list = ls(all.names = TRUE)); gc(reset = TRUE)

# Load packages

# For general data manipulation & visualization
library(data.table)
library(ggplot2)
library(glue)
library(lazyeval)
library(RColorBrewer)

# For data tree manipulation
library(picante)
library(ape)
library(pez)
library(geiger)

# For data tree visualization
# intsall ggtree from bioconductor
# source("https://bioconductor.org/biocLite.R")
# biocLite("ggtree")
# load/attach ggtree package
library(ggtree)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prepare phylogeny tree and annotation -----------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read phylogeny information to be used for annotating the tree plot
phylo_info <- fread("Data/phylogeny/Phylogeny_annotation.csv")

# Read the tree
tree_orig <- read.tree("Data/phylogeny/Aggre.tree_new.tre")
tree_orig$tip.label[1:5] # check formating of first 5 species names

# Note that "Physocarpus_amurensis" "Silene_stockenii" are extra in the tree 
# (they are not in the phylo_info)
my_sp <- 1:nrow(phylo_info)
names(my_sp) <- phylo_info$Species_accepted_names
geiger::name.check(phy = tree_orig, data = my_sp)
# Or check using setdiff
# setdiff(tree_orig$tip.label, phylo_info$Species_accepted_names)
# setdiff(phylo_info$Species_accepted_names, tree_orig$tip.label)

# Remove the extra taxa from the tree
tree <- ape::drop.tip(phy = tree_orig, 
                      tip = c("Physocarpus_amurensis",
                              "Silene_stockenii"))

# Save the updated tree
# write.tree(tree, file = "Output/SiteTree_VS.tree")
# write.nexus(tree, file = "Output/SiteTree_VS_nexus.nex")

# Merge tree tip labels with annotation data from phylo_info;
# "tip.label" column name needs to be used exactly as such (with this name).
# This will be needed for merging annotation data with the tree (with  %<+%
# operator) using tip.label as key.
tip_lbs <- data.table(tip.label = tree$tip.label)
phylo_info_merged <- merge(x = tip_lbs, 
                           y = phylo_info, 
                           by.x  = "tip.label", 
                           by.y  = "Species_accepted_names", 
                           all.x = TRUE, 
                           sort  = FALSE)

# Rename "Magnoliids" to "Magnoliid" in the annotation table
phylo_info_merged[APG4_group == "Magnoliids", APG4_group := "Magnoliid"]

# Split by APG4_group data - for coloring purposes
# see https://bioconductor.org/packages/devel/bioc/vignettes/ggtree/inst/doc/treeAnnotation.html
# or http://www.ggplot2-exts.org/ggtree.html
APG4_gr <- split(phylo_info_merged$tip.label, phylo_info_merged$APG4_group)
names(APG4_gr)
SiteTree_gr <- ggtree::groupOTU(tree, APG4_gr)
str(SiteTree_gr)
# There is a "0" group label - "0 is for those which didn't belong to any group"
# see https://github.com/GuangchuangYu/ggtree/issues/127
levels(attributes(SiteTree_gr)$group) # check all group labels
# Overwrite "0" with "Basal" so that the basal segments get the same color.
# similar to: 
# https://en.wikipedia.org/wiki/Phylogenetic_tree#/media/File:CollapsedtreeLabels-simplified.svg
levels(attributes(SiteTree_gr)$group)[1] <- "Basal"
# Reorder factor levels if needed
attributes(SiteTree_gr)$group <- factor(x = attributes(SiteTree_gr)$group, 
                                        levels = c("Basal", 
                                                   "Eudicot", 
                                                   "Magnoliid", 
                                                   "Monocot"),
                                        ordered = TRUE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build tree base-plot ----------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Explore with color - for branches
my_cols <- brewer.pal(n = 4, name = "Set1")
names(my_cols) <- levels(attributes(SiteTree_gr)$group)
scales::show_col(my_cols); my_cols
my_cols[1] <- "#000000" # assign black to basal 
scales::show_col(my_cols); my_cols


tree_pl <- 
    ggtree(tr      = SiteTree_gr, 
           mapping = aes(color = group), 
           layout  = 'circular',
           # set line thikness
           size = 0.15) +
    # adjust coloring of main groups
    scale_color_manual(name = 'Clade',
                       values = my_cols)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Construct coordinates for segment annotations ---------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Attaches annotation data to the tree view. Doing so, all variables in the
# phylo_info_merged are visible to ggtree.
tree_pl <- tree_pl %<+% phylo_info_merged 

# Get data from tree view
tree_dt <- data.table(tree_pl$data)
# select only the tips and order by coord y
tree_dt <- tree_dt[isTip == TRUE][order(y)]

# Make a table with y coordinates for each group; this helps for drawing and
# labeling segments. Using the "rleid" function (a grouping ID generator)
# because consecutive rows of an identical reoccurring group must form a unique
# group. When grouping, take first and last y coordinates of a group, also
# compute an average angle and count number of records in a group.
coord_groups <- tree_dt[, .(y1 = y[1],
                            y2 = y[.N],
                            angle = mean(angle),
                            n = .N), # optional - helps with counting
                        # group by:
                        by = .(order_group, 
                               id_gr = rleid(order_group, 
                                             prefix = "grp"))]
# Compute the middle y coordinate – will be used for placing the group label.
# Similarly, the mean angle was computed above already. 
coord_groups[, y_mid := rowMeans(.SD), .SDcols = c("y1", "y2")]
# For groups where y1=y2, adjust their y coordinates so that a segment gets
# drawn; if not, no segment gets drawn for such cases.
coord_groups[, y1_adj := ifelse(y1 == y2, y1 - 0.25, y1)]
coord_groups[, y2_adj := ifelse(y1 == y2, y2 + 0.25, y2)]

# Labels need angle adjustment for cases between 90 and 270 dg
coord_groups[, angle_adj := angle]
coord_groups[angle %between% c(90, 180), angle_adj := angle_adj + 180]
coord_groups[angle %between% c(180, 270), angle_adj := angle_adj - 180]

# Labels with angles between 90 and 270 dg need the horizontal adjustment
# argument changed from 0 to 1.
coord_groups[, hjust_adj := ifelse(angle %between% c(90, 270), yes = 1, no = 0)]

# if needed, coloring of segments could be binary 
# coord_groups[, col := ifelse(.I%%2, 0.5, 1)]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prepare ES values for creating the circular barplot effect --------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read PL data (the output of the Prepare_data.R script). Select only the needed
# columns.
ES_all_dt <- fread("Output/GloPL_with_id_updated_ES.csv",
                   select = c("Species_accepted_names", 
                              "PL_Effect_Size"))

# Compare species in data and tree.
my_sp_ES <- 1:length(unique(ES_all_dt$Species_accepted_names))
names(my_sp_ES) <- unique(ES_all_dt$Species_accepted_names)
geiger::name.check(phy = SiteTree_gr, data = my_sp_ES)
# OK = the tree tips and the es table match in regards to species names.

# Compute average ES per species
ES_dt <- ES_all_dt[, .(PL_Effect_Size = mean(as.numeric(PL_Effect_Size), 
                                             na.rm = TRUE)), 
                   by = Species_accepted_names]

# Attach mean ES values to each species
tree_dt <- merge(x = tree_dt,
                 y = ES_dt,
                 by.x = "label",
                 by.y = "Species_accepted_names")
tree_dt[, ES_categ := ifelse(PL_Effect_Size <= 0, "neg", "pos")]

# Define variable to control the x coordinates of bars (segments).
# A multiplicative factor to ES values so that differences get enhanced.
my_factor <- 10 
x_base <-
    max(tree_dt$x) +
    abs(min(tree_dt$PL_Effect_Size, na.rm = TRUE)) *
    my_factor + 2

# Define variable to control the x coordinates of segments and labels.
my_x <-
    x_base +
    max(tree_dt$PL_Effect_Size, na.rm = TRUE) *
    my_factor + 5


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build the tree plot -----------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tree_ES_bars <- 
    tree_pl + 
    # Add a background disc to plot ES bars on top of it
    geom_rect(data = tree_dt,
              aes(xmin = x_base + min(PL_Effect_Size, na.rm = TRUE) * my_factor,
                  ymin = 0,
                  xmax = x_base + max(PL_Effect_Size, na.rm = TRUE) * my_factor,
                  ymax = max(y) + 1), 
              # +1 to force complete circle, otherwise a stripe of white remains.
              color = NA, # set NA so to avoid coloring borders
              fill = "#deebf7", # or try "#8da0cb"
              alpha = 0.1) +
    # Add ES bars
    geom_rect(data = tree_dt,
              aes(xmin = x_base,
                  ymin = y - 0.53,
                  xmax = x_base + PL_Effect_Size*my_factor,
                  ymax = y + 0.53,
                  fill = ES_categ),
              # no borders
              color = NA) +
    # Fill the ES bars
    scale_fill_manual(name   = 'Effect size',
                      breaks = c("neg", "pos"),
                      values = c("neg" = "#66c2a5",
                                 "pos" = "#fc8d62"),
                      labels = c("negative", "positive")) +
    # Add line segments for each group.
    geom_segment(data = coord_groups,
                 aes(x = my_x, 
                     y = y1_adj, 
                     xend = my_x, 
                     yend = y2_adj),
                 color = "black",
                 lineend = "butt",
                 size = 1,
                 show.legend = FALSE) +
    # Add text group labels at the middle of each segment.
    geom_text(data = coord_groups,
              aes(x = my_x,
                  y = y_mid,
                  angle = angle_adj,
                  hjust = hjust_adj,
                  label = order_group),
              vjust = 0.5, 
              size  = 1.5,
              nudge_x = 7, # Offsetting label from its default x coordinate.
              color = "black",
              show.legend = FALSE) +
    # Change the order of color and fill guides (the order of the legend panels)
    guides(color = guide_legend(order = 1),
           fill = guide_legend(order = 2)) +
    # Adjust theme components
    theme(
        # Set font size & family - affects legend only 
        # "sans" = "Arial" and is the default on Windows OS; check windowsFonts()
        text = element_text(size = 8, family = "sans"),
        # Grab bottom-right (x=1, y=0) legend corner 
        legend.justification = c(1,0),
        # Position the legend in the bottom-right plot area.
        legend.position = c(1.2, 0.04),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        # Set height of legend items (keys).
        legend.key.height = unit(3, "mm"),
        # Set margin around entire plot.
        plot.margin = unit(c(t = -0.5, r = 1.7, b = -0.35, l = -0.05), "cm")
    )


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save as pdf and png file ------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggsave(plot = tree_ES_bars,
       filename = "Output/Phylo_tree_ES_bars_draft_16.png", 
       width = 10, 
       height = 8, 
       units = "cm", 
       dpi = 1000)

ggsave(plot = tree_ES_bars,
       filename = "Output/Phylo_tree_ES_bars_draft_16.pdf", 
       width = 10, 
       height = 8, 
       units = "cm")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# References --------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ggtree:
#   https://bioconductor.org/packages/release/bioc/html/ggtree.html
#   https://guangchuangyu.github.io/ggtree/
#   https://github.com/GuangchuangYu/ggtree
# tidytree:
#   https://cran.r-project.org/web/packages/tidytree/vignettes/tidytree.html
#   https://cran.r-project.org/web/packages/tidytree/index.html
# HowTo/DataTreeManipulation
#   https://www.r-phylo.org/wiki/HowTo/DataTreeManipulation
