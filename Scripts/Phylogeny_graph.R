###############################################################################
## Phylogeny tree graph for pollen limitation dataset
###############################################################################

# Load packages

# For general data manipulation & visualization
library(readxl)
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
# Installing from GitHub does not work at this time (Dec. 2017).
# load/attach ggtree package
library(ggtree)
# more about ggtree at: 
# https://bioconductor.org/packages/release/bioc/html/ggtree.html

# For validating species names 
library(taxize)
library(Taxonstand)
library(wikitaxa)

# =============================================================================
# Read & prepare data
# =============================================================================
# Read species names and their corresponding family (file from Tiffany).
# This file might be optional.
data_aggregated <- data.table(readxl::read_excel(path = "Data/phylogeny/specieslist.xlsx", 
                                                 sheet     = 1, 
                                                 col_names = TRUE))
# Get only unique records by species names
dataspecies <- unique(data_aggregated, by = "Species_accepted_names")

# Read phylogeny information to be used for labeling the plot
# Phylogeny information_VS.xlsx contains some corrections as oposed to Phylogeny information.xlsx
Phylo_info <- data.table(readxl::read_excel(path = "Data/phylogeny/Phylogeny information_VS.xlsx", 
                                            sheet     = 1, 
                                            col_names = TRUE))
# Defensively replace spaces with "_" in column names
data.table::setnames(Phylo_info, gsub("\\s+", "_", names(Phylo_info)))

# Check if any differences by species names between the 2 files
# This is important because the key to merge the files is the species names
all.equal(sort(Phylo_info$Species_accepted_names, na.last = TRUE), 
          sort(dataspecies$Species_accepted_names, na.last = TRUE))

# Merge the two files
Phylo_info <- merge(x = Phylo_info,
                    y = dataspecies[,.(Species_accepted_names, Family)],
                    by = "Species_accepted_names",
                    all.x = TRUE)

# -----------------------------------------------------------------------------
# Check some taxa names
# -----------------------------------------------------------------------------
my_sp <- fread("Output/taxa_to_check_VS.csv")

my_sp <- merge(x = my_sp,
               y = Phylo_info,
               by.x = "taxa",
               by.y = "Species_accepted_names",
               all.x = TRUE, sort = FALSE)

# replace _ with white space
my_sp[, taxa := gsub(pattern = "_", replacement = " ", x = taxa)]

# gives errors
# taxize::classification(x = my_sp$taxa, db = "tol")

# Connects to The Plant List (TPL) website and validates the names
my_tpl <- data.table(Taxonstand::TPL(splist = my_sp$taxa))
setnames(my_tpl, paste0(names(my_tpl), "_TPL"))
my_tpl

my_sp <- merge(x = my_sp,
               y = my_tpl[, c("Taxon_TPL", "Taxonomic.status_TPL", "Family_TPL", "New.Genus_TPL",
                              "New.Species_TPL", "New.Taxonomic.status_TPL", "Typo_TPL")],
               by.x = "taxa",
               by.y = "Taxon_TPL",
               all.x = TRUE, sort = FALSE)

my_families <- unique(my_sp$Family_TPL)
my_orders <- vector(mode = "character", length = length(my_families))

# Retrieve info from WikiSpecies about orders based on given families.
# Had to use WikiSpeceis because TPL does not return order.
# Try running several times if NA-s are returned - I guess it depends on the API connection somehow.
for (i in 1:length(my_families)){
    my_dt <- data.table(wikitaxa::wt_wikispecies(name = my_families[i])$classification)
    # some cases might not match properly and does not retrieve order information,
    # so, assign NA for such cases
    my_ord <- my_dt[rank == "Ordo", name]
    my_orders[i] <- ifelse(length(my_ord) != 0, my_ord, NA)
}

my_orders_df <- data.frame(my_families, order_wikitaxa = my_orders)

my_sp <- merge(x = my_sp,
               y = my_orders_df,
               by.x = "Family_TPL",
               by.y = "my_families",
               all.x = TRUE, sort = FALSE)

# move "Family_TPL" column from first to last position
setcolorder(my_sp, c(setdiff(names(my_sp), "Family_TPL"), "Family_TPL"))

writexl::write_xlsx(my_sp, path = "Output/taxa_to_check_TPL_suggest_VS.xlsx")


# -----------------------------------------------------------------------------
# Prepare phylogeny tree for plotting
# -----------------------------------------------------------------------------
# Read tree from Tiffany
tree <- read.tree("Data/phylogeny/Aggre.tree_new.tre")
tree$tip.label[1:5] # check formating of first 5 species names

# Note that "Physocarpus_amurensis" "Silene_stockenii" are extra in the tree 
# as opposed to file Phylogeny information.xlsx
my_sp <- 1:nrow(Phylo_info)
names(my_sp) <- Phylo_info$Species_accepted_names
geiger::name.check(phy = tree, data = my_sp)
# Or check using setdiff
# setdiff(SiteTree$tip.label, Phylo_info$Species_accepted_names)
# setdiff(Phylo_info$Species_accepted_names, SiteTree$tip.label)

# Remove extra taxa from phylogeny
SiteTree <- ape::drop.tip(phy = tree, 
                          tip = c("Physocarpus_amurensis",
                                  "Silene_stockenii"))
# (I think) since there are no species that need to be added to the tree, 
# then no need of using pez::congeneric.merge(); If needed, fallow:
# https://gist.github.com/valentinitnelav/4cfe294b27c66276f28b2377a158813c#file-congeneric-merge_example-r

# Save updated tree
write.tree(SiteTree, file = "Output/SiteTree_VS.tree")
write.nexus(SiteTree, file = "Output/SiteTree_VS_nexus")

# Merge tree tip labels with annotation data from Phylo_info;
# "tip.label" column name needs to be used exactly as such (with this name).
# This will be needed for merging annotation data with the tree (with  %<+% operator) using tip.label as key.
tip_lbs <- data.table(tip.label = SiteTree$tip.label)
Phylo_info_merged <- merge(x = tip_lbs, 
                           y = Phylo_info, 
                           by.x  = "tip.label", 
                           by.y  = "Species_accepted_names", 
                           all.x = TRUE, 
                           sort  = FALSE)

# Split by APG4_group data - for coloring purposes
# see https://bioconductor.org/packages/devel/bioc/vignettes/ggtree/inst/doc/treeAnnotation.html
# or http://www.ggplot2-exts.org/ggtree.html
APG4_gr <- split(Phylo_info_merged$tip.label, Phylo_info_merged$APG4_group)
names(APG4_gr)
SiteTree_gr <- ggtree::groupOTU(SiteTree, APG4_gr)
str(SiteTree_gr)
# There is a "0" group label - "0 is for those didn't belong to any group."
# https://github.com/GuangchuangYu/ggtree/issues/127
levels(attributes(SiteTree_gr)$group) # check all group labels
# Overwrite "0" with "Basal" so that the basal segments get the same color.
# similar to: https://en.wikipedia.org/wiki/Phylogenetic_tree#/media/File:CollapsedtreeLabels-simplified.svg
levels(attributes(SiteTree_gr)$group)[1] <- "Basal"
# Reorder factor levels if needed
attributes(SiteTree_gr)$group <- factor(x = attributes(SiteTree_gr)$group, 
                                        levels = c("Basal", 
                                                   "Eudicot", 
                                                   "Magnoliids", 
                                                   "Monocot"))

# =============================================================================
# Plot tree with ES bars
# =============================================================================

# -----------------------------------------------------------------------------
# Experiment with color
# -----------------------------------------------------------------------------
my_cols <- brewer.pal(n = 4, name = "Set1")
names(my_cols) <- levels(attributes(SiteTree_gr)$group)
scales::show_col(my_cols); my_cols
my_cols[1] <- "#000000" # assign black to basal 
scales::show_col(my_cols); my_cols

# -----------------------------------------------------------------------------
# Build base plot
# -----------------------------------------------------------------------------
tree_pl <- 
    ggtree(tr      = SiteTree_gr, 
           mapping = aes(color = group), 
           layout  = 'circular',
           # set line thikness
           size = 0.15) +
    # adjust coloring of main groups
    scale_color_manual(name = 'Clade',
                       values = my_cols)

# -----------------------------------------------------------------------------
# Construct table of coordinates for segment annotations
# -----------------------------------------------------------------------------
# Attaches annotation data to a tree view.
# Doing so, all variables in the Phylo_info_merged are visible to ggtree
tree_pl <- tree_pl %<+% Phylo_info_merged 

# Get data from tree view
tree_dt <- data.table(tree_pl$data)
# select only the tips and order by coord y
tree_dt <- tree_dt[isTip == TRUE][order(y)]

# Make table with y cords for each group; this helps for drawing & labeling segments.
# Note the usage of "rleid" function, which is a grouping ID generator,
# needed because consecutive rows of an identical reoccurring group must form a unique group.
coord_groups <- tree_dt[, .(y1 = y[1],
                            y2 = y[.N],
                            angle = mean(angle),
                            n = .N), # optional - helps with counting
                        by = .(order_group, 
                               id_gr = rleid(order_group, 
                                             prefix = "grp"))]
# Compute the middle y – will be used for placing the group label;
# similarly the mean angle was computed above already. 
coord_groups[, y_mid := rowMeans(.SD), .SDcols = c("y1", "y2")]
# For groups that y1=y2, adjust their y coordinates so that a segment gets drawn;
# if not, no segment get drawn for such cases.
coord_groups[, y1_adj := ifelse(y1 == y2, y1 - 0.25, y1)]
coord_groups[, y2_adj := ifelse(y1 == y2, y2 + 0.25, y2)]

# Labels need angle adjustment for cases between 90 and 270 dg
coord_groups[, angle_adj := angle]
coord_groups[angle %between% c(90, 180), angle_adj := angle_adj + 180]
coord_groups[angle %between% c(180, 270), angle_adj := angle_adj - 180]

# Labels with angles between 90 and 270 dg
# need change of horizontal adjustment argument from 0 to 1.
coord_groups[, hjust_adj := ifelse(angle %between% c(90, 270), yes = 1, no = 0)]

# if needed, coloring of segments could be binary 
# coord_groups[, col := ifelse(.I%%2, 0.5, 1)]

# -----------------------------------------------------------------------------
# Prepare ES values for creating the circular barplot effect
# -----------------------------------------------------------------------------
# Read PL table with ES (effect size) data
ES_all_dt <- fread("Output/PL_masters_for_publication_with_ES_cols.csv",
                   select = c("Species_accepted_names", "ES_mst.VS"))

# Compares species in data and tree.
# If “OK” then the tree tips and data match in regards to species names.
my_sp_ES <- 1:length(unique(ES_all_dt$Species_accepted_names))
names(my_sp_ES) <- unique(ES_all_dt$Species_accepted_names)
geiger::name.check(phy = SiteTree_gr, data = my_sp_ES)

# Compute average ES per species
ES_dt <- ES_all_dt[, .(ES_mst.VS = mean(ES_mst.VS, na.rm = TRUE)), by = Species_accepted_names]

# Join mean ES values to each species
tree_dt <- merge(x = tree_dt,
                 y = ES_dt,
                 by.x = "label",
                 by.y = "Species_accepted_names")
tree_dt[, ES_categ := ifelse(ES_mst.VS <= 0, "neg", "pos")]

# Define variable to control the x coordinates of bars (segments)
my_factor <- 10 # multiplicative factor to ES values so that differences get enhanced
x_base <- max(tree_dt$x) + abs(min(tree_dt$ES_mst.VS, na.rm = TRUE))*my_factor + 2

# Define variable to control the x coordinates of segments & labels
my_x <- x_base + max(tree_dt$ES_mst.VS, na.rm = TRUE)*my_factor + 5

# Plot
tree_ES_bars <- 
    tree_pl + 
    # Add a background disc to plot ES bars on top of it
    geom_rect(data = tree_dt,
              aes(xmin = x_base + min(ES_mst.VS, na.rm = TRUE)*my_factor,
                  ymin = 0,
                  xmax = x_base + max(ES_mst.VS, na.rm = TRUE)*my_factor,
                  ymax = max(y)+1), # +1 to force complete circle (otherwise a stripe of white remains)
              color = NA, # set NA so to avoid coloring borders
              fill = "#deebf7", # or try "#8da0cb"
              alpha = 0.1) +
    # Add ES bars
    geom_rect(data = tree_dt,
              aes(xmin = x_base,
                  ymin = y - 0.53,
                  xmax = x_base + ES_mst.VS*my_factor,
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
    # Adjust theme components
    theme(
        # Set font size & family - affects legend only 
        # "sans" = "Arial" and is the default on Windows OS; check windowsFonts()
        text = element_text(size = 8, family = "sans"),
        # Grab bottom-right (x=1, y=0) legend corner 
        legend.justification = c(1,0),
        # and position it in the bottom-right plot area.
        legend.position = c(1.2, 0.04),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        # legend.key = element_rect(size = 1, color = 'white'),
        # Set height of legend items (keys).
        legend.key.height = unit(3, "mm"),
        # Set margin around entire plot.
        plot.margin = unit(c(t = -0.5, r = 1.7, b = -0.35, l = -0.05), "cm")
    )

ggsave(plot = tree_ES_bars,
       filename = "Output/Phylo_tree_ES_bars_draft10.png", 
       width = 10, height = 8, units = "cm", dpi = 1000)

ggsave(plot = tree_ES_bars,
       filename = "Output/Phylo_tree_ES_bars_draft10.pdf", 
       width = 10, height = 8, units = "cm")

# Warning message:
# Removed 22 rows containing missing values (geom_rect).

# =============================================================================
# References
# =============================================================================
# ggtree:
#   https://bioconductor.org/packages/release/bioc/html/ggtree.html
#   https://guangchuangyu.github.io/ggtree/
#   https://github.com/GuangchuangYu/ggtree
# tidytree:
#   https://cran.r-project.org/web/packages/tidytree/vignettes/tidytree.html
#   https://cran.r-project.org/web/packages/tidytree/index.html
# HowTo/DataTreeManipulation
#   https://www.r-phylo.org/wiki/HowTo/DataTreeManipulation