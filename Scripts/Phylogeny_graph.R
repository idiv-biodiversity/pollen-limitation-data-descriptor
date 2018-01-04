###############################################################################
## Phylogeny tree graph for pollen limitation dataset
###############################################################################

library(readxl)
library(data.table)
library(ggplot2)
library(glue)
library(lazyeval)
library(RColorBrewer)

library(picante)
library(ape)
library(pez)

# for validating species names 
library(taxize)
library(Taxonstand)
library(wikitaxa)

# Intsall ggtree from bioconductor
# source("https://bioconductor.org/biocLite.R")
# biocLite("ggtree")
# Installing from GitHub does not work at this time (Dec. 2017).

# load/attach ggtree package
library(ggtree)
# more about ggtree at: 
# https://bioconductor.org/packages/release/bioc/html/ggtree.html

# =============================================================================
# Read & prepare data
# =============================================================================
# Read species names and their corresponding family (file from Tiffany)
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
# if not TRUE then check differences
setdiff(dataspecies$Species_accepted_names, Phylo_info$Species_accepted_names)
setdiff(Phylo_info$Species_accepted_names, dataspecies$Species_accepted_names)
# "Physocarpus_amurensis" "Silene_stockenii" where added in Phylogeny information_VS.xlsx
# as they were present in the tree object already.
# Need checking Phylogeny information_VS.xlsx if I added correct info for these species!
# for now, these two are missing family info

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
# Build phylogeny
# -----------------------------------------------------------------------------
# Read tree from Tiffany
tree <- read.tree("Data/phylogeny/Aggre.tree.tre")
tree$tip.label[1:5] # check formating of first 5 species names
# tree <- read.tree("Data/phylogeny/phylo1265species.tre")
SiteTree <- pez::congeneric.merge(tree    = tree, 
                                  species = Phylo_info$Species_accepted_names, 
                                  split   = "_") # space or "_" doesn't seem to make a difference
all.equal(tree, SiteTree)
identical(tree, SiteTree)
# Why need congeneric.merge if the two trees are identical?

# Note that "Physocarpus_amurensis" "Silene_stockenii" were extra in the tree 
# when using Phylogeny information.xlsx instead of Phylogeny information_VS.xlsx
setdiff(SiteTree$tip.label, Phylo_info$Species_accepted_names)
setdiff(Phylo_info$Species_accepted_names, SiteTree$tip.label)

# Merge tree tip labels with data from Phylo_info;
# tip.label needs to be used exactly as such (with this name).
# This is needed for merging data with the tree based on tip labels with  %<+% operator
tip_lbs <- data.table(tip.label = SiteTree$tip.label)
# Note that Phylo_info_merged may contain extra 2 rows if Phylogeny information.xlsx was used
# (the 2 extra species discussed above)
Phylo_info_merged <- merge(x = tip_lbs, 
                           y = Phylo_info, 
                           by.x  = "tip.label", 
                           by.y  = "Species_accepted_names", 
                           all.x = TRUE, 
                           sort  = FALSE)

# Phylo_info_merged[is.na(order_group), order_group := "Other"]

# Split by APG4_group data - for coloring purposes
# see https://bioconductor.org/packages/devel/bioc/vignettes/ggtree/inst/doc/treeAnnotation.html
# or http://www.ggplot2-exts.org/ggtree.html
length(Phylo_info_merged$tip.label)
length(Phylo_info_merged$APG4_group)
APG4_gr <- split(Phylo_info_merged$tip.label, Phylo_info_merged$APG4_group)
names(APG4_gr)
length(APG4_gr)
SiteTree_gr <- ggtree::groupOTU(SiteTree, APG4_gr)
str(SiteTree_gr)
# There is a "0" group label - "0 is for those didn't belong to any group."
# https://github.com/GuangchuangYu/ggtree/issues/127
levels(attributes(SiteTree_gr)$group) # check all group labels
# overwrite "0" with "Basal" so that the basal segments get the same color.
# similar to: https://en.wikipedia.org/wiki/Phylogenetic_tree#/media/File:CollapsedtreeLabels-simplified.svg
levels(attributes(SiteTree_gr)$group)[1] <- "Basal"
# reorder factor levels if needed
attributes(SiteTree_gr)$group <- factor(x = attributes(SiteTree_gr)$group, 
                                        levels = c("Basal", 
                                                   "Eudicot", 
                                                   "Magnoliids", 
                                                   "Monocot"))

# =============================================================================
# Plot tree
# =============================================================================

# Experiment with color
my_cols <- brewer.pal(n = 4, name = "Set1")
names(my_cols) <- levels(attributes(SiteTree_gr)$group)
scales::show_col(my_cols); my_cols

tree_pl <- 
    ggtree(tr      = SiteTree_gr, 
           mapping = aes(color = group), 
           layout  = 'circular',
           # set line thikness
           size = 0.1) +
    # adjust coloring of main groups
    scale_color_manual(name = 'Clade',
                       values = my_cols)

# Attaches annotation data to a tree view
# so that all the variables in the Phylo_info_merged are visible to ggtree
tree_pl <- tree_pl %<+% Phylo_info_merged 

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

# if needed, coloring could be binary 
# coord_groups[, col := ifelse(.I%%2, 0.5, 1)]

# Define variable to control x coordinate of segments & labels
my_x <- max(tree_dt$x) + 5

test_tree <- 
    tree_pl + 
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
        # Set font size & family - affects legend only.
        # ("serif" is "Times New Roman").
        text = element_text(size = 8, family = "serif"),
        # Grab bottom-right (x=1, y=0) legend corner 
        legend.justification = c(1,0),
        # and position it in the bottom-right plot area.
        legend.position = c(1.145, 0.025),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        # Set spacing between legend items (keys).
        legend.key.height = unit(4, "mm"),
        # Set margin around entire plot.
        plot.margin = unit(c(t = -0.5, r = 1.3, b = -0.35, l = -0.2), "cm")
    )

ggsave(plot = test_tree,
       filename = "Output/Phylo_tree_draft7.pdf", 
       width = 10, height = 8, scale = 1, units = "cm")

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