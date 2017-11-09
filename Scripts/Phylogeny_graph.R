####################################################################################
## Phylogeny graph for pollen limitation dataset
####################################################################################

library(readxl)
library(picante)
library(ape)
library(pez)
library(taxize)
library(data.table)
library(lazyeval)
library(ggplot2)
library(glue)
# Intsall ggtree
# source("https://bioconductor.org/biocLite.R")
# biocLite("ggtree")
# attach ggtree
library(ggtree)
# more about ggtree at: https://bioconductor.org/packages/release/bioc/html/ggtree.html

library(Taxonstand)
library(wikitaxa)

# =================================================================================
# Read & prepare data
# =================================================================================
# Read species names and their corresponding family
data_aggregated <- data.table(readxl::read_excel(path = "Data/phylogeny/specieslist.xlsx", 
                                                 sheet     = 1, 
                                                 col_names = TRUE))
# Get only unique records by species names
dataspecies <- unique(data_aggregated, by = "Species_accepted_names")

# Read phylogeny information to be used for labeling the plot
Phylo_info <- data.table(readxl::read_excel(path = "Data/phylogeny/Phylogeny information_VS.xlsx", 
                                            sheet     = 1, 
                                            col_names = TRUE))
# If needed, replace spaces with _ in column names
data.table::setnames(Phylo_info, gsub(" ", "_", names(Phylo_info)))

# Check if any differences by species names between the 2 files
# This is important because the key to merge the files is the species names
all.equal(sort(Phylo_info$Species_accepted_names), 
          sort(dataspecies$Species_accepted_names))
# if not TRUE then check differences
setdiff(dataspecies$Species_accepted_names, Phylo_info$Species_accepted_names)
setdiff(Phylo_info$Species_accepted_names, dataspecies$Species_accepted_names)
# "Physocarpus_amurensis" "Silene_stockenii" where added in Phylogeny information_VS
# as they were present in the tree object already.
# Need checking if I added correct info!

# Merge the two files
Phylo_info <- merge(x = Phylo_info,
                    y = dataspecies[,.(Species_accepted_names, Family)],
                    by = "Species_accepted_names",
                    all.x = TRUE)

# --------------------------------------------------------------------------------
# Check some taxa names
# --------------------------------------------------------------------------------
my_sp <- fread("data/phylogeny/taxa_to_check.csv")
my_sp[, taxa4tpl := gsub(pattern = "_", replacement = " ", x = taxa, fixed = TRUE)]

my_sp <- merge(x = my_sp,
               y = Phylo_info,
               by.x = "taxa",
               by.y = "Species_accepted_names",
               all.x = TRUE)

my_tpl <- Taxonstand::TPL(splist = my_sp$taxa4tpl)
my_tpl

my_sp <- merge(x = my_sp,
               y = my_tpl[, c("Taxon", "Taxonomic.status", "Family", "New.Genus",
                              "New.Species", "New.Taxonomic.status", "Typo")],
               by.x = "taxa4tpl",
               by.y = "Taxon",
               all.x = TRUE)

my_families <- unique(my_sp$Family.y)
my_orders <- vector(mode = "character", length = length(my_families))

for (i in 1:length(my_families)){
    my_dt <- data.table(wt_wikispecies(name = my_families[i])$classification)
    my_orders[i] <- my_dt[rank == "Ordo", name]
}

my_orders_df <- data.frame(my_families, my_orders)

my_sp <- merge(x = my_sp,
               y = my_orders_df,
               by.x = "Family.y",
               by.y = "my_families",
               all.x = TRUE, sort = FALSE)

fwrite(my_sp, file = "Output/sp_to_check4tree.csv")

# --------------------------------------------------------------------------------
# Build phylogeny
# --------------------------------------------------------------------------------
# Read tree from Tiffany
tree <- read.tree("Data/phylogeny/Aggre.tree.tre")
# tree <- read.tree("Data/phylogeny/phylo1265species.tre")
SiteTree <- pez::congeneric.merge(tree    = tree, 
                                  species = Phylo_info$Species_accepted_names, 
                                  split   = " ")
all.equal(tree, SiteTree)
identical(tree, SiteTree)
# Why the need of congeneric.merge if the two trees are identical?

# "Physocarpus_amurensis" "Silene_stockenii"  are extra in the tree 
# when using Phylogeny information.xlsx
setdiff(SiteTree$tip.label, Phylo_info$Species_accepted_names)
setdiff(Phylo_info$Species_accepted_names, SiteTree$tip.label)

# Merge tree tip labels with data from Phylogeny information.xlsx 
# tip.label needs to be used exactly as such (with this name).
# This is needed for merging data with the tree based on tip labels with  %<+% operator
tip_lbs <- data.table(tip.label = SiteTree$tip.label)
# Phylo_info_merged will contain extra 2 rows if Phylogeny information.xlsx was used
# (the 2 extra species discussed above)
Phylo_info_merged <- merge(x = tip_lbs, 
                           y = Phylo_info, 
                           by.x  = "tip.label", 
                           by.y  = "Species_accepted_names", 
                           all.x = TRUE, 
                           sort  = FALSE)

# Phylo_info_merged[is.na(order_group), order_group := "Other"]

# Split by APG4_group data - for coloring purposes
# inspired from https://bioconductor.org/packages/devel/bioc/vignettes/ggtree/inst/doc/treeAnnotation.html
# or http://www.ggplot2-exts.org/ggtree.html
APG4_gr <- split(Phylo_info_merged$tip.label, Phylo_info_merged$APG4_group)
names(APG4_gr)
length(APG4_gr)
SiteTree_gr <- ggtree::groupOTU(SiteTree, APG4_gr)

# =================================================================================
# Plot tree
# =================================================================================
str(SiteTree_gr)
tree_pl <- ggtree(tr      = SiteTree_gr, 
                  mapping = aes(color = group), 
                  layout  = 'circular') +
    geom_tiplab(aes(angle=angle), # to plot labels properly alligned from the center of the circle
                size = 1) +
    theme(legend.position = "right")
# attaches annotation data to a tree view
# so that all the columns in the Phylo_info_merged are visible to ggtree
tree_pl2 <- tree_pl %<+% Phylo_info_merged +
    geom_tippoint(aes(color = order_group),
                  position = position_nudge(x = 1),
                  shape = 16,
                  size = 0.5,
                  show.legend = FALSE) +
    geom_tippoint(aes(color = order_group),
                  position = position_nudge(x = 40),
                  shape = 16,
                  size = 0.5,
                  show.legend = FALSE) +
    geom_text2(aes(angle = angle, 
                   subset = !isTip, 
                   label = node), 
               hjust = 1, 
               vjust = 0.5, 
               size  = 0.6,
               color = "black") +
    geom_text(aes(angle = angle,
                  color = order_group,
                  label = ifelse(is.na(order_group),
                                 yes = "",
                                 no  = paste("node:", node,
                                             "y:", y,
                                             "order:", order_group,
                                             "family:", Family))), 
              hjust = 1, 
              vjust = 0.5, 
              size  = 0.6,
              nudge_x = 75,
              show.legend = FALSE)

ggsave(plot = tree_pl2,
       filename = "Output/Phylo_tree_draft5.pdf", 
       width = 10, height = 10, scale = 5, units = "cm")
# use geom_tippoint and geom_text instead of geom_strip
# https://bioconductor.org/packages/devel/bioc/vignettes/ggtree/inst/doc/treeAnnotation.html

###################
tree_dt <- data.table(tree_pl$data)

test_tree <- tree_pl2 + 
    geom_segment(aes(x = max(tree_dt$x) + 80, 
                     y = 2, 
                     xend = max(tree_dt$x) + 80, 
                     yend = 19),
                 colour = "black",
                 lineend = "round",
                 show.legend = FALSE) +
    geom_cladelabel(node=10, label="test label")
    
ggsave(plot = test_tree,
       filename = "Output/Phylo_tree_draft6.pdf", 
       width = 10, height = 10, scale = 5, units = "cm")


###############################################
# Trial with  ggtree::geom_strip
###############################################
# str(tree_pl, max.level = 1)
tree_dt <- data.table(tree_pl$data)

# Split tip indices by desired order groups (NA-s are not considered if any)
# Orders.tip.idx <- split(1:dim(Phylo_info_merged)[1], Phylo_info_merged$order_group)
all_taxa_y <- split(tree_dt[isTip == TRUE, y], tree_dt[isTip == TRUE, order_group])

my_taxa <- "Lamiales"
apply_geom_strip <- function(my_taxa, ...){
    nodes <- tree_dt[isTip == TRUE, node, by = "order_group"][order_group == my_taxa, node]
    # split vector of indices in continous sequences
    # https://stackoverflow.com/questions/25793981/split-a-vector-by-its-sequences
    nodes_gr <- split(nodes, cumsum(c(TRUE, diff(nodes)!=1)))
    # create empty list
    my_strips <- vector(mode = "list", length = length(nodes_gr))
    # for each sequence create a geom_strip to be plot on the tree
    for (i in 1:length(nodes_gr)){
        my_strips[[i]] <- ggtree::geom_strip(taxa1 = nodes_gr[[i]][1], 
                                             taxa2 = nodes_gr[[i]][length(nodes_gr[[i]])],
                                             label = my_taxa)
    }
    return(my_strips)
}


# y <- tree_dt[isTip == TRUE, y, by = "order_group"][order_group == "Lamiales", sort(y)]
# y.gr <- split(y, cumsum(c(TRUE, diff(y)!=1)))
# y.gr
# tree_pl + geom_line(mapping = aes(x = 243.2697,
#                                   y = y.gr[[2]]),
#                     data = tree_dt)

tree_pl2 <- tree_pl + 
    geom_text(aes(angle=angle,
                  label=ifelse(is.na(order_group),
                               yes = "",
                               no  = paste("n:",node,"y:",y,"gr:",order_group))), 
              hjust=1, vjust=0.5, size=0.6, color = "red") +
    # geom_text(aes(label=y), hjust=1, vjust=1.4, size=0.6, color = "red") +
    # this fails for wather resons...
    # geom_strip(taxa1 = 'Gesneria_pedunculosa', taxa2 = 'Gesneria_vernicosa', label = "Lamiales", offset = 0.5, align=TRUE, barsize = 1)
    # does not work with the y coordinates
    # geom_strip(taxa1 = 1260, taxa2 = 1267, label = "Lamiales", offset = 0.5, align=TRUE, barsize = 1) +
    # but it works with node index
    # geom_strip(taxa1 = 1236, taxa2 = 1243, label = "Lamiales", offset = 0.5, align=TRUE, barsize = 1) +
    # however, because how node and y are related, if the node interval stops earlier (e.g. reaches 1267 for Lamiales case)
    # than its y contor then the strip stops there as weel - try this:
    # geom_strip(taxa1 = 1144, taxa2 = 1267, label = "Lamiales", offset = 0.5, align=TRUE, barsize = 1)
    apply_geom_strip( my_taxa="Other", offset=0.5, offset.text=10, align=TRUE, barsize=1, hjust=0.5, color='red') +
    lapply(X = tree_dt[isTip == TRUE & order_group != "Other", unique(order_group)],
           FUN = apply_geom_strip, offset=0.5, offset.text=10, align=TRUE, barsize=1, hjust=0.5)
# geom_strip(taxa1 = 41, taxa2 = 42, label = "Other", offset = 0.5, align=TRUE, barsize = 1)

# Save graph
ggsave(plot = tree_pl2,
       filename = "Output/Phylo_tree_draft3.pdf", 
       width = 10, height = 10, scale = 5, units = "cm")

###
node_labels <- sort(unique(SiteTree_gr$node.label))
node_labels <- node_labels[-1]
order_groups <- sort(unique(Phylo_info_merged$order_group))

setdiff(order_groups, node_labels)


###
###############################################
# Trial with geom_cladelabel
###############################################

mrca(SiteTree_gr)["Phyllospadix_scouleri", "Dichorisandra_incurva"]
MRCA(tree_pl, tip=c('Phyllospadix_scouleri', 'Dichorisandra_incurva'))

test_tree <- tree_pl + geom_cladelabel(node=1270, label="test label")

ggsave(plot = test_tree,
       filename = "Output/Phylo_tree_draft6.pdf", 
       width = 10, height = 10, scale = 5, units = "cm")

.node.desc(tree, 1243)



