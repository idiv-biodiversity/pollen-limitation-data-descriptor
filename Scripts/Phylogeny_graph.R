####################################################################################
## Phylogeny graph for pollen limitation dataset
# NOTE from Sam - try to use .node.desc(tree, node_id) from picante
# https://rdrr.io/cran/picante/src/R/node.desc.R
####################################################################################

library(readxl)
library(picante)
library(ape)
library(pez)
library(taxize)
library(data.table)

# Intsall ggtree
# source("https://bioconductor.org/biocLite.R")
# biocLite("ggtree")
# attach ggtree
library(ggtree)
# more about ggtree at: https://bioconductor.org/packages/release/bioc/html/ggtree.html

# =================================================================================
# Read & prepare data
# =================================================================================

# Read tree
tree <- read.tree("Data/phylogeny/Aggre.tree.tre")

# Read species names and family file
data.aggregated <- data.table(readxl::read_excel(path = "Data/phylogeny/specieslist.xlsx", 
                                                 sheet     = 1, 
                                                 col_names = TRUE))
# get only unique records by species names
dataspecies <- unique(data.aggregated, by = "Species_accepted_names")

# bound species names into the tree if missing from it
SiteTree <- pez::congeneric.merge(tree    = tree, 
                                  species = dataspecies$Species_accepted_names, 
                                  split   = " ")
all.equal(tree, SiteTree)
identical(tree, SiteTree)

# "Physocarpus_amurensis" "Silene_stockenii"  are extra in the tree
setdiff(SiteTree$tip.label, dataspecies$Species_accepted_names)
setdiff(dataspecies$Species_accepted_names, SiteTree$tip.label)

# Read phylogeny information to be used for labeling the plot
Phylo.info <- data.table(readxl::read_excel(path = "Data/phylogeny/Phylogeny information.xlsx", 
                                            sheet     = 1, 
                                            col_names = TRUE))
# replace spaces with dots in column names
data.table::setnames(Phylo.info, gsub(" ", ".", names(Phylo.info)))

# merge tree tip labels with data from Phylogeny information.xlsx 
tip.lbs <- data.table(tip.label = SiteTree$tip.label)

Phylo.info.merged <- merge(tip.lbs, Phylo.info, 
                           by.x  = "tip.label", 
                           by.y  = "Species_accepted_names", 
                           all.x = TRUE, 
                           sort  = FALSE)

# group by APG4.Group data
APG4.gr <- split(Phylo.info.merged$tip.label, Phylo.info.merged$APG4.Group)
SiteTree.gr <- ggtree::groupOTU(SiteTree, APG4.gr)
str(SiteTree.gr)
# ========================================
# Plot tree
# ========================================
tree.pl <- ggtree(tr = SiteTree.gr, 
                  aes(color=group), 
                  layout = 'circular') + 
    geom_tiplab(size=1, aes(angle=angle)) +
    theme(legend.position="right") #+ 
# geom_cladelabel(node=4:5, label="Alismatales", align=TRUE, offset=.5)
tree.pl
ggsave("Output/tree_draft2.pdf", width=30, height=30, units="cm")
# Thereianthus_spicatus seems to be an Eudicot within Monocots clade
# https://en.wikipedia.org/wiki/Iridaceae seems to be actually in Monocots (?)