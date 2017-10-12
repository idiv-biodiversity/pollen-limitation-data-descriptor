####################################################################################
## Phylogeny graph for pollen limitation dataset
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

# Read tree from Tiffany
tree <- read.tree("Data/phylogeny/Aggre.tree.tre")

# Read species names and thier corresponding family
data.aggregated <- data.table(readxl::read_excel(path = "Data/phylogeny/specieslist.xlsx", 
                                                 sheet     = 1, 
                                                 col_names = TRUE))
# get only unique records by species names
dataspecies <- unique(data.aggregated, by = "Species_accepted_names")

# Read phylogeny information to be used for labeling the plot
Phylo.info <- data.table(readxl::read_excel(path = "Data/phylogeny/Phylogeny information_VS.xlsx", 
                                            sheet     = 1, 
                                            col_names = TRUE))
# replace spaces with dots in column names
data.table::setnames(Phylo.info, gsub(" ", ".", names(Phylo.info)))

# check if any differences by species names between the 2 files
# this is important because the key to merge the files is the species names
all.equal(sort(Phylo.info$Species_accepted_names), 
          sort(dataspecies$Species_accepted_names))
# TRUE

# merge the two files
Phylo.info <- merge(x = Phylo.info,
                    y = dataspecies[,.(Species_accepted_names, Family)],
                    by = "Species_accepted_names")

# Build phylogeny
SiteTree <- pez::congeneric.merge(tree    = tree, 
                                  species = Phylo.info$Species_accepted_names, 
                                  split   = " ")
all.equal(tree, SiteTree)
identical(tree, SiteTree)

# "Physocarpus_amurensis" "Silene_stockenii"  are extra in the tree
setdiff(SiteTree$tip.label, Phylo.info$Species_accepted_names)
setdiff(Phylo.info$Species_accepted_names, SiteTree$tip.label)

# Merge tree tip labels with data from Phylogeny information.xlsx 
tip.lbs <- data.table(tip.label = SiteTree$tip.label)
# Phylo.info.merged will contain extra 2 rows (the 2 extra species detected above)
Phylo.info.merged <- merge(x = tip.lbs, 
                           y = Phylo.info, 
                           by.x  = "tip.label", 
                           by.y  = "Species_accepted_names", 
                           all.x = TRUE, 
                           sort  = FALSE)

# group by APG4.Group data
APG4.gr <- split(Phylo.info.merged$tip.label, Phylo.info.merged$APG4.Group)
SiteTree.gr <- ggtree::groupOTU(SiteTree, APG4.gr)
str(SiteTree.gr)

# NOTE from Sam - try to use .node.desc(tree, node_id) from picante
# https://rdrr.io/cran/picante/src/R/node.desc.R
# test <- .node.desc(SiteTree, 1268)
# ee<- SiteTree$edge
# node <- 2000
# ee[ee[,1]==node,2]

# SiteTree.gr$tip.label == "Nymphaea_ampla"

# MRCA(SiteTree.gr, tip="Nymphaea_ampla")
# MRCA(SiteTree.gr, tip=c("Nymphaea_ampla", "Phyllospadix_scouleri", "Phyllospadix_serrulatus"))
# https://bioconductor.org/packages/devel/bioc/vignettes/ggtree/inst/doc/treeManipulation.html#internal-node-number

myOrders <- Phylo.info.merged[, .I, by = "order.group"]
unique(myOrders$order.group)
x <- myOrders[order.group == "Other", I]
x2 <- split(x, cumsum(c(TRUE, diff(x)!=1)))
# https://stackoverflow.com/questions/25793981/split-a-vector-by-its-sequences


# ========================================
# Plot tree
# ========================================
tree.pl <- ggtree(tr = SiteTree.gr, 
                  aes(color=group), 
                  layout = 'circular') + 
    geom_tiplab(size=1, aes(angle=angle)) +
    theme(legend.position="right") + 
    # https://bioconductor.org/packages/devel/bioc/vignettes/ggtree/inst/doc/treeAnnotation.html#annotate-clades
    geom_strip(taxa1 = 2, taxa2 = 11, label = "Other", offset = 0.5, align=TRUE, barsize = 1)
    # geom_cladelabel(node=1:1267, label=test$order.group, align=TRUE, offset=.5)
    # geom_cladelabel(node=x2$`1`, label="Other", align=TRUE, offset=.5)
    # geom_cladelabel(node=test[order.group == "Other", I], label="Other", align=TRUE, offset=.5) # goes all around without gaps...
    # geom_cladelabel(node=1:100, label="Phyllospadix_scouleri", align=TRUE, offset=.5)
    # geom_cladelabel(node=1:1267, label="complete_circle", align=TRUE, offset=.5) # makes the whole circle
    # geom_cladelabel(node=1268, label="Phyllospadix_scouleri", align=TRUE, offset=.5) # makes the whole circle also

# tree.pl
ggsave("Output/Phylo_tree_draft3.pdf", width=10, height=10, scale = 2.5, units="cm")
# Thereianthus_spicatus seems to be an Eudicot within Monocots clade
# https://en.wikipedia.org/wiki/Iridaceae seems to be actually in Monocots (?)

