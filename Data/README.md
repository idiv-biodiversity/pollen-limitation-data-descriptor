# File `GloPL_with_id.csv`

It contains raw GloPL data used in `Compute_ES.R` to compute effect sizes (ES) and then curated with `Prepare_data.R`. This dataset went previously through other various curation steps.

# File `NaturalEarth.RData`

Helper binary data used in the `Global_map_pollen_lim.R` script to create the **Figure 2.** *Global distribution of data from the GloPL database.* The binary file contains several spatial files that were downloaded from http://www.naturalearthdata.com/
Using the ArcMap software, the `NE_graticules` SpatialLinesDataFrame was adjusted to 10 dg step for latitude lines and 20 dg for longitude lines.

# File `phylogeny/Aggre.tree_new.tre`

Tree edited by J.C. Vamosi, based on dated molecular phylogeny created by Zanne and colleagues:

> Zanne, A. E. et al. Three keys to the radiation of angiosperms into freezing environments. Nature 506, 89–92 (2014)

This tree was used in the `Phylogeny_graph.R` script to create the **Figure 3.** *Phylogenetic distribution of data from the GloPL database.*

# File `phylogeny/Phylogeny_annotation.csv`

Data used for annotating the tree in `Phylogeny_graph.R` script.
