## Data descriptors

### Whittaker biomes plot
The Whittaker biomes polygons were digitized in QGIS after georeferencing the graph in Fig 5.5 p. 92 from Ricklefs – The Economy of Nature (may be available [here](https://www.academia.edu/15092278/Ricklefs_The_Economy_of_Nature_6th_txtbk)). The multi-page PDF document was split into component pages with [PDFTK Builder](https://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/) application. The PDF page containing the Whittaker biomes graph was imported in [Inkscape](https://inkscape.org/en/). Text and extra graphic layers where removed and the remaining layers were flipped and exported as *.png file and georeferenced in QGIS. Further notes in `Whittaker_biomes_prepare.R`.

The script `Whittaker_biomes_prepare.R` was used to prepare the digitized polygons for plotting.

`Extract_temp_rainfall.R` was used to extract pairs of temperature and precipitation at study locations.
These extractions together with the digitized biome polygons were used as layers for plotting in `Whittaker_biomes_plot.R`

### Global map with study locations
Check the script `Global_map_pollen_lim.R`

### Phylogenetic tree
See `Phylogeny_graph.R` script. In developing stage. There are issues with the clustering of species in the original tree.