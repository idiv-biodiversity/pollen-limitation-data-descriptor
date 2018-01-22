# Graphs for Nature Scientific Data

## Compute PL ES (Pollen Limitation Effect Sizes)
See `Compute_ES.R` script.

## Whittaker biomes graph
See `Whittaker_biomes_plot.R` script and the helper scripts `Extract_temp_rainfall.R`.

**Notes**

The Whittaker biomes polygons are the ones digitized and stored in package [plotbiomes](https://github.com/valentinitnelav/plotbiomes).
The `plotbiomes` package simulates the graph from *Figure 5.5 in Ricklefs, R. E. (2008), The economy of nature. W. H. Freeman and Company. (Chapter 5, Biological Communities, The biome concept)*.
Details are given in [Whittaker_biomes_dataset document](https://rawgit.com/valentinitnelav/plotbiomes/master/html/Whittaker_biomes_dataset.html).

`Extract_temp_rainfall.R` script was used to extract pairs of temperature and precipitation at study locations.
These extractions together with the digitized biome polygons were used as layers for constructing final graph with `Whittaker_biomes_plot.R` script.

## Global map with study locations
See `Global_map_pollen_lim.R` script.

## Phylogenetic tree graph
See `Phylogeny_graph.R` script.

## Publications/PL cases frequencies graph
See ` publication_frequencies.R` script.
