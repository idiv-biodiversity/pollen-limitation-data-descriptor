# Graphs & data preparation for Nature Scientific Data

## Compute PL ES (Pollen Limitation Effect Sizes)
See `Compute_ES.R` script.

Computes pollen limitation effect sizes (ES) & variances. Simple means were applied for consistency (weighted means were finally NOT used as they were applicable to only a few cases because almost all authors do not report all sample sizes). Variance formula was adapted from *Koricheva et al., 2013 - Handbook of Meta-Analysis, p 64*

## Whittaker biomes graph
See `Whittaker_biomes_plot.R` script and the helper scripts `Extract_temp_rainfall.R`.

Study cases are placed within the Whittaker biomes space.

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

## Prepare data for upload
See `merge_datasets.R` script to merge PL data with metadata and citations information. Also does some data cleaning and checking.
