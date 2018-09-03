# The order of running the scripts matters

1. Prepare data and compute Effect Sizes (ES) 
2. Create figures

## 1) Prepare data and compute Effect Sizes (ES)

### 1.1 Run `Compute_ES.R`

Computing ES for each record using several methods.

### 1.2 Run `Prepare_data.R`

This script produces the cleaned version of the GloPL data, which is further used for making the figures.

### Simple descriptive statistics and ES ANOVA (optional)

Run the `Check_simple_stats.R` script to check some simple descriptive statistics and run ANOVA on the effect size explained by the level of supplementation.


## 2) Create figures
The order of running the scripts is not an issue this time, with the exception that before executing `Whittaker_biomes_plot.R` one must run `Extract_temp_rainfall.R`.

### Whittaker biomes
PL records are placed within the Whittaker biomes polygons based on their temperature-precipitation pairs.

See `Whittaker_biomes_plot.R` script and the helper script `Extract_temp_rainfall.R`. The helper script must be run before making the graph, since `Whittaker_biomes_plot.R` relays on the temperature-precipitation raster extractions.

The Whittaker biomes polygons are the ones digitized and stored in the package [plotbiomes](https://github.com/valentinitnelav/plotbiomes).
The `plotbiomes` package simulates the graph from *Figure 5.5 in Ricklefs, R. E. (2008), The economy of nature. W. H. Freeman and Company. (Chapter 5, Biological Communities, The biome concept)*.

`Extract_temp_rainfall.R` script was used to extract pairs of temperature and precipitation at study locations.
These extractions together with the digitized biome polygons were used for constructing the final graph in the `Whittaker_biomes_plot.R` script.

### Global map with study locations
See `Global_map_pollen_lim.R` script.

### Phylogenetic tree
See `Phylogeny_graph.R` script.

### Publication frequencies across time
See `Publication_frequencies.R` script.
