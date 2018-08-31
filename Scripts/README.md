# Running scripts respects a certain order

1. Prepare data and compute Effect Sizes (ES) 
2. Make graphs

## 1) Prepare data and compute Effect Sizes (ES)

### 1.1 Run `Compute_ES.R`

No aggregation takes place, just computing ES using several methods.
Several ways of computing ES were tested. Results are saved at `Output/cache/master_es_multi_methods.csv`.

No need to run the helper script `ES_functions.R` which contains the helper ES functions. The helpers are run on the fly when executing `Compute_ES.R`.

### 1.2 Run `prepare_data.R`

This script produces the cleaned version of the GloPL data, which is further used for making the graphs.


## 2) Make graphs
The order of script execution is not an issue this time, with the exception that before executing `Whittaker_biomes_plot.R` one must run `Extract_temp_rainfall.R`.

### Whittaker biomes graph
PL records are placed within the Whittaker biomes polygons based on their temperature-precipitation pairs.

See `Whittaker_biomes_plot.R` script and the helper script `Extract_temp_rainfall.R`. The helper script must be run before making the graph, since `Whittaker_biomes_plot.R` relays on the temperature-precipitation raster extractions.

The Whittaker biomes polygons are the ones digitized and stored in the package [plotbiomes](https://github.com/valentinitnelav/plotbiomes).
The `plotbiomes` package simulates the graph from *Figure 5.5 in Ricklefs, R. E. (2008), The economy of nature. W. H. Freeman and Company. (Chapter 5, Biological Communities, The biome concept)*.

`Extract_temp_rainfall.R` script was used to extract pairs of temperature and precipitation at study locations.
These extractions together with the digitized biome polygons were used for constructing the final graph in the `Whittaker_biomes_plot.R` script.

### Global map with study locations
See `Global_map_pollen_lim.R` script.

### Phylogenetic tree graph
See `Phylogeny_graph.R` script.

### Publication frequencies graph
See `publication_frequencies.R` script.

### Simple stats and ANOVA (optional)
See `check_simple_stats.R` script. 
