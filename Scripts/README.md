# Graphs & data preparation for Nature Scientific Data

**Running scripts respects a certain order:**

1. Compute ES and prepare data 
2. Make graphs & run simple stats

## 1) Compute ES and prepare data

### 1.1 Run `Compute_ES.R`

No aggregation takes place, just computing ES using several methods. No need to run the helper script `ES_functions.R` which contains the helper ES functions. This script is run on the fly from `Compute_ES.R`. Several ways of computing ES were tested. Results are also used as histograms for the distribution of `PL_Effect_Size...` variables.

### 1.2 Run `prepare_data.R`

This script produces the cleaned version of PL data that is further used for making graphs and that will end up published.
It updates the master ES columns in the raw data file - `GloPL_with_id…` It also does some data cleaning.

Note that previosly, the scripts `diff_analysis_data.R`, `check_clean_update.R` and further `merge_datasets.R` carried a fair amount of data checking and cleaning but are now **obsolete**.


## 2) Graphs
Order of script execution is not an issue this time, with the exception that before executing `Whittaker_biomes_plot.R` one must run `Extract_temp_rainfall.R`.

### Whittaker biomes graph
PL cases are placed within the Whittaker biomes polygons based on their temperature-precipitation pairs.

See `Whittaker_biomes_plot.R` script and the helper script `Extract_temp_rainfall.R`. The helper script must be run before making the graph since `Whittaker_biomes_plot.R` relays on the temperature-precipitation raster extractions.

The Whittaker biomes polygons are the ones digitized and stored in the package [plotbiomes](https://github.com/valentinitnelav/plotbiomes).
The `plotbiomes` package simulates the graph from *Figure 5.5 in Ricklefs, R. E. (2008), The economy of nature. W. H. Freeman and Company. (Chapter 5, Biological Communities, The biome concept)*.
Details are given in [Whittaker_biomes_dataset document](https://rawgit.com/valentinitnelav/plotbiomes/master/html/Whittaker_biomes_dataset.html).

`Extract_temp_rainfall.R` script was used to extract pairs of temperature and precipitation at study locations.
These extractions together with the digitized biome polygons were used as layers for constructing final graph with `Whittaker_biomes_plot.R` script.

### Global map with 'study'/'PL cases' locations
See `Global_map_pollen_lim.R` script.

### Phylogenetic tree graph
See `Phylogeny_graph.R` script.

### Publications/'PL cases' frequencies graph
See `publication_frequencies.R` script.

## 3) Simple stats and ANOVA (optional)
See `check_simple_stats.R` script. 
