# Graphs & data preparation for Nature Scientific Data

Running scripts respects a certain order:

## 1) Compute ES & data cleaning 
### 1.1. `Compute_ES.R`
One should run first `Compute_ES.R` which computes ES columns in a table that can be later merged with the main PL data using as key the `unique_number` column. 

Simple means were applied for consistency (weighted means were finally NOT used as they were applicable to only a few cases because almost all authors do not report all sample sizes).

### 1.2. `check_clean_update.R`
Then run `check_clean_update.R` for some data cleaning (checks for potential typos with regular expression and gives sorted unique values per column in order to detect any possible artefacts).

Note that `merge_datasets.R` script carried a fair amount of data checking and cleaning but is now **obsolete**. It also used to merge (join) PL data with metadata and citations information. It is kept nevertheless for tracking back the cleaning process if ever needed, though some of the files used as input are now obsolete since some data cleaning was done also manually with the introduction of `GloPL_with_id.csv` file on Sat 27-Jan-18 3:37 PM.

## 2) Graphs
Once the two scripts above (`Compute_ES.R` & `check_clean_update.R`) were executed, one can use the output of `check_clean_update.R` to build the graphs. Order of execution is not an issue this time, with the exception that before executing `Whittaker_biomes_plot.R` one must run `Extract_temp_rainfall.R`.

### Whittaker biomes graph
PL cases are placed within the Whittaker biomes polygons based on their temperature-precipitation pairs.

See `Whittaker_biomes_plot.R` script and the helper script `Extract_temp_rainfall.R`. The helper script should be run before making the graph since `Whittaker_biomes_plot.R` relays on the temperature-precipitation raster extractions.

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
See ` publication_frequencies.R` script.

## 3) Simple stats and ANOVA
See `check_simple_stats.R` script. 
