# Graphs & data preparation for Nature Scientific Data

**Running scripts respects a certain order:**

1. clean data and compute ES
2. make graphs & run simple stats

## 1) Data cleaning and compute ES

### 1.1. `diff_analysis_data.R`

Script to apply some corrections on the row data used for graphs and for submitting.
There were mismatches between the raw data assumed cleaned for submission and the data for analysis.
It seems that the data for analysis got updated manually while the row data not.
Therefore, this script was introduced lately and attempts to find mismatches between the two version of the data and align them. This cleaning step unfortunately added complexity to the workflow, therefore the order of the scripts needs to be respected.

### 1.2. `Compute_ES.R`

No aggregation takes place, just computing the simple log response ratios as ES.
Computes ES columns that will replace existing ES columns from the main PL data (output of `diff_analysis_data.R`)  (updating is done in the next step - `check_clean_update.R`).

### 1.3. `check_clean_update.R`

This script produces the cleaned version of PL data that is further used for making graphs and submission.
Executes data cleaning (checks for potential typos with regular expression and gives sorted unique values per column in order to detect any possible artefacts). 

Note that previosly, the script `merge_datasets.R`, carried a fair amount of data checking and cleaning but is now **obsolete**. It also used to merge (join) PL data with metadata and citations information. It is kept nevertheless for tracking back the cleaning process if ever needed. But some of the files used as input are now obsolete since some data cleaning was done manually in the file `GloPL_with_id.csv`. The script `merge_datasets.R` actually helped at detecting the data artefacts. Therefore is kept now as documentation.

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
See ` publication_frequencies.R` script.

## 3) Simple stats and ANOVA
See `check_simple_stats.R` script. 
