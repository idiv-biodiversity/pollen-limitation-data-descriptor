# The order of running the scripts matters:

## 1) Compute effect sizes (ES) and prepare data

### 1.1 Run `Compute_ES.R`

This script computes ES for each record using several methods. It calls the helper script `helper_functions/ES_functions.R`.

### 1.2 Run `Prepare_data.R`

This script produces a cleaned version of the GloPL data, which is further used for making figures and is also stored on [DRYAD](https://doi.org/10.5061/dryad.dt437).

## 2) Create figures

The order of running the scripts is not an issue this time, with the exception that before executing `Whittaker_biomes_plot.R` one must run `Extract_temp_rainfall.R`.

### Whittaker biomes
The GloPL records are placed within the Whittaker biomes polygons based on their temperature-precipitation information.

See `Whittaker_biomes_plot.R` script and the helper script `Extract_temp_rainfall.R`. The helper script must be run before making the graph, since `Whittaker_biomes_plot.R` depends on the temperature-precipitation raster extractions.

The Whittaker biomes polygons are the ones digitized and stored in the package [plotbiomes](https://github.com/valentinitnelav/plotbiomes).
The `plotbiomes` package simulates the graph from *Figure 5.5 in Ricklefs, R. E. (2008), The economy of nature. W. H. Freeman and Company. (Chapter 5, Biological Communities, The biome concept)*.

`Extract_temp_rainfall.R` script was used to extract pairs of temperature and precipitation at study locations. The raster datasets were downloaded from http://chelsa-climate.org/, CHELSA v1.2.
These extractions together with the digitized biome polygons were used for constructing the final graph in the `Whittaker_biomes_plot.R` script.

### Global map with study locations
See `Global_map_pollen_lim.R` script.

### Phylogenetic tree
See `Phylogeny_graph.R` script.

### Publication frequencies across time
See `Publication_frequencies.R` script.

## 3) Descriptive statistics and ES ANOVA

`Check_simple_stats.R` script contains code for some descriptive statistics and to run ANOVA on the effect size explained by the level of supplementation.
