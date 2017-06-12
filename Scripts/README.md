## Scripts to prepare graphs for data descriptor submission

### Whittaker biomes plot
The Whittaker biomes polygons were digitized in QGIS after georeferencing the graph in Fig 5.5 p. 92 from Ricklefs – The Economy of Nature
(at: https://www.academia.edu/15092278/Ricklefs_The_Economy_of_Nature_6th_txtbk)

The script `Whittaker_biomes_prepare.R` was used to prepare the digitized polygons for plotting.

`Extract_temp_rainfall.R` was used to extract pairs of temperature and precipitation at study locations.
These extractions together with the digitized biome polygons were used as layer for plotting in `Whittaker_biomes_plot.R`

`Extract_Whittaker_biomes.R` was further used for extracting Whittaker biomes names at each study location.

### Global map with study locations
Check the script `Global_map_pollen_lim.R`
