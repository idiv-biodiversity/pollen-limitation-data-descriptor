# File `GloPL.csv`

This file is identical with the DRYAD uploaded `GloPL.csv` file - [download](https://datadryad.org/bitstream/handle/10255/dryad.188937/GloPL.csv?sequence=1). It is the output of `Prepare_data.R` script.

# File `GloPL_with_id_updated_ES.csv`

This file is also the output of `Prepare_data.R`, and is used in all figure preparation scripts. It differs from `GloPL.csv` by having two extra helper columns "unique_number", "unique_study_number" and different column order.

# Intermediary output in the `cache` folder

When running scripts, some intermediary output is stored in the `cache` folder. Details about these files are stored as comments in the scripts that produces them.

- `cache/extractions_temp_pp.csv` - produced by `Extract_temp_rainfall.R`. Contains temperature-precipitation pairs for each record/study to be used in building the Whittaker biomes graph (Figure 4).

- `cache/lookup_cols.csv` - manually produced to serve as lookup table used in `Prepare_data.R`

- `cache/master_es_multi_methods.csv` - the output of `Compute_ES.R`

- `cache/table_for_publication_freq_graph_ok.csv` produced by `Publication_frequencies.R`. This is the table behind the publication frequency graph (Figure 1).

# Folder `share`

This folder is empty, but will be populated with the PDF and PNG files for each figure once their corresponding scripts are executed. 
