# /////////////////////////////////////////////////////////////////////////
#
# Prepare GloPL dataset. Final GloPL data cleaning and preparation. The final
# cleaned data with ES values is used for uploading on DRYAD and figure making.
#
# /////////////////////////////////////////////////////////////////////////


library(data.table)

# Cleans environment, including hidden objects (which begin with a .)
rm(list = ls(all.names = TRUE))

# Read GloPL table
GloPL <- fread("Data/GloPL_with_id.csv", colClasses = "character")

# Read table with corresponding various Effect Size (ES) types
ES <- fread("Output/cache/master_es_multi_methods.csv", colClasses = "character")

# Update "by reference" the raw data with the ES computed based on the log
# response ratio with 0.5 added to zero cases.
GloPL[ES, on = "unique_number", ":=" (
    PL_Effect_Size = PL_Effect_Size_lnRkto0_0.5,
    PL_Effect_Size_Type1 = PL_Effect_Size_Type1_lnRkto0_0.5,
    PL_Effect_Size_Type2 = PL_Effect_Size_Type2_lnRkto0_0.5
)]

# Indicate if the constant was added in the formula of the log response ratio.
# Uses a lookup table to match cases and identifies columns to test for zeroes.
# TRUE means that the constant was added.
lookup <- fread("Output/cache/lookup_cols.csv")
lookup
# Example: if for a row in GloPL table, PL_Effect_Size_Type2 is "Bagout" and
# PL_Effect_Size_Type1 is "FS", then it will test if the values in the
# Bagout_X_FS and Natural_X_FS columns are zero or not. If they are zero then a
# constant was added when computing ES for log response ratio. Use %in% operator
# instead of == to avoid NA cases.
GloPL[lookup, on = .(PL_Effect_Size_Type1, PL_Effect_Size_Type2),
      Constant_added := get(col1.new) %in% 0 | get(col2.new) %in% 0]

# Number of cases where constant was added to log response ratio
GloPL[, sum(Constant_added)] # 59


# Replace "_and_" with "_&_" in the Author column
GloPL[grepl(pattern = '_and_', x = Author), 
      Author := gsub(pattern = "_and_", 
                     replacement = "_&_", 
                     x = Author, 
                     fixed = TRUE)]

# Update Level_of_Supplementation
GloPL[Level_of_Supplementation == "partialplant", 
      Level_of_Supplementation := "partial_plant"]


# Update Natural_N_FS column – replace non-numeric values with NA-s. All other
# columns entering the ES computation were previously tested for such cases in
# the section “Read & prepare data” of the script Compute_ES.R During testing
# only Natural_N_FS showed such 4 cases.
GloPL[is.na(as.numeric(Natural_N_FS)) != is.na(Natural_N_FS), 
      .(unique_number, Natural_N_FS)]
GloPL[, Natural_N_FS := as.numeric(Natural_N_FS)]
# The warnings are ok.

# Save to csv file. This file is almost identical with what is uploaded on
# DRYAD, except that is has two extra helper columns "unique_number",
# "unique_study_number" and the column order is different. This file is used in
# this repo for figure making.
write.csv(GloPL,
          file = "Output/GloPL_with_id_updated_ES.csv", 
          row.names = FALSE)


# Remove extra helper columns and set desired column order for uploading on
# DRYAD.

GloPL[, c("unique_number", "unique_study_number") := NULL]

names_new_order <- c(
    "Author",
    "Year",
    "DOI",
    "Species_Author",
    "Species_accepted_names",
    "Family",
    "Latitude",
    "Longitude",
    "Included_Bennett_et_al_meta_analysis",
    "Level_of_Supplementation",
    "PL_Effect_Size",
    "Constant_added",
    "PL_Effect_Size_Type1",
    "PL_Effect_Size_Type2",
    "Natural_N_FS",
    "Natural_X_FS",
    "Natural_SD_FS",
    "Sup_N_FS",
    "Sup_X_FS",
    "Sup_SD_FS",
    "Bagout_N_FS",
    "Bagout_X_FS",
    "Bagout_SD_FS",
    "Natural_N_SO",
    "Natural_X_SO",
    "Natural_SD_SO",
    "Sup_N_SO",
    "Sup_X_SO",
    "Sup_SD_SO",
    "Bagout_N_SO",
    "Bagout_X_SO",
    "Bagout_SD_SO",
    "Natural_N_SPFR",
    "Natural_X_SPFR",
    "Natural_SD_SPFR",
    "Sup_N_SPFR",
    "Sup_X_SPFR",
    "Sup_SD_SPFR",
    "Bagout_N_SPFR",
    "Bagout_X_SPFR",
    "Bagout_SD_SPFR",
    "Natural_N_SPFL",
    "Natural_X_SPFL",
    "Natural_SD_SPFL",
    "Sup_N_SPFL",
    "Sup_X_SPFL",
    "Sup_SD_SPFL",
    "Bagout_N_SPFL",
    "Bagout_X_SPFL",
    "Bagout_SD_SPFL",
    "Natural_N_SPP",
    "Natural_X_SPP",
    "Natural_SD_SPP",
    "Sup_N_SPP",
    "Sup_X_SPP",
    "Sup_SD_SPP",
    "Bagout_N_SPP",
    "Bagout_X_SPP",
    "Bagout_SD_SPP",
    "Emasculated_treatment",
    "Additional_Treatment_Information",
    "population",
    "year_of_study",
    "on_phylogeny"
)

setcolorder(GloPL, neworder = names_new_order)

# Save to csv file. This file is identical with the one uploaded on DRYAD.
write.csv(GloPL,
          file = "Output/GloPL.csv", 
          row.names = FALSE)
