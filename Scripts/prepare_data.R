# /////////////////////////////////////////////////////////////////////////
# Prepare GloPL dataset
# /////////////////////////////////////////////////////////////////////////

rm(list = ls(all.names = TRUE)); gc(reset = TRUE)

library(data.table)


# Read GloPL table
GloPL <- fread("Data/GloPL_with_id_2_2_2018.csv", 
               colClasses = "character", 
               na.strings = c("NA","N/A","null", ""))

# Read PL ES table
ES <- fread("Output/cache/master_es_multi_methods.csv", 
            colClasses = "character")

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

# Update Level_of_Supplementation with level_of_treatment2_WD column
# from PL_ANALYSIS file
pl_analys <- fread("Data/PL_ANALYSIS_12_04_2018.csv",
                   colClasses = "character",
                   na.strings = c("NA","N/A","null", ""))
GloPL[pl_analys, on = "unique_number", 
      Level_of_Supplementation := level_of_treatment2_WD]

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


# save to csv file
write.csv(GloPL,
          file = "Output/GloPL_with_id_updated_ES.csv", 
          row.names = FALSE)
