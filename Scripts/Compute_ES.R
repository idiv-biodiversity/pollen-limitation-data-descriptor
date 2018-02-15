###############################################################################
# Script to compute pollen limitation effect sizes (ES).
# Outputs a CSV file with effect size (ES - the log response ratio) 
# and other helper ES columns.
# These columns will replace any existing ES columns in the row data 
# (this takes place in check_clean_update.R) 
###############################################################################

library(data.table)

# =============================================================================
# Read & prepare data
# =============================================================================
# Read PL data. Treat all values as character. Also convert to NA everything that is:
# "NA", "N/A", "null", "" (last one is treated as blank in Excel).
PL_dt <- fread("output/GloPL_with_id_updated.csv", 
               colClasses = "character", 
               na.strings = c("NA","N/A","null", ""))

# -----------------------------------------------------------------------------
# Read meta_data (all columns as character)
# -----------------------------------------------------------------------------
meta_dt <- fread(file = "Data/for_merging/Meta_data_24_01.csv", 
                 colClasses = "character")
# Delete any quotation symbol from column names
data.table::setnames(meta_dt, gsub("'", "", names(meta_dt)))
# Replace spaces with dots in column names
data.table::setnames(meta_dt, gsub(" ", ".", names(meta_dt)))
# Create a helper idex
meta_dt[, row_idx_meta := 1:.N]

old_names <- meta_dt$Current.Column.Name
new_names <- meta_dt$Variable.within.GloPL.database

# Check if new_names can be found in current column names & check differences.
setdiff(names(PL_dt), new_names)
setdiff(new_names, names(PL_dt))

# Switch names
setnames(PL_dt, 
         old = new_names, 
         new = old_names)

# -----------------------------------------------------------------------------
# Transform desired columns to numeric type.
# -----------------------------------------------------------------------------
cols_to_numeric <- c("supp_X_fruitset_JSJEB", "supp_SD_fruitset_JSJEB", "supp_N_fruitset_JSJEB",
                     "open_X_fruitset_JSJEB", "open_SD_fruitset_JSJEB", "open_N_fruitset_JSJEB",
                     "bagout_X_fruitset_JSJEB", "bagout_SD_fruitset_JSJEB", "bagout_N_fruitset_JSJEB",
                     "supp_X_seeds_per_ovule_Corrected_JR", "supp_SD_seeds_per_ovule_Corrected_JR", "supp_N_seeds_per_ovule_Corrected_JR",
                     "open_X_seeds_per_ovule_Corrected_JR", "open_SD_seeds_per_ovule_Corrected_JR", "open_N_seeds_per_ovule_Corrected_JR",
                     "bagout_X_seeds_per_ovule_Corrected_JR", "bagout_SD_seeds_per_ovule_Corrected_JR", "bagout_N_seeds_per_ovule_Corrected_JR",
                     "supp_X_SPFR_Author", "supp_SD_SPFR_Author", "supp_N_SPFR_Author",
                     "open_X_SPFR_Author", "open_SD_SPFR_Author", "open_N_SPFR_Author",
                     "bagout_X_SPFR_Author", "bagout_SD_SPFR_Author", "bagout_N_SPFR_Author",
                     "supp_X_SPFL_Author", "supp_SD_SPFL_Author", "supp_N_SPFL_Author",
                     "open_X_SPFL_Author", "open_SD_SPFL_Author", "open_N_SPFL_Author",
                     "bagout_X_SPFL_Author", "bagout_SD_SPFL_Author", "bagout_N_SPFL_Author",
                     "supp_X_SPP_Author", "supp_SD_SPP_Author", "supp_N_SPP_Author",
                     "open_X_SPP_Author", "open_SD_SPP_Author", "open_N_SPP_Author",
                     "bagout_X_SPP_Author", "bagout_SD_SPP_Author", 'bagout_N_SPP_Author')

# Check cases where conversion to numeric encounters characters that yield NA-s.
# This helps to check for legitimate conversions
test <- PL_dt[, lapply(.SD, function(col) is.na(as.numeric(col)) != is.na(col)), .SDcols = cols_to_numeric]
for (col in cols_to_numeric[ test[, colSums(.SD) > 0] ]){
    print(col)
    print(PL_dt[test[, get(col)], .(unique_number,  get(col))])
}
# [1] "open_N_fruitset_JSJEB"
# unique_number                                                                 V2
# 1:          2264 need_to_contact_author_for_data;_paper_presents_PLI_(1-open/cross)
# 2:          2265 need_to_contact_author_for_data;_paper_presents_PLI_(1-open/cross)
# 3:          2268 need_to_contact_author_for_data;_paper_presents_PLI_(1-open/cross)
# 4:          2269 need_to_contact_author_for_data;_paper_presents_PLI_(1-open/cross)
# These represent legitimate conversions to NA (something like "45?" would need checking) 

# Safely transform columns to numeric mode.
# The "NAs introduced by coercion" message refers to the cases mentioned above.
PL_dt[, (cols_to_numeric) := lapply(.SD, as.numeric), .SDcols = cols_to_numeric]


# =============================================================================
# Compute pollen limitation effect sizes (ES = log response ratio).
# No aggregation takes place here, just computing the simple log response ratios.
# =============================================================================

PL_dt[, ":=" # go inside data table environment and start assigning/creating following columns
      (
          # -----------------
          # Fruitset
          # -----------------
          # - Fruitset_Sup
          ES_Fruitset_Sup.VS = log(supp_X_fruitset_JSJEB + 0.5) - log(open_X_fruitset_JSJEB + 0.5),
          # - Fruitset_Bagout
          ES_Fruitset_Bagout.VS = log(bagout_X_fruitset_JSJEB + 0.5) - log(open_X_fruitset_JSJEB + 0.5),
          
          # -----------------
          # Seedset
          # -----------------
          # - Seedset_Sup
          ES_Seedset_Sup.VS = log(supp_X_seeds_per_ovule_Corrected_JR + 0.5) - log(open_X_seeds_per_ovule_Corrected_JR + 0.5),
          # - Seedset_Bagout
          ES_Seedset_Bagout.VS = log(bagout_X_seeds_per_ovule_Corrected_JR + 0.5) - log(open_X_seeds_per_ovule_Corrected_JR + 0.5),
          
          # -----------------
          # SPFR
          # -----------------
          # - SPFR_Sup
          ES_SPFR_Sup.VS = log(supp_X_SPFR_Author + 0.5) - log(open_X_SPFR_Author + 0.5),
          # - SPFR_Bagout
          ES_SPFR_Bagout.VS = log(bagout_X_SPFR_Author + 0.5) - log(open_X_SPFR_Author + 0.5),
          
          # -----------------
          # SPFL
          # -----------------
          # - SPFL_Sup
          ES_SPFL_Sup.VS = log(supp_X_SPFL_Author + 0.5) - log(open_X_SPFL_Author + 0.5),
          # - SPFL_Bagout
          ES_SPFL_Bagout.VS = log(bagout_X_SPFL_Author + 0.5) - log(open_X_SPFL_Author + 0.5),
          
          # -----------------
          # SPP
          # -----------------
          # - SPP_Sup
          ES_SPP_Sup.VS = log(supp_X_SPP_Author + 0.5) - log(open_X_SPP_Author + 0.5),
          # - SPP_Bagout
          ES_SPP_Bagout.VS = log(bagout_X_SPP_Author + 0.5) - log(open_X_SPP_Author + 0.5)
      )]

# =============================================================================
# Choose "Sup" or "Bagout" effect sizes (ES).
# Give priority to "Sup" ES.
# =============================================================================
# If "Sup" ES values are NA, then take "Bagout" ES
# If both "Sup" and "Bagout" ES values are present, then always take "Sup"
# Then, create a character column that specifies what treatment was used - "Sup" or "Bagout"

PL_dt[, ":=" # go inside data table environment and start assigning/creating following columns
      (
          # _____ Fruitset _____ #
          # - choose Sup or Bagout ES value
          ES_Fruitset_Sup_Bagout.VS = ifelse(is.na(ES_Fruitset_Sup.VS), 
                                             ES_Fruitset_Bagout.VS, 
                                             ES_Fruitset_Sup.VS),
          # - specifies what treatment was used - Sup or Bagout
          FS_S.Bo.VS = ifelse(!is.na(ES_Fruitset_Sup.VS), "Sup",
                              ifelse(!is.na(ES_Fruitset_Bagout.VS), "Bagout", NA)),
          
          # _____ Seedset _____ #
          # - choose Sup or Bagout ES value
          ES_Seedset_Sup_Bagout.VS = ifelse(is.na(ES_Seedset_Sup.VS), 
                                            ES_Seedset_Bagout.VS, 
                                            ES_Seedset_Sup.VS),
          # - specifies what treatment was used - Sup or Bagout
          SS_S.Bo.VS = ifelse(!is.na(ES_Seedset_Sup.VS), "Sup",
                              ifelse(!is.na(ES_Seedset_Bagout.VS), "Bagout", NA)),
          
          # _____ SPFR _____ #
          # - choose Sup or Bagout ES value
          ES_SPFR_Sup_Bagout.VS = ifelse(is.na(ES_SPFR_Sup.VS), 
                                         ES_SPFR_Bagout.VS, 
                                         ES_SPFR_Sup.VS),
          # - specifies what treatment was used - Sup or Bagout
          SPFR_S.Bo.VS = ifelse(!is.na(ES_SPFR_Sup.VS), "Sup",
                              ifelse(!is.na(ES_SPFR_Bagout.VS), "Bagout", NA)),
          
          # _____ SPFL _____ #
          # - choose Sup or Bagout ES value
          ES_SPFL_Sup_Bagout.VS = ifelse(is.na(ES_SPFL_Sup.VS), 
                                         ES_SPFL_Bagout.VS, 
                                         ES_SPFL_Sup.VS),
          # - specifies what treatment was used - Sup or Bagout
          SPFL_S.Bo.VS = ifelse(!is.na(ES_SPFL_Sup.VS), "Sup",
                                ifelse(!is.na(ES_SPFL_Bagout.VS), "Bagout", NA)),
          
          # _____ SPP _____ #
          # - choose Sup or Bagout ES value
          ES_SPP_Sup_Bagout.VS = ifelse(is.na(ES_SPP_Sup.VS), 
                                        ES_SPP_Bagout.VS, 
                                        ES_SPP_Sup.VS),
          # - specifies what treatment was used - Sup or Bagout
          SPP_S.Bo.VS = ifelse(!is.na(ES_SPP_Sup.VS), "Sup",
                                ifelse(!is.na(ES_SPP_Bagout.VS), "Bagout", NA))
      )]

# =============================================================================
# Construct the ES master columns
# =============================================================================
PL_dt[, ":=" # go inside data table environment and start assigning/creating (:=) following columns 
      (
          # Create "Master" Effect Size (ES) column
          # fallow the order: SPP, SPFL, SPFR, FS (Fruitset), SO (Seedset)
          ES_mst.VS = ifelse(!is.na(ES_SPP_Sup_Bagout.VS), # if SPP is not NA, then use it
                             ES_SPP_Sup_Bagout.VS,
                             ifelse(!is.na(ES_SPFL_Sup_Bagout.VS), # else, if SPFL is not NA, then use it
                                    ES_SPFL_Sup_Bagout.VS,
                                    ifelse(!is.na(ES_SPFR_Sup_Bagout.VS), # else, if SPFR is not NA, then use it
                                           ES_SPFR_Sup_Bagout.VS,
                                           ifelse(!is.na(ES_Fruitset_Sup_Bagout.VS), # else, if FS is not NA, then use it
                                                  ES_Fruitset_Sup_Bagout.VS,
                                                  ifelse(!is.na(ES_Seedset_Sup_Bagout.VS), # else, if SO is not NA, then use it
                                                         ES_Seedset_Sup_Bagout.VS, NA))))), # else, if all NA, then give NA
          # create a column that gives the corresponding measure name for the value in ES_mst.VS (see above)
          ES_mst_idx.VS = ifelse(!is.na(ES_SPP_Sup_Bagout.VS), 
                                 "SPP",
                                 ifelse(!is.na(ES_SPFL_Sup_Bagout.VS), 
                                        "SPFL",
                                        ifelse(!is.na(ES_SPFR_Sup_Bagout.VS), 
                                               "SPFR",
                                               ifelse(!is.na(ES_Fruitset_Sup_Bagout.VS), 
                                                      "FS",
                                                      ifelse(!is.na(ES_Seedset_Sup_Bagout.VS), 
                                                             "SO", NA))))),
          # create a column that tells if it was Sup or Bagout treatment
          ES_mst_S.Bo.VS = ifelse(!is.na(ES_SPP_Sup_Bagout.VS), 
                                  SPP_S.Bo.VS,
                                  ifelse(!is.na(ES_SPFL_Sup_Bagout.VS), 
                                         SPFL_S.Bo.VS,
                                         ifelse(!is.na(ES_SPFR_Sup_Bagout.VS), 
                                                SPFR_S.Bo.VS,
                                                ifelse(!is.na(ES_Fruitset_Sup_Bagout.VS), 
                                                       FS_S.Bo.VS,
                                                       ifelse(!is.na(ES_Seedset_Sup_Bagout.VS), 
                                                              SS_S.Bo.VS, NA)))))
      )]

# save to csv file
pl_es <- PL_dt[,.(unique_number, ES_mst.VS, ES_mst_idx.VS, ES_mst_S.Bo.VS)]
write.csv(pl_es, file = "Output/PL_ES.csv", row.names = FALSE)
