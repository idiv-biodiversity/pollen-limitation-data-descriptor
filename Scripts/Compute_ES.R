###############################################################################
# Script to compute pollen limitation effect sizes (ES) & variances.
###############################################################################

library(data.table)

# =============================================================================
# Read & prepare data
# =============================================================================
PL_dt <- fread("Data/PL_masters_for publication.csv", colClasses = "character")

PL_dt[,.(unique_number)] # check for empty rows (Excel does this to tables...)
# Remove any empty rows
PL_dt <- PL_dt[unique_number != ""]

# Transform desired columns to numeric type.
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
# Transform columns to numeric mode
# Some NAs are introduced by coercion
PL_dt[, (cols_to_numeric) := lapply(.SD, as.numeric), .SDcols = cols_to_numeric]

# =============================================================================
# Compute pollen limitation effect sizes (ES) & variances 
# For all records, simple means were applied for consistency 
# (weighted means are NOT used as they were applicable to only a few cases).
# Variance formula - adapted from Koricheva et al., 2013 - Handbook of Meta-Analysis, p 64
# =============================================================================
# -----------------
# Fruitset
# -----------------
# - Fruitset_Sup
PL_dt[, ES_Fruitset_Sup.VS := log(supp_X_fruitset_JSJEB + 0.5) - log(open_X_fruitset_JSJEB + 0.5)]
PL_dt[, ES_var.5_Fruitset_Sup.VS := 
          supp_SD_fruitset_JSJEB^2/(supp_N_fruitset_JSJEB * (supp_X_fruitset_JSJEB + 0.5)^2) + 
          open_SD_fruitset_JSJEB^2/(open_N_fruitset_JSJEB * (open_X_fruitset_JSJEB + 0.5)^2)]

# - Fruitset_Bagout
PL_dt[, ES_Fruitset_Bagout.VS := log(bagout_X_fruitset_JSJEB + 0.5) - log(open_X_fruitset_JSJEB + 0.5)]
PL_dt[, ES_var.5_Fruitset_Bagout.VS := 
          bagout_SD_fruitset_JSJEB^2/(bagout_N_fruitset_JSJEB * (bagout_X_fruitset_JSJEB + 0.5)^2) + 
          open_SD_fruitset_JSJEB^2/(open_N_fruitset_JSJEB * (open_X_fruitset_JSJEB + 0.5)^2)]

# -----------------
# Seedset
# -----------------
# - Seedset_Sup
PL_dt[, ES_Seedset_Sup.VS := log(supp_X_seeds_per_ovule_Corrected_JR + 0.5) - log(open_X_seeds_per_ovule_Corrected_JR + 0.5)]
PL_dt[, ES_var.5_Seedset_Sup.VS := 
          supp_SD_seeds_per_ovule_Corrected_JR^2/(supp_N_seeds_per_ovule_Corrected_JR * (supp_X_seeds_per_ovule_Corrected_JR + 0.5)^2) + 
          open_SD_seeds_per_ovule_Corrected_JR^2/(open_N_seeds_per_ovule_Corrected_JR * (open_X_seeds_per_ovule_Corrected_JR + 0.5)^2)]

# - Seedset_Bagout
PL_dt[, ES_Seedset_Bagout.VS := log(bagout_X_seeds_per_ovule_Corrected_JR + 0.5) - log(open_X_seeds_per_ovule_Corrected_JR + 0.5)]
PL_dt[, ES_var.5_Seedset_Bagout.VS := 
          bagout_SD_seeds_per_ovule_Corrected_JR^2/(bagout_N_seeds_per_ovule_Corrected_JR * (bagout_X_seeds_per_ovule_Corrected_JR + 0.5)^2) + 
          open_SD_seeds_per_ovule_Corrected_JR^2/(open_N_seeds_per_ovule_Corrected_JR * (open_X_seeds_per_ovule_Corrected_JR + 0.5)^2)]

# -----------------
# SPFR
# -----------------
# - SPFR_Sup
PL_dt[, ES_SPFR_Sup.VS := log(supp_X_SPFR_Author + 0.5) - log(open_X_SPFR_Author + 0.5)]
PL_dt[, ES_var.5_SPFR_Sup.VS := 
          supp_SD_SPFR_Author^2/(supp_N_SPFR_Author * (supp_X_SPFR_Author + 0.5)^2) + 
          open_SD_SPFR_Author^2/(open_N_SPFR_Author * (open_X_SPFR_Author + 0.5)^2)]

# - SPFR_Bagout
PL_dt[, ES_SPFR_Bagout.VS := log(bagout_X_SPFR_Author + 0.5) - log(open_X_SPFR_Author + 0.5)]
PL_dt[, ES_var.5_SPFR_Bagout.VS := 
          bagout_SD_SPFR_Author^2/(bagout_N_SPFR_Author * (bagout_X_SPFR_Author + 0.5)^2) + 
          open_SD_SPFR_Author^2/(open_N_SPFR_Author * (open_X_SPFR_Author + 0.5)^2)]

# -----------------
# SPFL
# -----------------
# - SPFL_Sup
PL_dt[, ES_SPFL_Sup.VS := log(supp_X_SPFL_Author + 0.5) - log(open_X_SPFL_Author + 0.5)]
PL_dt[, ES_var.5_SPFL_Sup.VS := 
          supp_SD_SPFL_Author^2/(supp_N_SPFL_Author * (supp_X_SPFL_Author + 0.5)^2) + 
          open_SD_SPFL_Author^2/(open_N_SPFL_Author * (open_X_SPFL_Author + 0.5)^2)]

# - SPFL_Bagout
PL_dt[, ES_SPFL_Bagout.VS := log(bagout_X_SPFL_Author + 0.5) - log(open_X_SPFL_Author + 0.5)]
PL_dt[, ES_var.5_SPFL_Bagout.VS := 
          bagout_SD_SPFL_Author^2/(bagout_N_SPFL_Author * (bagout_X_SPFL_Author + 0.5)^2) + 
          open_SD_SPFL_Author^2/(open_N_SPFL_Author * (open_X_SPFL_Author + 0.5)^2)]

# -----------------
# SPP
# -----------------
# - SPP_Sup
PL_dt[, ES_SPP_Sup.VS := log(supp_X_SPP_Author + 0.5) - log(open_X_SPP_Author + 0.5)]
PL_dt[, ES_var.5_SPP_Sup.VS := 
          supp_SD_SPP_Author^2/(supp_N_SPP_Author * (supp_X_SPP_Author + 0.5)^2) + 
          open_SD_SPP_Author^2/(open_N_SPP_Author * (open_X_SPP_Author + 0.5)^2)]

# - SPP_Bagout
PL_dt[, ES_SPP_Bagout.VS := log(bagout_X_SPP_Author + 0.5) - log(open_X_SPP_Author + 0.5)]
PL_dt[, ES_var.5_SPP_Bagout.VS := 
          bagout_SD_SPP_Author^2/(bagout_N_SPP_Author * (bagout_X_SPP_Author + 0.5)^2) + 
          open_SD_SPP_Author^2/(open_N_SPP_Author * (open_X_SPP_Author + 0.5)^2)]

# =============================================================================
# Choose "Sup" or "Bagout" effect sizes (ES) and corresponding variances (ES_var.5_)
# Give priority to "Sup" ES.
# =============================================================================
# If "Sup" ES values are NA, then take "Bagout" ES
# If both "Sup" and "Bagout" ES values are present, then always take "Sup"
# Do the same for variance columns.
# Then, create a character column that specifies what treatment was used - "Sup" or "Bagout"

PL_dt[, ":=" # go inside data table environment and start assigning/creating following columns
      (
          # _____ Fruitset _____ #
          # - choose Sup or Bagout ES value
          ES_Fruitset_Sup_Bagout.VS       = ifelse(is.na(ES_Fruitset_Sup.VS), 
                                                   ES_Fruitset_Bagout.VS, 
                                                   ES_Fruitset_Sup.VS),
          # - choose Sup or Bagout variance
          ES_var.5_Fruitset_Sup_Bagout.VS = ifelse(is.na(ES_Fruitset_Sup.VS), 
                                                   ES_var.5_Fruitset_Bagout.VS, 
                                                   ES_var.5_Fruitset_Sup.VS),
          # - specifies what treatment was used - Sup or Bagout
          FS_S.Bo.VS = ifelse(is.na(ES_Fruitset_Sup.VS) & !is.na(ES_Fruitset_Bagout.VS), "Bagout", 
                              ifelse(!is.na(ES_Fruitset_Sup.VS), "Sup", NA)),
          
          # _____ Seedset _____ #
          # - choose Sup or Bagout ES value
          ES_Seedset_Sup_Bagout.VS       = ifelse(is.na(ES_Seedset_Sup.VS), 
                                                  ES_Seedset_Bagout.VS, 
                                                  ES_Seedset_Sup.VS),
          # - choose Sup or Bagout variance
          ES_var.5_Seedset_Sup_Bagout.VS = ifelse(is.na(ES_Seedset_Sup.VS), 
                                                  ES_var.5_Seedset_Bagout.VS, 
                                                  ES_var.5_Seedset_Sup.VS),
          # - specifies what treatment was used - Sup or Bagout
          SS_S.Bo.VS = ifelse(is.na(ES_Seedset_Sup.VS) & !is.na(ES_Seedset_Bagout.VS), "Bagout", 
                              ifelse(!is.na(ES_Seedset_Sup.VS), "Sup", NA)),
          
          # _____ SPFR _____ #
          # - choose Sup or Bagout ES value
          ES_SPFR_Sup_Bagout.VS       = ifelse(is.na(ES_SPFR_Sup.VS), 
                                               ES_SPFR_Bagout.VS, 
                                               ES_SPFR_Sup.VS),
          # - choose Sup or Bagout variance
          ES_var.5_SPFR_Sup_Bagout.VS = ifelse(is.na(ES_SPFR_Sup.VS), 
                                               ES_var.5_SPFR_Bagout.VS, 
                                               ES_var.5_SPFR_Sup.VS),
          # - specifies what treatment was used - Sup or Bagout
          SPFR_S.Bo.VS = ifelse(is.na(ES_SPFR_Sup.VS) & !is.na(ES_SPFR_Bagout.VS), "Bagout", 
                                ifelse(!is.na(ES_SPFR_Sup.VS), "Sup", NA)),
          
          # _____ SPFL _____ #
          # - choose Sup or Bagout ES value
          ES_SPFL_Sup_Bagout.VS       = ifelse(is.na(ES_SPFL_Sup.VS), 
                                               ES_SPFL_Bagout.VS, 
                                               ES_SPFL_Sup.VS),
          # - choose Sup or Bagout variance
          ES_var.5_SPFL_Sup_Bagout.VS = ifelse(is.na(ES_SPFL_Sup.VS), 
                                               ES_var.5_SPFL_Bagout.VS, 
                                               ES_var.5_SPFL_Sup.VS),
          # - specifies what treatment was used - Sup or Bagout
          SPFL_S.Bo.VS = ifelse(is.na(ES_SPFL_Sup.VS) & !is.na(ES_SPFL_Bagout.VS), "Bagout", 
                                ifelse(!is.na(ES_SPFL_Sup.VS), "Sup", NA)),
          
          # _____ SPP _____ #
          # - choose Sup or Bagout ES value
          ES_SPP_Sup_Bagout.VS       = ifelse(is.na(ES_SPP_Sup.VS), 
                                              ES_SPP_Bagout.VS, 
                                              ES_SPP_Sup.VS),
          # - choose Sup or Bagout variance
          ES_var.5_SPP_Sup_Bagout.VS = ifelse(is.na(ES_SPP_Sup.VS), 
                                              ES_var.5_SPP_Bagout.VS, 
                                              ES_var.5_SPP_Sup.VS),
          # - specifies what treatment was used - Sup or Bagout
          SPP_S.Bo.VS = ifelse(is.na(ES_SPP_Sup.VS) & !is.na(ES_SPP_Bagout.VS), "Bagout", 
                               ifelse(!is.na(ES_SPP_Sup.VS), "Sup", NA))
      )]

# =============================================================================
# Construct the ES master columns
# =============================================================================
PL_dt[, ":=" # go inside data table environment and start assigning/creating (:=) following columns 
      (
          # Create "Master" Effect Size (ES) column
          # fallow the order: SPP, SPFL, SPFR, FS (Fruitset), SS (Seedset)
          ES_mst.VS = ifelse(!is.na(ES_SPP_Sup_Bagout.VS) & !is.na(ES_var.5_SPP_Sup_Bagout.VS), 
                             ES_SPP_Sup_Bagout.VS,
                             ifelse(!is.na(ES_SPFL_Sup_Bagout.VS) & !is.na(ES_var.5_SPFL_Sup_Bagout.VS), 
                                    ES_SPFL_Sup_Bagout.VS,
                                    ifelse(!is.na(ES_SPFR_Sup_Bagout.VS) & !is.na(ES_var.5_SPFR_Sup_Bagout.VS), 
                                           ES_SPFR_Sup_Bagout.VS,
                                           ifelse(!is.na(ES_Fruitset_Sup_Bagout.VS) & !is.na(ES_var.5_Fruitset_Sup_Bagout.VS), 
                                                  ES_Fruitset_Sup_Bagout.VS,
                                                  ifelse(!is.na(ES_Seedset_Sup_Bagout.VS) & !is.na(ES_var.5_Seedset_Sup_Bagout.VS), 
                                                         ES_Seedset_Sup_Bagout.VS, NA))))),
          # Create "master" column for variance
          ES_var.5_mst.VS = ifelse(!is.na(ES_SPP_Sup_Bagout.VS) & !is.na(ES_var.5_SPP_Sup_Bagout.VS), 
                                   ES_var.5_SPP_Sup_Bagout.VS,
                                   ifelse(!is.na(ES_SPFL_Sup_Bagout.VS) & !is.na(ES_var.5_SPFL_Sup_Bagout.VS), 
                                          ES_var.5_SPFL_Sup_Bagout.VS,
                                          ifelse(!is.na(ES_SPFR_Sup_Bagout.VS) & !is.na(ES_var.5_SPFR_Sup_Bagout.VS), 
                                                 ES_var.5_SPFR_Sup_Bagout.VS,
                                                 ifelse(!is.na(ES_Fruitset_Sup_Bagout.VS) & !is.na(ES_var.5_Fruitset_Sup_Bagout.VS), 
                                                        ES_var.5_Fruitset_Sup_Bagout.VS,
                                                        ifelse(!is.na(ES_Seedset_Sup_Bagout.VS) & !is.na(ES_var.5_Seedset_Sup_Bagout.VS), 
                                                               ES_var.5_Seedset_Sup_Bagout.VS, NA))))),
          # create a column that gives the corresponding measure name for the value in ES_mst.VS (see above)
          ES_mst_idx.VS = ifelse(!is.na(ES_SPP_Sup_Bagout.VS) & !is.na(ES_var.5_SPP_Sup_Bagout.VS), 
                                 "SPP",
                                 ifelse(!is.na(ES_SPFL_Sup_Bagout.VS) & !is.na(ES_var.5_SPFL_Sup_Bagout.VS), 
                                        "SPFL",
                                        ifelse(!is.na(ES_SPFR_Sup_Bagout.VS) & !is.na(ES_var.5_SPFR_Sup_Bagout.VS), 
                                               "SPFR",
                                               ifelse(!is.na(ES_Fruitset_Sup_Bagout.VS) & !is.na(ES_var.5_Fruitset_Sup_Bagout.VS), 
                                                      "Fruitset",
                                                      ifelse(!is.na(ES_Seedset_Sup_Bagout.VS) & !is.na(ES_var.5_Seedset_Sup_Bagout.VS), 
                                                             "Seedset", NA))))),
          # create a column that tells if it was Sup or Bagout treatment
          ES_mst_S.Bo.VS = ifelse(!is.na(ES_SPP_Sup_Bagout.VS) & !is.na(ES_var.5_SPP_Sup_Bagout.VS), 
                                  SPP_S.Bo.VS,
                                  ifelse(!is.na(ES_SPFL_Sup_Bagout.VS) & !is.na(ES_var.5_SPFL_Sup_Bagout.VS), 
                                         SPFL_S.Bo.VS,
                                         ifelse(!is.na(ES_SPFR_Sup_Bagout.VS) & !is.na(ES_var.5_SPFR_Sup_Bagout.VS), 
                                                SPFR_S.Bo.VS,
                                                ifelse(!is.na(ES_Fruitset_Sup_Bagout.VS) & !is.na(ES_var.5_Fruitset_Sup_Bagout.VS), 
                                                       FS_S.Bo.VS,
                                                       ifelse(!is.na(ES_Seedset_Sup_Bagout.VS) & !is.na(ES_var.5_Seedset_Sup_Bagout.VS), 
                                                              SS_S.Bo.VS, NA)))))
      )]

# save to csv file
PL_dt[, idx.VS := 1:.N]
write.csv(PL_dt, file = "Output/PL_masters_for_publication_with_ES_cols.csv", row.names = FALSE)
