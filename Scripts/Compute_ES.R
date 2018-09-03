# /////////////////////////////////////////////////////////////////////////
# Script to compute pollen limitation effect sizes (ES) with various methods.
# Note that there is the need of switching between two versions of columns
# names. There is a metadata file that contains a lookup table with the
# correspondence between the column names.
# /////////////////////////////////////////////////////////////////////////

rm(list = ls(all.names = TRUE)); gc(reset = TRUE)

library(data.table)

# Source functions that apply ES formulae.
source("scripts/helper_functions/ES_functions.R")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read & prepare data -----------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read pollen limitation (PL) raw data. Treat all values as character. 
# Also convert to NA everything that is:
# "NA", "N/A", "null", "" (the last one is treated as blank in Excel).
PL_dt <- fread("Data/GloPL_with_id_2_2_2018.csv", 
               colClasses = "character", 
               na.strings = c("NA","N/A","null", ""))

# There is the need of switching between two versions of columns names because a
# new set of names was used for the final dataset. A lookup table can be used
# from the metadata file below.

# Read meta_data (all columns as character)
meta_dt <- fread(file = "Data/Meta_data_24_01.csv", 
                 colClasses = "character")
# Delete any quotation symbol from column names
setnames(meta_dt, gsub("'", "", names(meta_dt)))
# Replace spaces with dots in column names
setnames(meta_dt, gsub(" ", ".", names(meta_dt)))

old_names <- meta_dt$Current.Column.Name
new_names <- meta_dt$Variable.within.GloPL.database # names as will be published

# Check if new_names can be found within the current column names; 
# check differences.
setdiff(names(PL_dt), new_names)
# "unique_number", "unique_study_number", "Species_Author" are extra in PL_dt,
# which is ok
setdiff(new_names, names(PL_dt)) # all good if "character(0)"

# Switch names
setnames(PL_dt, 
         old = new_names, 
         new = old_names)
rm(meta_dt, new_names, old_names)


# Transform desired columns to numeric type. 
# These are the columns that are used for ES computations.
cols_to_numeric_4ES <- 
    c(
        "supp_X_fruitset_JSJEB",
        "supp_SD_fruitset_JSJEB",
        "supp_N_fruitset_JSJEB",
        "open_X_fruitset_JSJEB",
        "open_SD_fruitset_JSJEB",
        "open_N_fruitset_JSJEB",
        "bagout_X_fruitset_JSJEB",
        "bagout_SD_fruitset_JSJEB",
        "bagout_N_fruitset_JSJEB",
        "supp_X_seeds_per_ovule_Corrected_JR",
        "supp_SD_seeds_per_ovule_Corrected_JR",
        "supp_N_seeds_per_ovule_Corrected_JR",
        "open_X_seeds_per_ovule_Corrected_JR",
        "open_SD_seeds_per_ovule_Corrected_JR",
        "open_N_seeds_per_ovule_Corrected_JR",
        "bagout_X_seeds_per_ovule_Corrected_JR",
        "bagout_SD_seeds_per_ovule_Corrected_JR",
        "bagout_N_seeds_per_ovule_Corrected_JR",
        "supp_X_SPFR_Author",
        "supp_SD_SPFR_Author",
        "supp_N_SPFR_Author",
        "open_X_SPFR_Author",
        "open_SD_SPFR_Author",
        "open_N_SPFR_Author",
        "bagout_X_SPFR_Author",
        "bagout_SD_SPFR_Author",
        "bagout_N_SPFR_Author",
        "supp_X_SPFL_Author",
        "supp_SD_SPFL_Author",
        "supp_N_SPFL_Author",
        "open_X_SPFL_Author",
        "open_SD_SPFL_Author",
        "open_N_SPFL_Author",
        "bagout_X_SPFL_Author",
        "bagout_SD_SPFL_Author",
        "bagout_N_SPFL_Author",
        "supp_X_SPP_Author",
        "supp_SD_SPP_Author",
        "supp_N_SPP_Author",
        "open_X_SPP_Author",
        "open_SD_SPP_Author",
        "open_N_SPP_Author",
        "bagout_X_SPP_Author",
        "bagout_SD_SPP_Author",
        'bagout_N_SPP_Author'
    )

# Check cases where conversion to numeric encounters characters that yield NA-s.
# This helps to check for legitimate conversions. Search across columns and
# display the unique_number values together with the non-numeric values that
# yield NA-s.
test <- PL_dt[, lapply(.SD, function(col) is.na(as.numeric(col)) != is.na(col)), 
              .SDcols = cols_to_numeric_4ES]
for (col in cols_to_numeric_4ES[ test[, colSums(.SD) > 0] ]){
    print(col)
    print(PL_dt[test[, get(col)], .(unique_number,  get(col))])
}
# [1] "open_N_fruitset_JSJEB"
#    unique_number                                                                 V2
# 1:          2264 need_to_contact_author_for_data;_paper_presents_PLI_(1-open/cross)
# 2:          2265 need_to_contact_author_for_data;_paper_presents_PLI_(1-open/cross)
# 3:          2268 need_to_contact_author_for_data;_paper_presents_PLI_(1-open/cross)
# 4:          2269 need_to_contact_author_for_data;_paper_presents_PLI_(1-open/cross)
#
# These represent legitimate conversions to NA

rm(test, col) # remove objects

# Safely transform columns to numeric mode.
# The "NAs introduced by coercion" message refers to the cases mentioned above.
PL_dt[, (cols_to_numeric_4ES) := lapply(.SD, as.numeric), .SDcols = cols_to_numeric_4ES]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute ES --------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute pollen limitation effect sizes. Uses the get_ES() helper function.
# No aggregation takes place here, just computing the log response ratios.

# Constant k to add to the means in the log response ratio (ES) formula.
k_vect <- c(.01, .05, .1, .5)

# Prepare the combination of methods and constants used for ES computation.
mk <- as.data.table(rbind(expand.grid(m = c("lnRktoall", "lnRkto0"), 
                                      k = k_vect),
                          expand.grid(m = c("lnR", "hedgesD"),
                                      k = 0)))
mk[, m := as.character(m)]
mk[, names := ifelse(k != 0, paste(m, k, sep = "_"), m)]
mk

# Define a list that will contain tables with ES results for each combination of
# methods and constants.
dt_list <- vector(mode = "list", length = nrow(mk))
names(dt_list) <- mk$names

# Make a copy of the columns that enter the ES computation
dt <- PL_dt[, cols_to_numeric_4ES, with = FALSE]
# For each combination, compute ES and store results in a list of data tables.
# E.g. – the ES results for Hedge’s d will be stored in the list at
# dt_list$hedgesD
for (i in 1:nrow(mk)){
    dt[, ":=" ( # create the following ES columns for:
        # _____ Fruitset _____ #
        # - Fruitset_Sup
        ES_Fruitset_Sup.VS = get_ES(supp_X_fruitset_JSJEB,  open_X_fruitset_JSJEB,
                                    supp_N_fruitset_JSJEB,  open_N_fruitset_JSJEB,
                                    supp_SD_fruitset_JSJEB, open_SD_fruitset_JSJEB,
                                    k = mk[i, k], method =  mk[i, m]),
        # - Fruitset_Bagout
        ES_Fruitset_Bagout.VS = get_ES(bagout_X_fruitset_JSJEB,  open_X_fruitset_JSJEB, 
                                       bagout_N_fruitset_JSJEB,  open_N_fruitset_JSJEB,
                                       bagout_SD_fruitset_JSJEB, open_SD_fruitset_JSJEB,
                                       k = mk[i, k], method =  mk[i, m]),
        # _____ Seedset/Seeds per ovule _____ #
        # - Seedset_Sup
        ES_Seedset_Sup.VS = get_ES(supp_X_seeds_per_ovule_Corrected_JR,  open_X_seeds_per_ovule_Corrected_JR, 
                                   supp_N_seeds_per_ovule_Corrected_JR,  open_N_seeds_per_ovule_Corrected_JR,
                                   supp_SD_seeds_per_ovule_Corrected_JR, open_SD_seeds_per_ovule_Corrected_JR,
                                   k = mk[i, k], method =  mk[i, m]),
        # - Seedset_Bagout
        ES_Seedset_Bagout.VS = get_ES(bagout_X_seeds_per_ovule_Corrected_JR,  open_X_seeds_per_ovule_Corrected_JR, 
                                      bagout_N_seeds_per_ovule_Corrected_JR,  open_N_seeds_per_ovule_Corrected_JR,
                                      bagout_SD_seeds_per_ovule_Corrected_JR, open_SD_seeds_per_ovule_Corrected_JR,
                                      k = mk[i, k], method =  mk[i, m]),
        # _____ SPFR _____ #
        # - SPFR_Sup
        ES_SPFR_Sup.VS = get_ES(supp_X_SPFR_Author,  open_X_SPFR_Author, 
                                supp_N_SPFR_Author,  open_N_SPFR_Author,
                                supp_SD_SPFR_Author, open_SD_SPFR_Author,
                                k = mk[i, k], method =  mk[i, m]),
        # - SPFR_Bagout
        ES_SPFR_Bagout.VS = get_ES(bagout_X_SPFR_Author,  open_X_SPFR_Author, 
                                   bagout_N_SPFR_Author,  open_N_SPFR_Author,
                                   bagout_SD_SPFR_Author, open_SD_SPFR_Author,
                                   k = mk[i, k], method =  mk[i, m]),
        # _____ SPFL _____ #
        # - SPFL_Sup
        ES_SPFL_Sup.VS = get_ES(supp_X_SPFL_Author,  open_X_SPFL_Author, 
                                supp_N_SPFL_Author,  open_N_SPFL_Author,
                                supp_SD_SPFL_Author, open_SD_SPFL_Author,
                                k = mk[i, k], method =  mk[i, m]),
        # - SPFL_Bagout
        ES_SPFL_Bagout.VS = get_ES(bagout_X_SPFL_Author,  open_X_SPFL_Author, 
                                   bagout_N_SPFL_Author,  open_N_SPFL_Author,
                                   bagout_SD_SPFL_Author, open_SD_SPFL_Author,
                                   k = mk[i, k], method =  mk[i, m]),
        # _____ SPP _____ #
        # - SPP_Sup
        ES_SPP_Sup.VS = get_ES(supp_X_SPP_Author,  open_X_SPP_Author, 
                               supp_N_SPP_Author,  open_N_SPP_Author,
                               supp_SD_SPP_Author, open_SD_SPP_Author,
                               k = mk[i, k], method =  mk[i, m]),
        # - SPP_Bagout
        ES_SPP_Bagout.VS = get_ES(bagout_X_SPP_Author,  open_X_SPP_Author, 
                                  bagout_N_SPP_Author,  open_N_SPP_Author,
                                  bagout_SD_SPP_Author, open_SD_SPP_Author,
                                  k = mk[i, k], method =  mk[i, m])
    )]
    # For each table obtained at each iteration, keep only the ES columns
    dt_list[[i]] <- dt[, names(dt) %like% "ES_", with = FALSE]
}
rm(dt, i, k_vect, mk)

# E.g. - the ES results for Hedge’s d are stored in the list at dt_list$hedgesD
# str(dt_list$hedgesD)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sup vs Bagout ES --------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "Sup"    = unbagged, supplemental hand pollination treatment,
# "Bagout" = bagged, outcrossed hand pollination treatment.
# Choose "Sup" or "Bagout" effect sizes (ES). Give priority to "Sup" ES. That
# is, if "Sup" ES values are NA, then take "Bagout" ES. If both "Sup" and
# "Bagout" ES values are present, then take "Sup". Create a character column
# that specifies what treatment was used - "Sup" or "Bagout".

# In each data.table from dt_list compute the following columns:
for (i in 1:length(dt_list)){
    dt_list[[i]][, ":=" (
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
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Construct ES "master" columns -------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# In each data.table from dt_list compute the following columns:
for (i in 1:length(dt_list)){
    dt_list[[i]][, ":=" (
        # Create "Master" Effect Size (ES) column.
        # Fallow the order: SPP, SPFL, SPFR, FS (Fruitset), SO (Seedset/Seeds per ovule).
        PL_Effect_Size = 
            ifelse(
                !is.na(ES_SPP_Sup_Bagout.VS),
                # if SPP is not NA, then use it
                yes = ES_SPP_Sup_Bagout.VS,
                no  = ifelse(
                    !is.na(ES_SPFL_Sup_Bagout.VS),
                    # else, if SPFL is not NA, then use it
                    yes = ES_SPFL_Sup_Bagout.VS,
                    no  = ifelse(
                        !is.na(ES_SPFR_Sup_Bagout.VS),
                        # else, if SPFR is not NA, then use it
                        yes = ES_SPFR_Sup_Bagout.VS,
                        no  = ifelse(
                            !is.na(ES_Fruitset_Sup_Bagout.VS),
                            # else, if FS is not NA, then use it
                            yes = ES_Fruitset_Sup_Bagout.VS,
                            no  = ifelse(
                                !is.na(ES_Seedset_Sup_Bagout.VS),
                                # else, if SO is not NA, then use it
                                yes = ES_Seedset_Sup_Bagout.VS,
                                # else, if all NA, then give NA
                                no  = NA
                            )
                        )
                    )
                )
            ),
        # Create a column that gives the corresponding measure name 
        # for the value in ES_mst.VS above.
        PL_Effect_Size_Type1 = 
            ifelse(
                !is.na(ES_SPP_Sup_Bagout.VS),
                yes = "SPP",
                no  = ifelse(
                    !is.na(ES_SPFL_Sup_Bagout.VS),
                    yes = "SPFL",
                    no  = ifelse(
                        !is.na(ES_SPFR_Sup_Bagout.VS),
                        yes = "SPFR",
                        no  = ifelse(
                            !is.na(ES_Fruitset_Sup_Bagout.VS),
                            yes = "FS",
                            no  = ifelse(
                                !is.na(ES_Seedset_Sup_Bagout.VS),
                                yes = "SO", 
                                no  = NA)
                        )
                    )
                )
            ),
        # Create a column that tells if it was Sup or Bagout treatment
        PL_Effect_Size_Type2 = 
            ifelse(
                !is.na(ES_SPP_Sup_Bagout.VS),
                yes = SPP_S.Bo.VS,
                no  = ifelse(
                    !is.na(ES_SPFL_Sup_Bagout.VS),
                    yes = SPFL_S.Bo.VS,
                    no  = ifelse(
                        !is.na(ES_SPFR_Sup_Bagout.VS),
                        yes = SPFR_S.Bo.VS,
                        no  = ifelse(
                            !is.na(ES_Fruitset_Sup_Bagout.VS),
                            yes = FS_S.Bo.VS,
                            no  = ifelse(
                                !is.na(ES_Seedset_Sup_Bagout.VS),
                                yes = SS_S.Bo.VS, 
                                no  = NA)
                        )
                    )
                )
            )
    )]
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Results preparation -----------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save a table with "master" ES columns from different combination of methods
# and constants.

cols2keep_master <- c("PL_Effect_Size",
                      "PL_Effect_Size_Type1",
                      "PL_Effect_Size_Type2")

for (i in 1:length(dt_list)) {
    # From each table delete all other columns different from those in cols2keep_master
    dt_list[[i]][, setdiff(names(dt_list[[i]]), cols2keep_master) := NULL]
    # Add a suffix with the combination of method and constant to column names.
    setnames( dt_list[[i]],
              paste(names(dt_list[[i]]),
                    names(dt_list)[i],
                    sep = "_") )
}

# cbind ES tables
# Must remove list names, otherwise they are passed in the column names.
names(dt_list) <- NULL 
ES_dt <- do.call("cbind", dt_list)
# Add also the unique_number column (helpful for backtracking) 
ES_dt <- cbind(ES_dt, PL_dt[,.(unique_number)])

write.csv(ES_dt, 
          file = "Output/cache/master_es_multi_methods.csv", 
          row.names = FALSE)
