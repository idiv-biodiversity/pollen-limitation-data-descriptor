# /////////////////////////////////////////////////////////////////////////
#
# Script to compute pollen limitation effect sizes (ES) with various methods. It
# saves results in the  intermediary master_es_multi_methods.csv file that is
# used further in Prepare_data.R to updated the GloPL with the desired ES
# columns.
#
# /////////////////////////////////////////////////////////////////////////


library(data.table)

# Cleans environment, including hidden objects (which begin with a .)
rm(list = ls(all.names = TRUE))

# Source functions that apply ES formulae.
source("scripts/helper_functions/ES_functions.R")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read & prepare data -----------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read pollen limitation (GloPL) raw data. Treat all values as character.
# Appropriate data type conversions will be done next.
GloPL <- fread("Data/GloPL_with_id.csv", colClasses = "character")


# Transform desired columns to numeric type. 
# These are the columns that are used for ES computations.
cols_to_numeric_4ES <- 
    c(
        "Sup_X_FS",
        "Sup_SD_FS",
        "Sup_N_FS",
        "Natural_X_FS",
        "Natural_SD_FS",
        "Natural_N_FS",
        "Bagout_X_FS",
        "Bagout_SD_FS",
        "Bagout_N_FS",
        "Sup_X_SO",
        "Sup_SD_SO",
        "Sup_N_SO",
        "Natural_X_SO",
        "Natural_SD_SO",
        "Natural_N_SO",
        "Bagout_X_SO",
        "Bagout_SD_SO",
        "Bagout_N_SO",
        "Sup_X_SPFR",
        "Sup_SD_SPFR",
        "Sup_N_SPFR",
        "Natural_X_SPFR",
        "Natural_SD_SPFR",
        "Natural_N_SPFR",
        "Bagout_X_SPFR",
        "Bagout_SD_SPFR",
        "Bagout_N_SPFR",
        "Sup_X_SPFL",
        "Sup_SD_SPFL",
        "Sup_N_SPFL",
        "Natural_X_SPFL",
        "Natural_SD_SPFL",
        "Natural_N_SPFL",
        "Bagout_X_SPFL",
        "Bagout_SD_SPFL",
        "Bagout_N_SPFL",
        "Sup_X_SPP",
        "Sup_SD_SPP",
        "Sup_N_SPP",
        "Natural_X_SPP",
        "Natural_SD_SPP",
        "Natural_N_SPP",
        "Bagout_X_SPP",
        "Bagout_SD_SPP",
        'Bagout_N_SPP'
    )

# Check cases where conversion to numeric encounters characters that yield NA-s.
# This helps to check for legitimate conversions. Search across columns and
# display the unique_number values together with the non-numeric values that
# yield NA-s.
test <- GloPL[, lapply(.SD, function(col) is.na(as.numeric(col)) != is.na(col)), 
              .SDcols = cols_to_numeric_4ES]
for (col in cols_to_numeric_4ES[ test[, colSums(.SD) > 0] ]){
    print(col)
    print(GloPL[test[, get(col)], .(unique_number,  get(col))])
}
# [1] "Natural_N_FS"
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
GloPL[, (cols_to_numeric_4ES) := lapply(.SD, as.numeric), .SDcols = cols_to_numeric_4ES]


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
dt <- GloPL[, cols_to_numeric_4ES, with = FALSE]
# For each combination, compute ES and store results in a list of data tables.
# E.g. – the ES results for Hedge’s d will be stored in the list at
# dt_list$hedgesD
for (i in 1:nrow(mk)){
    dt[, ":=" ( # create the following ES columns for:
        # _____ Fruitset _____ #
        # - Fruitset_Sup
        ES_Fruitset_Sup.VS = get_ES(Sup_X_FS,  Natural_X_FS,
                                    Sup_N_FS,  Natural_N_FS,
                                    Sup_SD_FS, Natural_SD_FS,
                                    k = mk[i, k], method =  mk[i, m]),
        # - Fruitset_Bagout
        ES_Fruitset_Bagout.VS = get_ES(Bagout_X_FS,  Natural_X_FS, 
                                       Bagout_N_FS,  Natural_N_FS,
                                       Bagout_SD_FS, Natural_SD_FS,
                                       k = mk[i, k], method =  mk[i, m]),
        # _____ Seedset/Seeds per ovule _____ #
        # - Seedset_Sup
        ES_Seedset_Sup.VS = get_ES(Sup_X_SO,  Natural_X_SO, 
                                   Sup_N_SO,  Natural_N_SO,
                                   Sup_SD_SO, Natural_SD_SO,
                                   k = mk[i, k], method =  mk[i, m]),
        # - Seedset_Bagout
        ES_Seedset_Bagout.VS = get_ES(Bagout_X_SO,  Natural_X_SO, 
                                      Bagout_N_SO,  Natural_N_SO,
                                      Bagout_SD_SO, Natural_SD_SO,
                                      k = mk[i, k], method =  mk[i, m]),
        # _____ SPFR _____ #
        # - SPFR_Sup
        ES_SPFR_Sup.VS = get_ES(Sup_X_SPFR,  Natural_X_SPFR, 
                                Sup_N_SPFR,  Natural_N_SPFR,
                                Sup_SD_SPFR, Natural_SD_SPFR,
                                k = mk[i, k], method =  mk[i, m]),
        # - SPFR_Bagout
        ES_SPFR_Bagout.VS = get_ES(Bagout_X_SPFR,  Natural_X_SPFR, 
                                   Bagout_N_SPFR,  Natural_N_SPFR,
                                   Bagout_SD_SPFR, Natural_SD_SPFR,
                                   k = mk[i, k], method =  mk[i, m]),
        # _____ SPFL _____ #
        # - SPFL_Sup
        ES_SPFL_Sup.VS = get_ES(Sup_X_SPFL,  Natural_X_SPFL, 
                                Sup_N_SPFL,  Natural_N_SPFL,
                                Sup_SD_SPFL, Natural_SD_SPFL,
                                k = mk[i, k], method =  mk[i, m]),
        # - SPFL_Bagout
        ES_SPFL_Bagout.VS = get_ES(Bagout_X_SPFL,  Natural_X_SPFL, 
                                   Bagout_N_SPFL,  Natural_N_SPFL,
                                   Bagout_SD_SPFL, Natural_SD_SPFL,
                                   k = mk[i, k], method =  mk[i, m]),
        # _____ SPP _____ #
        # - SPP_Sup
        ES_SPP_Sup.VS = get_ES(Sup_X_SPP,  Natural_X_SPP, 
                               Sup_N_SPP,  Natural_N_SPP,
                               Sup_SD_SPP, Natural_SD_SPP,
                               k = mk[i, k], method =  mk[i, m]),
        # - SPP_Bagout
        ES_SPP_Bagout.VS = get_ES(Bagout_X_SPP,  Natural_X_SPP, 
                                  Bagout_N_SPP,  Natural_N_SPP,
                                  Bagout_SD_SPP, Natural_SD_SPP,
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
ES_dt <- cbind(ES_dt, GloPL[,.(unique_number)])

write.csv(ES_dt, 
          file = "Output/cache/master_es_multi_methods.csv", 
          row.names = FALSE)
