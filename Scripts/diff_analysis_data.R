###############################################################################
# Script to apply some corrections on the row data used for graphs and for submitting.
# There were mismatches between the raw data assumed cleaned for submission and the data for analysis.
# It seems that the data for analysis got updated manually while the row data not.
# This scripts attempts to find mismatches between the two version of the data and align them.
# The code might be a bit convoluted because column names were changed through time.
###############################################################################

library(data.table)

# =============================================================================
# Read & prepare data
# =============================================================================
# Read PL row data. This is the dataset Joanne wants for submission.
# Treat all values as character. Also convert to NA everything that is:
# "NA", "N/A", "null", "" (last one is treated as blank in Excel).
pl_glo <- fread("Data/GloPL_with_id.csv", 
                colClasses = "character", 
                na.strings = c("NA","N/A","null", ""))
# Check for empty rows (Excel does this to tables...)
pl_glo[is.na(unique_number)]

# This is the updated data Joanne wants to use for analysis
pl_a <- fread("Data/PL_ANALYSIS_25_01_2018.csv", 
              colClasses = "character",
              na.strings = c("NA","N/A","null", ""))
# Check for empty rows (Excel does this to tables...)
pl_a[is.na(unique_number)]

# -----------------------------------------------------------------------------
# Read meta_data (all columns as character)
# -----------------------------------------------------------------------------
# The metadata file contains a lookup table for correspondence between older and newer column names.
meta_dt <- fread(file = "Data/for_merging/Meta_data_24_01.csv", 
                 colClasses = "character")
# Delete any quotation symbol from column names 
# (the actual columns of the metadata table)
data.table::setnames(meta_dt, gsub("'", "", names(meta_dt)))
# Replace spaces with dots in column names
data.table::setnames(meta_dt, gsub(" ", ".", names(meta_dt)))
# Create a helper idex
meta_dt[, row_idx_meta := 1:.N]

# =============================================================================
# Detect difference between the two PL datasets
# =============================================================================
old_names <- meta_dt$Current.Column.Name
new_names <- meta_dt$Variable.within.GloPL.database

# Switch names
setnames(pl_glo, 
         old = new_names, 
         new = old_names)

# Vector of columns to check for differences
cols2check <- old_names[old_names %in% names(pl_a)]
cols2check <- c("unique_number", cols2check)

pl_a_check   <- copy(pl_a[, cols2check, with = FALSE]) # analysis dataset
pl_glo_check <- copy(pl_glo[, cols2check, with = FALSE]) # submission dataset

# Rename columns (add suffix) in the two datasets for easy differentiation
setnames(pl_a_check, paste0(names(pl_a_check), "__PL_ANLS"))
setnames(pl_glo_check, paste0(names(pl_glo_check), "__GloPL"))

# Merge the two datasets by unique_number column
pl_check <- merge(x = pl_a_check,
                  y = pl_glo_check,
                  by.x = "unique_number__PL_ANLS",
                  by.y = "unique_number__GloPL",
                  all = TRUE,
                  sort = FALSE)

setnames(pl_check, old = "unique_number__PL_ANLS", new = "unique_number")

# Update vector of columns to check for differences
cols2check_2 <- old_names[old_names %in% names(pl_a)]
cols2check_2 <- cols2check_2[! cols2check_2 %in% c("Author", 
                                                   "Year",
                                                   "level_of_treatment2_WD",
                                                   "Treatment_identity_TK")]

# Use expression evaluation to catch differences.
# Below is an example hot this works for a simple case. 
# The script will run such cases in a loop (for each cols2check_2)
# pl_check[supp_X_fruitset_JSJEB__PL_ANLS != supp_X_fruitset_JSJEB__GloPL, 
#          .(unique_number, 
#            supp_X_fruitset_JSJEB__PL_ANLS,
#            supp_X_fruitset_JSJEB__GloPL)]
# 
# eval(parse(text = 
#                "pl_check[supp_X_fruitset_JSJEB__PL_ANLS != supp_X_fruitset_JSJEB__GloPL, 
#            .(unique_number, 
#            supp_X_fruitset_JSJEB__PL_ANLS,
#            supp_X_fruitset_JSJEB__GloPL)]"))

cols__PL_ANLS <- paste0(cols2check_2, "__PL_ANLS")
cols__GloPL <- paste0(cols2check_2, "__GloPL")
# Operations to be passed to eval(parse())
operations <- paste0("pl_check[", cols__PL_ANLS, " != ", cols__GloPL, 
                     ", .(unique_number, ", cols__PL_ANLS, ", ", cols__GloPL, ")]")

my_lst <- vector(mode = "list", length = length(operations))
for (i in 1:length(operations)){
    dt <- eval(parse(text = operations[i]))
    my_lst[[i]] <- if (nrow(dt)) rbindlist(list(data.table(t(names(dt))), dt))
}

# Bind all differences in a single table
diff <- rbindlist(my_lst)
setnames(diff, c("unique_number", "col_PL_ANALYSIS", "col_GloPL"))

write.csv(diff, file = "output/differences_PL_ANLS_GloPL.csv", row.names = FALSE)

# =============================================================================
# Update values by reference. 
# =============================================================================
# These are updates based on the file differences_PL_ANLS_GloPL_Joanne.xlsx
# updates are happening in the submission dataset only.
pl_glo[unique_number == "462",  open_N_fruitset_JSJEB := "8"]
pl_glo[unique_number == "463",  open_N_fruitset_JSJEB := "8"]
pl_glo[unique_number == "1495", open_N_fruitset_JSJEB := "2261"]

pl_glo[unique_number == "462",  open_SD_fruitset_JSJEB := "0.056568542"]
pl_glo[unique_number == "463",  open_SD_fruitset_JSJEB := "0.062225397"]
pl_glo[unique_number == "1495", open_SD_fruitset_JSJEB := "1.901998948"]

pl_glo[unique_number == "1495", supp_N_fruitset_JSJEB := "215"]

pl_glo[unique_number == "1495", supp_SD_fruitset_JSJEB := "1.026401481"]

pl_glo[unique_number == "462",  bagout_N_fruitset_JSJEB := "8"]
pl_glo[unique_number == "463",  bagout_N_fruitset_JSJEB := "8"]
pl_glo[unique_number == "1495", bagout_N_fruitset_JSJEB := "338"]

pl_glo[unique_number == "1205", bagout_X_fruitset_JSJEB := "0.6422"]

pl_glo[unique_number == "462",  bagout_SD_fruitset_JSJEB := "0.141421356"]
pl_glo[unique_number == "463",  bagout_SD_fruitset_JSJEB := "0.254558441"]
pl_glo[unique_number == "884",  bagout_SD_fruitset_JSJEB := "0.7978"]
pl_glo[unique_number == "1495", bagout_SD_fruitset_JSJEB := "1.286934342"]

pl_glo[unique_number == "1078", open_N_seeds_per_ovule_Corrected_JR := "100"]
pl_glo[unique_number == "2496", open_N_seeds_per_ovule_Corrected_JR := "93"]

# Switch names back
setnames(pl_glo, 
         old = old_names, 
         new = new_names)

# Save updated dataset for further use
write.csv(pl_glo, file = "output/GloPL_with_id_updated.csv", row.names = FALSE)
