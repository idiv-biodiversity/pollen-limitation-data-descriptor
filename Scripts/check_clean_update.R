###############################################################################
## Script to attempt catching typos & artefacts and updates ES columns.
## Outputs a CSV file with cleaned values for submission.
###############################################################################

library(data.table)

# =============================================================================
# Read & prepare data
# =============================================================================
# Read PL data. Treat all values as character. 
# Also convert to NA everything that is:
# "NA", "N/A", "null", "" (last one is treated as blank in Excel).
pl_dt <- fread("output/GloPL_with_id_updated.csv", 
               colClasses = "character", 
               na.strings = c("NA","N/A","null", ""))

# Read PL effect size computed with the script Compute_ES.R
pl_es <- fread("output/PL_ES.csv", colClasses = "character")

# -----------------------------------------------------------------------------
# Data checking & edits in species names.
# Species_Author & Species_accepted_names columns.
# -----------------------------------------------------------------------------
# Check for all names that may contain punctuation other than ._- symbols
pl_dt[grepl(pattern = "[^[:alnum:][:space:]._-]", x = Species_Author), .(unique_number, Species_Author)]
# unique_number              Species_Author
# 1:           877 Salix_lantana_(syn._Lanata) # this seems ok
# 2:          3006  ?hododendron_siderophyllum
# Apply the correction
pl_dt[unique_number == "3006", Species_Author := "Rhododendron_siderophyllum"]

# Check for all names that may contain "_" character at least 1 times at the end of the name
pl_dt[grepl(pattern = "_{1,}$", x = Species_Author), .(unique_number, Species_Author)]
# unique_number          Species_Author
# 1:           440      Swartzia_pickelii_
# 2:           792      Metrodorea_nigra__
# 3:          1467 Psychotria_tenuinervis_
# 4:          1870       Walsura_trifolia_
# 5:          1929            Iris_ensata_
# 6:          1977          Machilus_kobu_
# 7:          2001       Solanum_nigrum___
# 8:          2004  Symplocos_pergracilis_
# 9:          2347  Begonia_fimbristipula_
# 10:          2408      Iris_atropurpurea_
# 11:          3379        Celtis_africana_
# Delete the extra "_" character(s)
pl_dt[grepl(pattern = "_{1,}$", x = Species_Author), # indexing is optional
      Species_Author := gsub(pattern = "_{1,}$", 
                             replacement = "", 
                             x = Species_Author)]
# check 
pl_dt[unique_number == "440", Species_Author]
# [1] "Swartzia_pickelii"

# Check for all names that may contain "_" character at least 2 times
pl_dt[grepl(pattern = "_{2,}", x = Species_Author), .(unique_number, Species_Author)]
# unique_number                           Species_Author
# 1:        2323.1 Primulina__glandulosa_var._yangshuoensis
# 2:          3047                      Vriesea__ensiformis
# Delete the extra "_" (keep only one)
pl_dt[grepl(pattern = "_{2,}", x = Species_Author), # indexing is optional
      Species_Author := gsub(pattern = "_{2,}", 
                             replacement = "_", 
                             x = Species_Author)]
# check 
pl_dt[unique_number == "2323.1", Species_Author]

# Checking Species_accepted_names
# Check for all names that may contain punctuation other than ._- symbols
pl_dt[grepl(pattern = "[^[:alnum:][:space:]._-]", x = Species_accepted_names), 
      .(unique_number, Species_accepted_names)]
# Check for extra "_" (at the end of the string or within the string)
pl_dt[grepl(pattern = "_{1,}$|_{2,}", x = Species_accepted_names), 
      .(unique_number, Species_accepted_names)]
# 0 rows detected - looks ok

# -----------------------------------------------------------------------------
# Check if author, DOI and year match with the ones from citations file
# -----------------------------------------------------------------------------
# Read citations data (all columns as character)
citations_dt <- fread(file = "Data/for_merging/Citations_25_01.csv",
                      colClasses = "character",
                      na.strings = c("NA","N/A","null", ""))
# Replace spaces with dots in column names
data.table::setnames(citations_dt, gsub(" ", ".", names(citations_dt)))
citations_dt[, row_idx_citations := 1:.N]

# Rename columns to easily identity them from original ones
setnames(citations_dt, 
         old = c("Author", "DOI", "year"),
         new = c("Author_CitationsFile", "DOI_CitationsFile", "Year_CitationsFile"))

# Run a merging test to check mismatches
test_citation <- merge(x = pl_dt[,.(unique_number, unique_study_number, 
                                    Author, DOI, Year)],
                       y = citations_dt[,.(Unquic.study.Number, Author_CitationsFile, 
                                           DOI_CitationsFile, Year_CitationsFile)],
                       by.x = "unique_study_number",
                       by.y = "Unquic.study.Number",
                       all = TRUE,
                       sort = FALSE)

# Create TRUE/FALSE checking columns 
test_citation[, ":=" (author_test = Author == Author_CitationsFile,
                      DOI_test = DOI == DOI_CitationsFile,
                      year_test = Year == Year_CitationsFile)]

# Visualize any differences
test_citation[author_test == FALSE] # none
test_citation[DOI_test == FALSE, .(unique_number, unique_study_number, DOI, DOI_CitationsFile)]
# Detected differences are not important (some extra "_" character in DOI_CitationsFile). 
# They indicate that DOI is the correct version.
test_citation[year_test == FALSE] # none

rm(test_citation)

# -----------------------------------------------------------------------------
# Check some columns for possible typos
# -----------------------------------------------------------------------------
# -------------------------------------
# Check Author
# -------------------------------------
# Check Author for punctuation other than '-_ symbols
pl_dt[grepl(pattern = "[^[:alnum:][:space:]'-_]", x = Author), .(unique_number, Author)]
# Check Author for extra "_" (at the end of the string or within the string)
pl_dt[grepl(pattern = "_{1,}$|_{2,}", x = Author), .(unique_number, Author)]
# unique_number       Author
# 1:          2411 Zhang__et_al
# 2:          2413 Zhang__et_al
# Delete the extra "_" (keep only one)
pl_dt[grepl(pattern = "_{2,}", x = Author), # indexing is optional
      Author := gsub(pattern = "_{2,}", 
                     replacement = "_", 
                     x = Author)]
pl_dt[unique_number == "2413", Author]

# -------------------------------------
# Check DOI
# -------------------------------------
# Check DOI for extra "_" (at the end of the string or within the string)
pl_dt[grepl(pattern = "_{1,}$|_{2,}", x = DOI), .(unique_number, DOI)]

# -------------------------------------
# Check year
# -------------------------------------
sort(unique(pl_dt$Year))

# -------------------------------------
# Check Level_of_Supplementation
# -------------------------------------
sort(unique(pl_dt$Level_of_Supplementation))
# Replace "partialplant" with "partial_plant"
pl_dt[Level_of_Supplementation == "partialplant", Level_of_Supplementation := "partial_plant"]

# -------------------------------------
# Check coordinates
# -------------------------------------
# Check cases where conversion to numeric encounters characters that yield NA-s.
# This helps to check for legitimate conversions.
cols2check <- c("Latitude", "Longitude")
test <- pl_dt[, lapply(.SD, function(col) is.na(as.numeric(col)) != is.na(col)), 
              .SDcols = cols2check]
for (col in cols2check[ test[, colSums(.SD) > 0] ]){
    print(col)
    print(pl_dt[test[, get(col)], .(unique_number,  get(col))])
}
# none detected - all seems ok

rm(cols2check, test, col)


# =============================================================================
# Update ES columns
# =============================================================================
pl_dt[pl_es, on = "unique_number", 
      ":=" (PL_Effect_Size = ES_mst.VS,
            PL_Effect_Size_Type1 = ES_mst_idx.VS,
            PL_Effect_Size_Type2 = ES_mst_S.Bo.VS)]
# Save to csv file
write.csv(pl_dt, file = "Output/GloPL_with_id_updated_ES.csv", row.names = FALSE)

# =============================================================================
# Optional - Final data checking
# =============================================================================
# Generate a table with unique sorted values for each column
# Na-s are recycled to fill the maximum number of rows
cols2remove <- c("unique_number", "unique_study_number")
check_lst <- sapply( pl_dt[, -cols2remove, with = FALSE], function(col)(sort(unique(col), na.last = TRUE)) )
check_dt <- sapply(check_lst, "length<-", max(lengths(check_lst)))
check_dt <- as.data.table(check_dt)
# Save table to csv to be inspected visually with Excel
write.csv(check_dt, "output/GloPL_check_unique_val_per_column.csv", row.names = FALSE)
