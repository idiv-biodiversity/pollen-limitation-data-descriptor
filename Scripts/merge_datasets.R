library(data.table)
library(readxl)

# =============================================================================
# Read & clean data
# =============================================================================

# -------------------------------------
# Read PL data 
# -------------------------------------
# Read first with default settings for guessing column types.
# This is helpful because triggers warnings messages pointing to various unexpected values.
# Is important to check the warnings with warnings() function
pl_dt <- read_excel(path  = "Data/for_merging/PL_masters_for publication_23_01.xlsx", 
                    sheet = 1)
warnings()
# Spreadsheet cell L2052 had not a proper minus sign for its numeric value. Corrected now directly in the Excel file.
# Spreadsheet column C ("Year") is expected to be numeric but has some character values. Keep it character.
# Check for example cell C2420 with value "2003b". Corrected to 2003 now directly in the Excel file.

# Read PL data (all columns as character)
pl_dt <- read_excel(path  = "Data/for_merging/PL_masters_for publication_23_01.xlsx", 
                    sheet = 1, col_types = "text")
setDT(pl_dt)
pl_dt[, row_idx_pldata := 1:.N]

# -------------------------------------
# Read PL data with ES
# -------------------------------------
ES_dt <- fread("Output/PL_masters_for_publication_with_ES_cols.csv",
               select = c("unique_number", "ES_mst.VS", "ES_mst_idx.VS", "ES_mst_S.Bo.VS"),
               colClasses = "character")

# -------------------------------------
# Read meta_data (all columns as character)
# -------------------------------------
meta_dt <- read_excel(path  = "Data/for_merging/Meta_data_24_01.xlsx", 
                   sheet = "MetaData", col_types = "text")
setDT(meta_dt)
# Delete any quotation symbol from column names
data.table::setnames(meta_dt, gsub("'", "", names(meta_dt)))
# Replace spaces with dots in column names
data.table::setnames(meta_dt, gsub(" ", ".", names(meta_dt)))
meta_dt[, row_idx_meta := 1:.N]

# -------------------------------------
# Read citations data (all columns as character)
# -------------------------------------
citations_dt <- read_excel(path  = "Data/for_merging/Citations_24_01C.xlsx", 
                   sheet = 1, col_types = "text")
setDT(citations_dt)
# Replace spaces with dots in column names
data.table::setnames(citations_dt, gsub(" ", ".", names(citations_dt)))
citations_dt[, row_idx_citations := 1:.N]

# =============================================================================
# a) Merge in "Data citation", year and DOI. Also do some data cleaning.
# from Citations_Pollen_supp_06_11_17 (002).xlsx to PL_masters_for publication_23_01.xlsx
# =============================================================================
setnames(citations_dt, 
         old = c("Author", "DOI", "year"),
         new = c("Author_CitationsFile", "DOI_CitationsFile", "Year_CitationsFile"))

merged_dt_1 <- merge(x = pl_dt,
                     y = citations_dt[,.(Unquic.study.Number, 
                                         Author_CitationsFile, 
                                         DOI_CitationsFile, 
                                         Year_CitationsFile, 
                                         row_idx_citations)],
                     by.x = "unique_study_number",
                     by.y = "Unquic.study.Number",
                     all = TRUE,
                     sort = FALSE)

# Check for any mismatches in unique_study_number key.
# Should get character(0) in both cases:
merged_dt_1[is.na(row_idx_citations), unique(unique_study_number)]
merged_dt_1[is.na(row_idx_pldata), unique(unique_study_number)]

# -------------------------------------
# Check for artefacts in DOI
# -------------------------------------
dois <- sort(merged_dt_1[, unique(DOI_CitationsFile)], na.last = TRUE)
dois
# Some DOI values that might be artefacts:
# "10. 3969 / j. issn. 1004-1524. 2013. 06. 16" 
# "doi:10.1016/j.aquabot.2011.03.010"                     
# "doi:10.1016/j.sajb.2009.06.014"                       
# "doi:10.1086/431803"                                    
# "doi:10.1093/aob/mcq149"                               
# "http://www.jstor.org/stable/4095852"                   
# "NA10.1139/b99-015"                                     
# "10.3969_/j?issn?1000-3142?2014?06.006"
# "10.11931/guihaia?gxzw201403018"                        
# "10.13388/j_.cnki_.ysajs.2009.03.018"
# "10.13989/j_.cnki_.0517_-6611"
# "10.5846_/stxb201305311251"
# "NA" 
# NA 
# Note that there are a few cells with "NA" as character and then some blanks (that become NA). 
# Convert both to NA:
merged_dt_1[DOI_CitationsFile == "NA", DOI_CitationsFile := NA]
# Joanne updated all other reported artefacts directly in the Excel file.

# Check for all DOI-s that may contain "?" or "_" characters
dois[grepl(pattern = "\\?|_", x = dois)]
# "10.1016/j.aquabot.2013.12.003_"
# Replace 
merged_dt_1[DOI_CitationsFile == "10.1016/j.aquabot.2013.12.003_", DOI_CitationsFile := "10.1016/j.aquabot.2013.12.003"]

# (Optional) Checkmismatches between the old and new DOI
# test_doi <- merged_dt_1[DOI_CitationsFile != DOI.x & DOI.x != "NA", .(unique_study_number, DOI.x, DOI_CitationsFile)]
# test_doi <- unique(test_doi, by = c("DOI.x", "DOI_CitationsFile"))
# test_doi[, DOI.x2 := gsub(pattern = "_|doi |doi|doi.org/|DOI:_|doi:_|doi:|DOI_|dx.doi.org/|http://dx.doi.org/|http://www.jstor.org/stable/",
#                           replacement = "", x = DOI.x) ]
# test_doi2 <- test_doi[DOI_CitationsFile != DOI.x2]
# test_doi2 <- unique(test_doi2, by = c("DOI.x2", "DOI_CitationsFile"))

# -------------------------------------
# Check mismatches between the old and new year data
# -------------------------------------
year_mismatches <- unique(merged_dt_1[Year != Year_CitationsFile, .(unique_study_number, Year, Year_CitationsFile)])
write.csv(year_mismatches, "output/for_merging/year_mismatches.csv", row.names = FALSE)

# Check for artefacts in column "Year_CitationsFile"
merged_dt_1[, sort(unique(Year_CitationsFile))] 
# Replace "2003b" with "2003" in column "Year_CitationsFile"
merged_dt_1[Year_CitationsFile == "2003b", Year_CitationsFile := "2003"]

# -------------------------------------
# Check mismatches between the old and new Author data
# -------------------------------------
author_mismatches <- unique(merged_dt_1[Author != Author_CitationsFile,
                                        .(unique_study_number, Author, Author_CitationsFile)])
# For detecting acute differences, remove punctuation, extra white space, translate to lower case and delete "and"
author_mismatches[, Author := trimws(gsub(pattern = '[[:punct:]]', 
                                          replacement = ' ', 
                                          x = Author))]
author_mismatches[, Author := gsub(pattern = 'and', 
                                   replacement = '', 
                                   x = Author)]
author_mismatches[, Author := tolower(trimws(gsub(pattern = '\\s+', 
                                                  replacement = '_', 
                                                  x = Author)))]
author_mismatches[, Author_CitationsFile := trimws(gsub(pattern = '[[:punct:]]', 
                                                        replacement = ' ', 
                                                        x = Author_CitationsFile))]
author_mismatches[, Author_CitationsFile := trimws(gsub(pattern = 'and', 
                                                        replacement = '', 
                                                        x = Author_CitationsFile))]
author_mismatches[, Author_CitationsFile := tolower(trimws(gsub(pattern = '\\s+', 
                                                                replacement = '_', 
                                                                x = Author_CitationsFile)))]
author_mismatches <- unique(author_mismatches[Author != Author_CitationsFile])
head(author_mismatches)
write.csv(author_mismatches, "output/for_merging/author_mismatches.csv", row.names = FALSE)

# =============================================================================
# b) Merge in ES columns: "ES_mst.VS", "ES_mst_idx.VS", "ES_mst_S.Bo.VS"
# =============================================================================
# In ES_mst_idx.VS, replace "Fruitset" with "FS" and "Seedset" with "SO"
ES_dt[, sort(unique(ES_mst_idx.VS), na.last = TRUE)] # check unique values
ES_dt[ES_mst_idx.VS == "Fruitset", ES_mst_idx.VS := "FS"]
ES_dt[ES_mst_idx.VS == "Seedset", ES_mst_idx.VS := "SO"]

# Merge
merged_dt_2 <- merge(x = merged_dt_1,
                     y = ES_dt,
                     by = "unique_number",
                     all = TRUE,
                     sort = FALSE)

# All existing NA-s in ES values are legitimate. 
# That is, they are not caused by merging mismatches,
# but by the fact that ES cannot be computed in the first place 
# because of lack of data (see script Compute_ES.R).
all.equal(merged_dt_2[is.na(ES_mst.VS), .(unique_number, ES_mst.VS, ES_mst_idx.VS, ES_mst_S.Bo.VS)],
          ES_dt[is.na(ES_mst.VS)]) # should give TRUE

# =============================================================================
# c) Change the column names to those listed in metadata file
# =============================================================================
old_names <- meta_dt$Current.Column.Name
new_names <- meta_dt$Variable.within.GloPL.database

# Check if old_names can be found in current column names.
# Display which cannot be found:
old_names[!(old_names %in% names(merged_dt_2))]

# Careful with versions of year, DOI and author.
# Subset ony to desired columns
merged_dt_3 <- merged_dt_2[, c(old_names, 
                               "DOI_CitationsFile", 
                               "Author_CitationsFile",
                               "Year_CitationsFile"), 
                           with = FALSE]

# Rename according to metadata
setnames(merged_dt_3, 
         old = old_names, 
         new = new_names)

# Sort column names
setcolorder(merged_dt_3, sort(names(merged_dt_3)))

# save to csv file
write.csv(merged_dt_3, "output/for_merging/merged.csv", row.names = FALSE)
