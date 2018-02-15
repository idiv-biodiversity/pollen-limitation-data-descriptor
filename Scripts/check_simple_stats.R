###############################################################################
## Script to check some stats and values.
###############################################################################

library(data.table)
library(readxl)
setwd("C:/Dropbox (iDiv)/_iDiv/Projects/pollen_limitation/Graphs_nature_dataset")

# read merged PL data
pl_dt <- fread("output/GloPL_with_id_updated_ES.csv")
# str(pl_dt)

# Check that all the unique study numbers that are in the PL data file exist in the citation file as well.
citations_dt <- fread(file = "Data/for_merging/Citations_25_01.csv",
                      colClasses = "character")
# Replace spaces with dots in column names
data.table::setnames(citations_dt, gsub(" ", ".", names(citations_dt)))
citations_dt[, row_idx_citations := 1:.N]

all.equal(unique(sort(pl_dt$unique_study_number)),
          unique(sort(citations_dt$Unquic.study.Number)))
# TRUE

# Number of unique studies 
length(unique(pl_dt$unique_study_number))
# 927
# same as above
# pl_dt[, uniqueN(unique_study_number)]

# Number of wild plant species
length(unique(pl_dt$Species_accepted_names))
# 1265

# Number of families
length(unique(pl_dt$Family))
# 163

# Number of orders 
# Check file Data/phylogeny/Phylogeny information_VS.xlsx
order_dt <- read_excel(path  = "Data/phylogeny/Phylogeny information_VS.xlsx", 
                       sheet = 1)
length(unique(order_dt$order))
# [1] 45

# Sup vs. Bagout
my_tbl <- table(pl_dt$PL_Effect_Size_Type2)
my_tbl
# Bagout    Sup 
# 916   2053 
prop.table(as.matrix(my_tbl), margin = 2)
# Bagout 0.3085214
# Sup    0.6914786

# Run ANOVA on Level_of_Supplementation with ES values
pl4anova <- pl_dt[,.(Level_of_Supplementation, PL_Effect_Size)]
# Note that there are some NA-s in Level_of_Supplementation column
pl_dt[is.na(Level_of_Supplementation), .N]
# 37 NA-s
# Remove rows that have NA-s for ANOVA analysis
pl4anova <- pl4anova[complete.cases(pl4anova)]

pl4anova[, PL_Effect_Size := as.numeric(PL_Effect_Size)]
pl4anova[, Level_of_Supplementation := as.factor(Level_of_Supplementation)]
levels(pl4anova$Level_of_Supplementation)

boxplot(PL_Effect_Size ~ Level_of_Supplementation, data = pl4anova, ylab = "ES")
aov.model <- aov(PL_Effect_Size ~ Level_of_Supplementation, data = pl4anova)
summary(aov.model)
TukeyHSD(aov.model)

# Adjust margins of plotting region (Bottom, Left, Top, Right)
par(mai = c(1,2.5,1,0.5))
plot(TukeyHSD(aov.model), las=2)
dev.off()
