# /////////////////////////////////////////////////////////////////////////
## Script to check some simple descriptive statistics and run ANOVA of the
## effect size explained by the level of supplementation.
# /////////////////////////////////////////////////////////////////////////

rm(list = ls(all.names = TRUE)); gc(reset = TRUE)

library(data.table)


# The hack with the setting of the working directory below is important only if
# compiling this script as HTML report.
setwd(gsub(pattern = '/Scripts', replacement = '', x = getwd()))


# Read merged PL data
pl_dt <- fread("output/GloPL_with_id_updated_ES.csv")

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
order_dt <- read.csv("Data/phylogeny/Phylogeny_annotation.csv")
length(unique(order_dt$order))
# [1] 45

# Sup vs. Bagout
my_tbl <- table(pl_dt$PL_Effect_Size_Type2)
my_tbl
# Bagout    Sup 
#    916   2053 
prop.table(as.matrix(my_tbl), margin = 2)
# Bagout 0.3085214
# Sup    0.6914786


# Run ANOVA on Level_of_Supplementation with ES values
pl4anova <- pl_dt[,.(Level_of_Supplementation, PL_Effect_Size)]
# Note that there are some NA-s in Level_of_Supplementation column
pl_dt[is.na(Level_of_Supplementation), .N]
# 7 NA-s
# Remove rows that have NA-s
pl4anova <- pl4anova[complete.cases(pl4anova)]

pl4anova[, PL_Effect_Size := as.numeric(PL_Effect_Size)]
pl4anova[, Level_of_Supplementation := as.factor(Level_of_Supplementation)]
levels(pl4anova$Level_of_Supplementation)

boxplot(PL_Effect_Size ~ Level_of_Supplementation, data = pl4anova, ylab = "ES")
aov.model <- aov(PL_Effect_Size ~ Level_of_Supplementation, data = pl4anova)
summary(aov.model)
TukeyHSD(aov.model)

# Plot Tukey Honest Significant Differences
# Adjust margins of plotting region (Bottom, Left, Top, Right)
par(mai = c(1,2.5,1,0.5))
plot(TukeyHSD(aov.model), las=2)
dev.off()
