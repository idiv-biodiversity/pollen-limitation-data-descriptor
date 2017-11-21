####################################################################################
## Script for publication frequencies.
## For "Figure 1. Distribution of year of publication for studies measuring pollen 
## limitation of plant reproduction included in the GloPL data base."
####################################################################################

library(data.table)
library(ggplot2)

# =================================================================================
# Read & prepare data
# =================================================================================
# read latest dataset (prepared by Joanne)
PL_all <- fread("Data/PL_ANALYSIS_02_10_2017.csv", colClasses = "character")

# get only columns of interest
# Note: what is unique_study_number_TK vs unique_study_number ? see the Excel file PL_masters_02_10_2017.xlsx
PL <- PL_all[,.(Year, Author, unique_number, unique_study_number)]

# there are character values for variable Year
unique(PL$Year)
# replace "2003b" with "2003" so to avoid transformation to NA when applying as.numeric
PL[Year == "2003b", Year := "2003"]
PL[, Year := as.numeric(Year)]
# which records got NA for variable Year?
PL[is.na(Year)]

PL_aggreg <- PL[!is.na(Year), 
                .(N_publications = uniqueN(unique_study_number),
                  N_studies = uniqueN(unique_number)), # same as = .N
                by = Year]
PL_aggreg <- PL_aggreg[order(Year)]
PL_aggreg[, cumul_studies := cumsum(N_studies)]

# https://rpubs.com/MarkusLoew/226759
# https://whatalnk.github.io/r-tips/ggplot2-secondary-y-axis.nb.html

# should be in the range of 
# max(PL_aggreg$cumul_studies)/max(PL_aggreg$N_publications)
# factor can be the ratio of max value on left OY divided by max of right OY
# my_factor <- max(PL_aggreg$N_publications)/max(PL_aggreg$cumul_studies)
my_factor <- 0.02

my_plot <- ggplot(data = PL_aggreg, 
                  aes(x = Year))
my_plot <- my_plot + geom_bar(aes(y = N_publications), 
                              stat = "identity",
                              fill = "gray60") 
my_plot <- my_plot + geom_line(aes(y = cumul_studies * my_factor),
                               size = 1)
my_plot <- my_plot + scale_y_continuous(sec.axis = sec_axis(trans = ~ . / my_factor, 
                                                            name = "Cumulative number of pollen limitation studies"))
my_plot <- my_plot + 
    # set axis labels
    labs(x = "Year of publication", 
         y = "Number of publications") + 
    # eliminate default backgound
    theme_bw() + 
    theme( panel.grid.minor = element_blank(), # eliminate minor grids
           # set font family for all text within the plot ("serif" should work as "Times New Roman")
           # note that this can be overridden with other adjustment functions below
           text = element_text(family="serif"),
           # adjust text in X-axis title
           axis.title.x = element_text(size = 10, face = "bold"),
           # adjust text in Y-axis title
           axis.title.y = element_text(size = 10, face = "bold") ) 

my_plot

ggsave(plot = my_plot,
       filename = "Output/Publication_freq.pdf", 
       width = 12, height = 10, scale = 1, units = "cm")