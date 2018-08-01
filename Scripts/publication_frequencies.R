# /////////////////////////////////////////////////////////////////////////
## Script for publication frequencies graph:
## "Figure 1. Distribution of year of publication for studies measuring pollen 
## limitation of plant reproduction included in the GloPL data base."
## Title extracted from draft PLDataPaper7_6_2017_GAG_JHB_WD_jcv.docx
# /////////////////////////////////////////////////////////////////////////

rm(list = ls()); gc(reset = TRUE)

library(data.table)
library(ggplot2)
library(ggsci)
library(scales)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read & prepare data -----------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read PL data. Treat all values as character. 
# Also convert to NA everything that is:
# "NA", "N/A", "null", "" (last one is treated as blank in Excel).
pl_dt <- fread("Output/share/GloPL_with_id_updated_ES.csv", 
               colClasses = "character",
               na.strings = c("NA","N/A","null", ""),
               select = c("unique_number", "unique_study_number", "Year"))

# There are character values for variable Year.
# NAs will be introduced by coercion.
unique(pl_dt$Year)
pl_dt[, Year := as.numeric(Year)]

# Aggregate
data_4graph <- pl_dt[!is.na(Year), # consider only records that have year information
                     .(N_publications = uniqueN(unique_study_number),
                       N_studies = uniqueN(unique_number)), # or .N if a study is a row
                     by = Year]
setorder(data_4graph, Year)
data_4graph[, N_cumul_studies := cumsum(N_studies)]

# Note that summing up N_publications will give less than 
# pl_dt[, uniqueN(unique_study_number)] because publications without year were removed
write.csv(data_4graph,
          "Output/cache/table_for_publication_freq_graph_ok.csv",
          row.names = FALSE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot --------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set 2nd scale "multiplication" factor
my_factor <- 0.02
# it should be in the range of the ratio of max value on left OY 
# divided by max values of the right OY (2nd axis)
max(data_4graph$N_publications)/max(data_4graph$N_cumul_studies)

# Experiment with color
mypal = ggsci::pal_npg("nrc", alpha = 1)(2)
scales::show_col(mypal); mypal

color_bars <- "#90B5CE" # color_bars <- "#4DBBD5FF" # for draft 6
color_line <- "#345269" # color_line <- "#E64B35FF" # for draft 6

my_plot <- 
    ggplot(data = data_4graph, 
           aes(x = Year)) +
    geom_bar(aes(y = N_publications), 
             stat = "identity",
             fill = color_bars) +
    geom_line(aes(y = N_cumul_studies * my_factor),
              size = 1,
              color = color_line) +
    # set range on OX axes and adjust the distance (gap) from OY axes
    scale_x_continuous(expand = c(0.025, 0)) +
    # set range on OY axes and adjust the distance (gap) from OX axes
    scale_y_continuous(sec.axis = sec_axis(trans = ~ . / my_factor, 
                                           name = "Cumulative number of\npollen limitation cases"),
                       limits = c(0, 75),
                       expand = c(0, 0)) +
    # set axis labels
    labs(x = "Year of publication", 
         y = "Number of publications") + 
    # eliminate default background
    theme_bw() + 
    theme( 
        panel.grid.major = element_line(size = 0.3, 
                                        linetype = "longdash"),
        panel.grid.minor = element_blank(), # eliminate minor grids
        # set font family for all text within the plot ("sans" = "Arial");
        # note that this can be overridden with other adjustment functions below
        text = element_text(family = "sans", size = 8),
        # adjust text in X-axis title
        # axis.title.x = element_text(size = 10),
        # adjust title of first/left Y-axis
        axis.title.y = element_text(color = color_bars),
        # adjust labels of first/left Y-axis 
        axis.text.y  = element_text(color = color_bars),
        # adjust title of right/second Y-axis 
        axis.title.y.right = element_text(angle = 90, # set text rotation
                                          color = color_line), 
        # adjust labels of right/second Y-axis 
        axis.text.y.right  = element_text(color = color_line),
        # set margins around entire plot ( https://goo.gl/zdgLMt )
        plot.margin = unit(c(t = 0.4, 
                             r = 1.1, 
                             b = 0, 
                             l = 0.1), 
                           "mm")
    )

# save graph
ggsave(plot = my_plot,
       filename = "Output/Publication_freq_draft_10.pdf", 
       width = 9, height = 5.5, units = "cm")

ggsave(plot = my_plot,
       filename = "Output/Publication_freq_draft_10.png", 
       width = 9, height = 5.5, units = "cm", dpi = 1000)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# References --------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# https://rpubs.com/MarkusLoew/226759
# https://whatalnk.github.io/r-tips/ggplot2-secondary-y-axis.nb.html
# using the grob approach: http://rpubs.com/kohske/dual_axis_in_ggplot2
