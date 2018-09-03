# /////////////////////////////////////////////////////////////////////////
# Effect Size (ES) histograms
# /////////////////////////////////////////////////////////////////////////

rm(list = ls(all.names = TRUE)); gc(reset = TRUE)


library(data.table)
library(ggplot2)

# Read ES table
es_dt <- fread("Output/cache/master_es_multi_methods.csv")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Log ratio vs Hedges's d -------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

range(es_dt$PL_Effect_Size_hedgesD , na.rm = TRUE)
# -535.1298 2599.3613

quantile(es_dt$PL_Effect_Size_hedgesD, 
         probs = c(0.005, 0.995),
         na.rm = TRUE)
#          0.5%     99.5% 
#     -11.79197  72.35611
quantile(es_dt$PL_Effect_Size_hedgesD, 
         probs = c(0.05, 0.95),
         na.rm = TRUE)
#            5%       95% 
#     -1.293465 12.533562 

es_melt_1 <- melt(es_dt[, .(PL_Effect_Size_lnRktoall_0.5,
                            PL_Effect_Size_hedgesD)])
# The warning is ok

es_melt_1[, variable := factor(variable, labels = c("Log response ratio",
                                                    "Hedges' d"))]

# Create a dummy data.frame to help with placing white horizontal lines at the
# zero level to force-delete the default gray zero line in each panel. The
# default line doesn’t allow the visual detection of very small frequencies at
# big values when printing. The y values were discovered by try & error.
dummy <- data.frame(variable = c("Log response ratio", "Hedges' d"), 
                    y = c(-0.6, -2.3)) 

hist_1 <- 
    ggplot(data = es_melt_1, 
           aes(x = value)) +
    geom_histogram(bins = 50, fill = "gray20") +
    facet_wrap(~ variable, scales = "free", ncol = 2) +
    labs(x = "Effect size", y = "Counts") +
    # Draw a thin white horizontal line to “delete” the default zero gray line
    geom_hline(data = dummy,
               aes(yintercept = y),
               colour = "white",
               size = 0.001) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), # eliminate minor grids,
          # set font family for all text within the plot ("sans" should work as "Arial")
          # note that this can be overridden with other adjustment functions below
          text = element_text(family = "sans", size = 8),
          strip.text = element_text(size = 8),
          # set margins around entire plot ( https://goo.gl/zdgLMt )
          plot.margin = unit(c(t = 0.3, 
                               r = 0.6, 
                               b = 0.1, 
                               l = 0), 
                             "mm"))

ggsave(hist_1,
       file = "Output/share/histo_lnR_vs_HedgeD_draft_3.pdf",
       units = "cm", width = 14, height = 7)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Log ratio - various constants -------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

es_melt_2 <- melt(es_dt[, names(es_dt) %like% "PL_Effect_Size_lnRkto0", 
                        with = FALSE])
# The warning is ok

es_melt_2[, variable := factor(variable, 
                               labels = paste0("Log response ratio, k = ", 
                                               c(0.01, 0.05, 0.1, 0.5)))]

hist_2 <- 
    ggplot(data = es_melt_2, 
           aes(x = value)) +
    geom_histogram(binwidth = 0.05, fill = "gray20") +
    facet_wrap(~ variable, scales = "free", ncol = 1) +
    labs(x = "Effect size", y = "Counts") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), # eliminate minor grids,
          # set font family for all text within the plot ("sans" should work as "Arial")
          # note that this can be overridden with other adjustment functions below
          text = element_text(family = "sans", size = 8),
          strip.text = element_text(size = 8),
          # set margins around entire plot ( https://goo.gl/zdgLMt )
          plot.margin = unit(c(t = 0.3, 
                               r = 0.6, 
                               b = 0.1, 
                               l = 0), 
                             "mm"))

ggsave(hist_2,
       file = "output/share/histo_lnR_testing_constants_draft_1.pdf",
       units = "cm", width = 7, height = 14)
