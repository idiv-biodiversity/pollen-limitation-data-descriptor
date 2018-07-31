# /////////////////////////////////////////////////////////////////////////
## Helper functions to calculate the effect size (ES)
## Several methods are considered:
## + log response ratio, sensu Hegdes et al., 1999 equations
##   see http://onlinelibrary.wiley.com/wol1/doi/10.1890/0012-9658(1999)080%5B1150:TMAORR%5D2.0.CO;2/abstract
##   Here several ways are implemented:
##   - the original formulae, without adding a constant to the means;
##   - add a constant to all means;
##   - add a constant to zero means only;
## + Hedges' d (known also as Hedges' g);
##   for formulae, see https://onlinelibrary.wiley.com/doi/epdf/10.1111/ele.12934, 
##   Appendix S2
# /////////////////////////////////////////////////////////////////////////


# Function for ES computation ---------------------------------------------

# _____ Arguments/Input:
# x1     = treatment 1 averages (supp_X_ or bagout_X_ columns)
# x2     = treatment 2 averages (open_X_ columns)
# n1     = columns with number of observations (supp_N_ or bagout_N_ columns)
# n2     = columns with number of observations (open_N_ columns)
# sd1    = SD for treatment 1 (supp_SD_ or bagout_SD_ columns)
# sd2    = SD for treatment 2 (open_SD_ columns)
# k      = constant to add (e.g. 0.5) to the means
# method = Character, with following possible values:
#          - "lnR" - log response ratio: no constant is added to the means;
#          - "lnRktoall" - log response ratio: add a constant to the means;
#          - "lnRkto0" - log response ratio: add a constant to zero means only;
#          - "hedgesD" - Hedges' d (known also as Hedges' g)

get_ES <- function(x1, x2, n1, n2, sd1, sd2, k, method){
    # Compute type of ES based on given method
    ES <- switch( method,
                  # log response ratio, no constant
                  lnR = log(x1) - log(x2), # or log(x1/x2)
                  # log response ratio, add a constant to all means
                  lnRktoall = log(x1 + k) - log(x2 + k),
                  # log response ratio, add a constant to zero means only
                  lnRkto0 = ifelse( x1 == 0 | x2 == 0, 
                                    yes = log(x1 + k) - log(x2 + k),
                                    no = log(x1) - log(x2) ),
                  # Hedges' d (Hedges' g)
                  hedgesD = .get_hedgesD(x1, x2, n1, n2, sd1, sd2))
    # Replace -Inf (for log(0) cases) with NaN
    ES[is.infinite(ES)] <- NaN
    return(ES)
}


# Helper for Hedges'd -----------------------------------------------------

# Computes Hedges' d (Hedges' g)
# This function is a helper in get_ES()
.get_hedgesD <- function(x1, x2, n1, n2, sd1, sd2) {
    # mean difference
    dif <- x1 - x2 
    # numerator of the pooled variance sp
    sp_num <- (n1 - 1) * sd1^2 + (n2 - 1) * sd2^2
    # denominator of the pooled variance sp
    sp_denom <- n1 + n2 - 2
    # pooled variance sp
    sp <- sqrt(sp_num/sp_denom)
    # correction j
    j <- 1 - 3 / (4 * sp_denom - 1)
    # Hedges' d (Hedges' g)
    hd <- dif / sp * j
    return(hd)
}
