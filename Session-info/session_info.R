# /////////////////////////////////////////////////////////////////////////
# Save R packages information in the files "Session-info/packages.csv" and
# "Session-info/sessionInfo.txt". This script was run after all needed packages
# for this repository were attached.
# /////////////////////////////////////////////////////////////////////////

rm(list = ls(all.names = TRUE)); gc(reset = TRUE)

# Store R session information in "Session-info/sessionInfo.txt'
sink(file = "Session-info/sessionInfo.txt")
sessionInfo()
sink()


# Store R package information in "Session-info/packages.csv"
# Store as table with three columns: package name, version, and type.
r_session <- sessionInfo()

pk   <- c(r_session$basePkgs,
          names(r_session$otherPkgs),
          names(r_session$loadedOnly))
ver  <- sapply(pk, getNamespaceVersion)
type <- rep(x = c("base_package", 
                  "attached_package", 
                  "loaded_not_attached"),
            times = c(length(r_session$basePkgs),
                      length(r_session$otherPkgs),
                      length(r_session$loadedOnly)))

df <- data.frame(package = pk, 
                 version = ver,
                 type = type)

write.csv(df, file = "Session-info/packages.csv", row.names = FALSE)
