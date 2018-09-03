In order to make this project more reproducible, the list of all used R packages and their dependencies with their version numbers were made available. 
One can find this information in the `packages.csv` file. 
Also, the R session information was stored in the `sessionInfo.txt` file. If needed, check the `session_info.R` script to see how these files were created.

Note that the package [plotbiomes][1] was installed from GitHub (see details at the beginning of the script `Whittaker_biomes_plot.R`).
Also, the package [ggtree][2] was installed from Bioconductor (see details at the beginning of the script `Phylogeny_graph.R`)

For installing the exact versions of the packages, one of the following packages could be helpful: [versions][3], [devtools][4], [checkpoint][5].
The [versions][3] package might be a more flexible options for Windows users because it does not require [Rtools][6]. This [Stack Overflow discussion][7] is also helpful.

Here is an example for installing the `MASS` package, version `7.3-47`.
```{r}
install.packages('versions')
library(versions)
if(!dir.exists("Session-info/lib/")) dir.create("Session-info/lib/")
install.versions(pkgs = "MASS", versions = "7.3-47", lib = "Session-info/lib/")
```

[1]: https://github.com/valentinitnelav/plotbiomes
[2]: https://bioconductor.org/packages/release/bioc/html/ggtree.html
[3]: https://cran.r-project.org/web/packages/versions/index.html
[4]: https://cran.r-project.org/web/packages/devtools/index.html
[5]: https://cran.r-project.org/web/packages/checkpoint/
[6]: https://cran.r-project.org/bin/windows/Rtools/
[7]: https://stackoverflow.com/q/17082341/5193830
