# Exploring models for relational data

Source code for an MA Statistics thesis project at UCD.

## Requirements

-   [Git LFS extension](https://git-lfs.com/) for checking out large CSV data
-   R or better [RStudio](https://posit.co/download/rstudio-desktop/)
-   [Clang](https://clang.llvm.org/) or other C++ compiler
-   A set of R libraries (see sources)

## Project overview

-   `Config.R` - shared filed with input and output file names and color theme
-   `PlotConfig.R` - shared config for `ggplot2` and helper routiens
-   `data` - input source [ATP data](https://datahub.io/sports-data/atp-world-tour-tennis-data) and data derived by analysis
-   `PrepareData.R` - R script for EDA and to convert ATP data to necceary formats
-   `charts` - output for SVG charts
-   `BradleyTerry.R` - code for the Bradley Terry analysis
-   `cpp` - C++ for Stochastic Block Model (SBM) inference
-   `SBMTest.R` - some code to verify correctness of the SBM C++ code
-   `SBM.R` - code for clustering analysis with SBM 

# Acknowledgements

-   [The Bradley-Terry Model of Ranking via Paired Comparisons](https://www.rpubs.com/dkarwosk12345/560307) Bob Carpenter
