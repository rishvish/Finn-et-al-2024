
# Design principles for multi-species productive grasslands: quantifying effects of diversity beyond richness

<!-- badges: start -->
<!-- [![Paper DOI](https://img.shields.io/badge/Paper_DOI-10.1007/s10651--023--00563--w-green)](https://doi.org/10.1007/s10651-023-00563-w)
[![Code DOI](https://img.shields.io/badge/Code_DOI-10.5281/zenodo.10827269-blue)](https://doi.org/10.5281/zenodo.10827269) -->
<!-- badges: end -->

This repository contains the source code for reproducing the figures in Finn et al. 2024 (paper link to be added soon).

### The File/Folder descriptions are as follows

+ **Helpers.R**: This file contains all the helper functions needed for preparing the visualisations and is referenced by the `Code for figures.R` and `Model coefficients.R` files. The code requires the latest versions (in 2024) of the following R packages and running this file would automatically install/update them
  - `DImodelsVis`
  - `DImodels` 
  - `MASS` 
  - `ggpubr`
  - `tidyverse` 
  - `PieGlyph` 
  - `gridExtra` 
  - `gtable` 
  - `rstudioapi` 
  - `nlme` 
  - `cowplot`
+ **Code for figures.R**: The main code for creating all the figures in the paper and supplementary material. The code takes about 20-30 minutes to run completely and would store the created visualisations in `Figures` folder with the corresponding names as in the paper.
+ **Model Coefficients.R**: The code for creating the table of model coefficients (Table S2 from supplementary material). This coefficients table would be stored as a pdf file called `Table S2.pdf` in the `Figures` folder.
+ **Figures Folder**: The folder containing the created resources after running the code in `Code for figures.R` and `Model coefficients.R` files.

