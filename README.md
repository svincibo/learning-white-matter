
# Replication scripts for the Relaxed Lasso Analysis

(as in Vinci-Booher et al. "Microstructure of white matter tracts connecting perceptual and motor association cortices predict learning to draw novel symbols")

* Note that MATLAB scripts for the rest of the manuscript are available in https://github.com/svincibo/wml-wmpredictslearning

## Software setup

1. Download R: https://cran.r-project.org
2. Download RStudio Desktop: https://www.rstudio.com/products/rstudio/
3. Install R then RStudio.
4. Launch RStudio. Choose File > New Project > Version Control > Git
5. Paste the Clone link above in the "Repository URL" field.
6. Choose a location to save.

## Necessary packages

The first Chunk in `relaxed-lasso-analysis.Rmd` file with `eval = FALSE` provides code to install necessary packages. 

## Data files

The 4 necessary data files can be downloaded from [OSF.io](https://osf.io/95zjk/files/osfstorage).

They should be downloaded to the root of the project directory.

## Knit the `relaxed-lasso-analysis.Rmd`

Choosing the `tracts` parameter on line 7/8 toggles between the two MRI runs.


