
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ProteoMS

<!-- badges: start -->

<!-- badges: end -->

The goal of ProteoMS is to analyze Proteomics data from LC-MS/MS. It
takes the output tables from MaxQuant and plots multiple parameters.

## Installation

<!-- You can install thess released version of ProteoMS from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("ProteoMS") -->

<!-- ``` -->

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("BioAlvaro/ProteoMS")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
#First you need to load this libraries:
library(readr)
library(tidyverse)
library(gridExtra)
library(chron)
```

``` r
library(ProteoMS)
## basic example code

dir <- system.file('extdata', package = 'ProteoMS') #is the directory with the output of the MaxQuant analysis.

files <- ReadDataFromDir(dir) #This function will read the tables needed for creating the outputs.

#files can be extracted like this:
summary <- files[["summary.txt"]]#
evidence <- files[["evidence.txt"]]#tomodify
peptides <- files[["peptides.txt"]]#
msmsScans <- files[["msmsScans.txt"]]#tomodify  
proteinGroups <- files[["proteinGroups.txt"]]#
runningTimes <-  files[["runningTimes.txt"]]#
parameters <- files[["parameters.txt"]]#
```

``` r
ExperimentInformation(runningTimes, parameters) 
#> [1] "The experiment started the day: 03/02/2021 at the time: 14:14:22."
#> [1] "The whole experiment lasted: 01:33 (hours:minutes)."
#> [1] "The MaxQuant version used was: 1.6.12.0"
#> [1] "The user was: alvaro.sanchez"
#> [1] "The machine name was: FGU045PC004"
#> [1] "The protein FDR was: 0.01"
#> [1] "The match between runs was: True"
#> [1] "The fasta file used was: C:\\MaxQuant_Databases\\UP000000589_10090.fasta"
```

``` r
PlotPeaks(summary)
```

<img src="man/figures/README-PlotPeaks-1.png" width="100%" />

``` r
PlotMsMs(summary)
```

<img src="man/figures/README-PlotMSMS-1.png" width="100%" />

``` r
PlotIsotopePattern(summary)
```

<img src="man/figures/README-isotope-1.png" width="100%" />

``` r
PLotPeptidesIdentified(summary)
```

<img src="man/figures/README-PeptidesIdentified-1.png" width="100%" />

``` r

PlotIntensity(proteinGroups, intensity_type = 'LFQ')
```

<img src="man/figures/README-PlotIntensity-1.png" width="100%" />

``` r
PlotIdentificationType(peptides)
#> Using sample as id variables
```

<img src="man/figures/README-IdentificationType-1.png" width="100%" />

``` r
PlotCharge(evidence)
```

<img src="man/figures/README-Charg-1.png" width="100%" />

``` r
PlotTotalIonCurrent(msmsScans)
```

<img src="man/figures/README-TotalIonCurrent-1.png" width="100%" />

``` r
PlotCombinedDynamicRange(proteinGroups)
```

<img src="man/figures/README-DynamicRange-1.png" width="100%" />

``` r
PLotAllDynamicRange(proteinGroups,columns = 1,rows = 3)
```

<img src="man/figures/README-DynamicRangeAll-1.png" width="100%" />
