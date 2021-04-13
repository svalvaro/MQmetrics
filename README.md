
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MQmetrics

<!-- badges: start -->

<!-- badges: end -->

The goal of MQmetrics is to analyze Proteomics data from LC-MS/MS. It
takes the output tables from MaxQuant and plots multiple parameters.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("BioAlvaro/MQmetrics")
```

## Example

MQmetrics will easily generate a report containing all the info with one
function. For that it is needed to provide the directory of the combined
folder of MaxQuant output.

``` r
library(MQmetrics)

MQPathCombined <- '/home/alvaro/Documents/MaxQuant/combined/'

generateReport(MQPathCombined, long_names = TRUE, sep_names = '_')
```

One of the most useful parameters of every function including
**generateReport()** are: *long\_names* and *sep\_names*. They will
allow a clear visualization of sample that have long names separated by
a character. Example:
