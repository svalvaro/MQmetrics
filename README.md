
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

``` r
library(ProteoMS)
```

``` r
MQPathCombined <- system.file('extdata', package = 'ProteoMS') #is the directory with the output of the MaxQuant analysis.

#MQPathCombined <- '/home/alvaro/Documents/MaxQuant_results/combined/'

files <- ReadDataFromDir(MQPathCombined) #This function will read the tables needed for creating the outputs.

#files can be extracted like this:

summary <- files[["summary.txt"]]
evidence <- files[["evidence.txt"]]
peptides <- files[["peptides.txt"]]
msmsScans <- files[["msmsScans.txt"]]  
proteinGroups <- files[["proteinGroups.txt"]]
runningTimes <-  files[["#runningTimes.txt"]]
parameters <- files[["parameters.txt"]]
```

``` r
ExperimentInformation(runningTimes, parameters) 
[1] "The experiment started the day: 17/02/2021 at the time: 18:57:27."
[1] "The whole experiment lasted: 05:48 (hours:minutes)."
[1] "The MaxQuant version used was: 1.6.12.0"
[1] "The user was: marek.vrbacky"
[1] "The machine name was: FGU013PC029"
[1] "The protein FDR was: 0.01"
[1] "The match between runs was: True"
[1] "The fasta file used was: C:\\MaxQuant_Databases\\UP000000589_10090.fasta"
```

``` r
PlotPeaks(summary, long_names = TRUE, sep_names = '_')
```

<img src="man/figures/README-PlotPeaks-1.png" width="100%" />

``` r
PlotMsMs(summary,long_names = TRUE, sep_names = '_')
```

<img src="man/figures/README-PlotMSMS-1.png" width="100%" />

``` r
PlotIsotopePattern(summary,long_names = TRUE, sep_names = '_')
```

<img src="man/figures/README-isotope-1.png" width="100%" />

``` r
PLotPeptidesIdentified(summary, long_names = TRUE, sep_names = '_')
```

<img src="man/figures/README-PeptidesIdentified-1.png" width="100%" />

``` r

PlotIntensity(proteinGroups, intensity_type = 'LFQ', log_base = 10, long_names = TRUE, sep_names = '_')
```

<img src="man/figures/README-PlotIntensity-1.png" width="100%" />

``` r
Path_iRT_run_with_iRT_peptides <- '/home/alvaro/Documents/MaxQuant/example3/'

files_irt <- ReadDataFromDir(Path_iRT_run_with_iRT_peptides)

evidence_irt <- files_irt[['evidence.txt']]
PlotiRT(evidence_irt, show_calibrated_rt = FALSE)
```

<img src="man/figures/README-irt_peps1-1.png" width="100%" />

``` r

PlotiRTScore(evidence_irt)
```

<img src="man/figures/README-irt_peps2-1.png" width="100%" />

``` r
PlotIdentificationType(peptides, long_names = TRUE, sep_names = '_')
#> Using sample as id variables
```

<img src="man/figures/README-IdentificationType-1.png" width="100%" />

``` r
PlotCharge(evidence_irt)
```

<img src="man/figures/README-Charg-1.png" width="100%" />

``` r

PlotMissedCleavages(peptides)
```

<img src="man/figures/README-missed_cleavages-1.png" width="100%" />

``` r
PlotTotalIonCurrent(msmsScans)
```

<img src="man/figures/README-TotalIonCurrent-1.png" width="100%" />

``` r
PlotProteinCoverage(peptides)
```

<img src="man/figures/README-protein_degradation-1.png" width="100%" />

``` r
PlotCombinedDynamicRange(proteinGroups)
```

<img src="man/figures/README-DynamicRange-1.png" width="100%" />

``` r
PLotAllDynamicRange(proteinGroups,columns = 1,rows = 3)
```

<img src="man/figures/README-DynamicRangeAll-1.png" width="100%" /><img src="man/figures/README-DynamicRangeAll-2.png" width="100%" />
