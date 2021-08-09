# MQmetrics 0.99.2

* Added a `NEWS.md` file to track changes to the package.


# MQmetrics 0.99.3 

* Functions now use a general input resulting from `make_MQCombined()`.
* Shortened lines in the code.
* Usage of *seq_len* rather than *1: ...*
* Usage of ultiples of 4 spaces for line indents.
* Removed *LazyData: TRUE*.

# MQmetrics 1.0.0 (Bioconductor stable version 3.13)

* The function `generateReport()` now takes the parameter *name_output_file* 
to name the output pdf file.
* Added information regarding peptides, and peptides/protein ratio in report 
tables.
* Added the function `PlotProteinPeptideRatio()` to visualize a comparison 
between the proteins identified and the ratio Peptide/Proteins among
Experiments.



# MQmetrics 1.1.1 

* Added pagination to `PlotiRT()` and `PlotiRTScore()`.
* Updated vignette style to Bioconductor's.
* Improved aesthethics of `PlotProteinOverlap()` and `PlotPCA()`.

# MQmetrics 1.1.2 

* Fixed units of time `MaxQuantAnalysisInfo()`  when experiment lasting
longer than a day.
* Added new line to `MaxQuantAnalyssInfo()` showing when the experiment ended.
* Improved aesthethics in the plots from `PlotCombinedDynamicRange()` and 
`PlotAllDynamicRange()`.

# MQmetrics 1.1.3 
* Added new function `PlotPTMAcrossSamples()`, it takes as input one PTM of 
interest and shows its intensities across the samples. 
This function is similar to `PlotPTM()` but in more detail.
* In the function `PlotPTM()` a parameter `combine_same_residue_ptms` has been
added. It combines multiple PTMs happening in the same residue such as:
Dimethyl (KR), Trimethyl (KR).


# MQmetrics 1.1.4 (Bioconductor Development Version 3.14) (Github version)


* The function `PlotProteinCoverage()` now reports the coverage individually in
each plot rather than the total protein coverage.

* MQmetrics now is adapted to MaxQuant v.2.0.1.0, since the column names 
are different than in MaxQuant v.1.6.17.0. MQmetrics will detect the MaxQuant 
version used and read the columns accordingly.

* Enhanced error message in `PlotiRT()` and `PlotiRTScore()` when irt 
peptides are note found. Enhanced error message for `PlotProteinCoverage()`
when the  `UniprotID` is not found.
