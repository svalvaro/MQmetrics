test_that("returns a list of 10 elements", {
    MQPathCombined <- system.file("extdata/combined/", package = "MQmetrics")

    MQCombined <- MQmetrics::make_MQCombined(MQPathCombined)

    name_files <- c(
        "MQPathCombined",
        "summary.txt",
        "peptides.txt",
        "evidence.txt",
        "msmsScans.txt",
        "msScans.txt",
        "proteinGroups.txt",
        "modificationSpecificPeptides.txt",
        "parameters.txt",
        "#runningTimes.txt"
    )

    expect_equal(names(MQCombined), name_files)
})
