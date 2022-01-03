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
        "#runningTimes.txt",
        "mqpar.xml"
    )

    # Check if the the names are in the MQCombined,
    # then with all return only one TRUE

    expect_true(all(names(MQCombined) %in% name_files))

})
