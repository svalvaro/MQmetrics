test_that("returns a list of 6 tables", {
    MQCombined <- make_MQCombined(MQPathCombined = system.file("extdata/combined/",
                                              package = "MQmetrics"))
    tables <- ReportTables(MQCombined)

    expect_equal(length(tables), 6)
})
