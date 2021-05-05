test_that('returns a list of 6 tables',{

  MQPathCombined <- system.file('extdata/combined/', package = 'MQmetrics')
  tables <- MQmetrics::ReportTables(MQPathCombined)

  expect_equal(length(tables),6)

})
