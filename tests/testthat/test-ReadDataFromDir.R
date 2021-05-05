test_that('returns a list of 9 elements',{

  MQPathCombined <- system.file('extdata/combined/', package = 'MQmetrics')
  files <- MQmetrics::ReadDataFromDir(MQPathCombined)

  expect_equal(length(files),9)

})
