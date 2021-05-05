test_that('returns a list of 9 elements',{

  MQPathCombined <- system.file('extdata/combined/', package = 'MQmetrics')

  files <- MQmetrics::ReadDataFromDir(MQPathCombined)

  name_files <-c('summary.txt', 'peptides.txt', 'evidence.txt','msmsScans.txt',
                 'msScans.txt','proteinGroups.txt','modificationSpecificPeptides.txt',
                 'parameters.txt', '#runningTimes.txt')

  expect_equal(names(files),name_files)

})
