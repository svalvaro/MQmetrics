test_that('returns a a printed message',{


    MQPathCombined <- system.file('extdata/combined/', package = 'MQmetrics')
    files <- ReadDataFromDir(MQPathCombined)
    parameters <- files[["parameters.txt"]]
    runningTimes <-  files[["#runningTimes.txt"]]



    expect_output(MaxQuantAnalysisInfo(MQPathCombined, runningTimes, parameters))

})
