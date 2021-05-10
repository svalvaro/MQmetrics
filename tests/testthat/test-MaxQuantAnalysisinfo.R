test_that('returns a a printed message',{


    MQPathCombined <- system.file('extdata/combined/', package = 'MQmetrics')
    MQCombined <- make_MQCombined(MQPathCombined)


    expect_output(MaxQuantAnalysisInfo(MQCombined))

})
