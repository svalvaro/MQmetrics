PlotiRT <- function(msmsScans){


  iRT.mZ <- c(487.2571, 547.2984, 622.8539, 636.8695, 644.8230, 669.8384,
              683.8282, 683.8541, 699.3388, 726.8361, 776.9301)

  iRT.score <- c(-24.92, 19.79, 70.52, 87.23, 0, 28.71, 12.39, 33.38, 42.26,
                 54.62, 100)

  names(iRT.mZ) <- c("LGGNEQVTR", "YILAGVENSK", "GTFIIDPGGVIR", "GTFIIDPAAVIR",
                     "GAGSSEPVTGLDAK", "TPVISGGPYEYR", "VEATFGVDESNAK",
                     "TPVITGAPYEYR", "DGLDAASYYAPVR", "ADVTPADFSEWSK",
                     "LFLQFGAQGSPFLK")



  # tolerance <- 0.001
  #
  # in_range <- unlist(sapply(A, function(x) x[any(abs(x-B) < tolerance)]))
  # C <- sapply(A, function(x) which.min(abs(x-B)))
  # C <- C[match(in_range, A)]
  #
  # ggplot(msmsScans, aes(x = `Retention time`, y = `Total ion current`))+
  #   geom_line(aes(colour = Experiment))

  tolerance <- 0.001

  msmsScans[which(msmsScans$`m/z` == 354.89)


  C <- sapply(msmsScans$`, function(x) which.min(abs(x-iRT.mZ)))
  C <- C[match(in_range, A)]

  ggplot(msmsScans, aes(x = `Retention time`, y = `Total ion current`))+
    geom_line(aes(colour = Experiment))



}
