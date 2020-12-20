#' @title binWData
#'
#' @description .....
#'
#' @param dataframe
#'
#' @return dataframe
#'
#' @examples ....
#' ....
#' ....
#'
#' @export

binWData = function(data, base = 4, min_size = 0.1){



  naWmaxInd = is.na(data$Wmax)


  binCol = data$Wmax

  nneg =  floor(log(min_size,base=base))-1 # -1 to get an extra lower bin that can be thrown away.
  npos = ceiling(log(max(binCol[!naWmaxInd]), base))
  n    = npos - nneg + 1 # antal bins inkl NA

  binned = nneg - 1 + cut(binCol[!naWmaxInd],breaks = c(0,base^(nneg:npos)),labels=FALSE,na.rm=TRUE)

  data$Bin = npos+1 # For alle NAer
  data$Bin[!naWmaxInd] = binned

  HlBin = NULL

  #if data does not have year
  HlBin = data %>% group_by(Bin) %>% summarise(BiomassMean = mean(Biomass,na.rm=TRUE),
                                               Biomass = sum(Biomass, na.rm=TRUE),
                                               Wmax = base^Bin[1]
  )


  #if data has year (will fail and proceed if it doesnt, not good code)
  try({
    HlBin = data %>% group_by(Year, Bin) %>% summarise(BiomassMean = mean(Biomass,na.rm=TRUE),
                                                       Biomass = sum(Biomass,na.rm=TRUE),
                                                       Wmax = base^Bin[1]
    )
  },silent=TRUE)


  #Tilfoej NA til Wmax
  HlBin[HlBin$Bin == npos+1,]$Wmax = NA


  attr(HlBin,"base") = base


  # Throw lowest bin (the one containing everything down to 0 away)

  subset(HlBin, Bin > min(HlBin$Bin,na.rm=T))
  subset(data,  Bin > min(data$Bin,na.rm=T) )

  return(list(HlBin, n, data))


}
