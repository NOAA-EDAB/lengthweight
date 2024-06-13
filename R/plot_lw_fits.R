#' pulls length and weight data from SVDBS, then fits data to model


source(here::here("R","fit_length_weight.r"))
library(magrittr)

outputDir <- here::here("output")

speciesList <- readr::read_csv(file=here::here("other","LW_species.csv"))

for (isp in 1:dim(speciesList)[1]) {
  species <- speciesList[isp,]
  print(species)

  lengthWeightData <- svdbs::get_length_weight(channel,year=1992:2020, species=species$SVSPP)
  seasonsPresent <- lengthWeightData$data %>% dplyr::distinct(SEASON)


  fits <- fit_length_weight(lengthWeightData$data,speciesName=species$COMNAME,outputDir,logfile = "logfile.txt",
                            sex="all",
                            season=c("SPRING","FALL"))
}



