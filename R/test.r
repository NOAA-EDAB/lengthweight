#' pulls length and weight data from SVDBS, then fits data to model


source(here::here("R","fit_length_weight.r"))
library(magrittr)

outputDir <- here::here("output2")

speciesList <- c(160617,564145,161722,164712,164744,172909,172905,172414,164791,164499)
speciesTable <- dbutils::create_species_lookup(channel,species=speciesList,speciesType = "SPECIES_ITIS")
table <- speciesTable$data |>
  dplyr::select(COMNAME,SVSPPsv) |>
  dplyr::distinct()

for (isp in 1:nrow(table)) {
  species <- table[isp,]
  print(species)

  lengthWeightData <- survdat::get_length_weight_data(channel,year=1992:2020, species=species$SVSPPsv)

  fits <- fit_length_weight(lengthWeightData$data,speciesName=species$COMNAME,outputDir,logfile = "logfile.txt",
                            sex="all",
                            season=c("FALL","SPRING"))

}

lwd <- lengthWeightData$data %>% dplyr::filter(INDWT > 0, SEX %in% 0, SEASON =="FALL") %>% dplyr::select(INDWT,LENGTH,SEX,SEASON)

for (len in 20:30) {
  d <- lwd %>% dplyr::filter(LENGTH==len,SEASON == "FALL") %>% dplyr::select(INDWT) %>% unlist()
  hist (d,breaks = 50,main=len)

}


p <- ggplot2::ggplot(data = fits,ggplot2::aes(x=LENGTH, y = INDWT)) +
  ggplot2::geom_point(shape = 1) +
  ggplot2::geom_line(ggplot2::aes(y = predWt),color = "red") +
  ggplot2::xlab("Length (cm)") +
  ggplot2::ylab("Weight (kg)") +
  ggplot2::ggtitle(paste0("Length-weight (SVDBS) relationship for ",species$COMNAME))


