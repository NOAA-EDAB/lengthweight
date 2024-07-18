#' Data pull
#'
#' Pulls length weight data and saves as rds

get_all_lengthweight_data <- function(channel) {

# Decadaal plls
  lengthWeightData1 <- survdat::get_length_weight_data(channel,year=1992:2001, species="all")
  lengthWeightData2 <- survdat::get_length_weight_data(channel,year=2002:2011, species="all")
  lengthWeightData3 <- survdat::get_length_weight_data(channel,year=2012:2023, species="all")

  # concatenate
  lw <- rbind(lengthWeightData1$data,lengthWeightData2$data,lengthWeightData3$data) |>
    dplyr::mutate(SVSPP = as.integer(SVSPP)) |>
    dplyr::filter(SVSPP %in% lengthweight::wigley$SVSPP)

  # join with lazy loaded data
  lwData <- lw |>
    dplyr::left_join(lengthweight::wigley, by ="SVSPP")

  # save data
  saveRDS(lwData,here::here("data-raw/data/lwAllData.rds"))

}

