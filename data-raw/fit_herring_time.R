#' look at herring BTS Weight length data  by year
#'
#' Fit time component to model and test


herring <- readRDS(here::here("data-raw/data/lwATLANTICHERRING.rds"))

# remove missing values
hd <- herring |>
  dplyr::mutate(YEAR = as.integer(substr(CRUISE6,start=1,stop=4))) |>
  dplyr::filter(!(is.na(INDWT)|is.na(LENGTH))) |>
  dplyr::filter(INDWT < 5)

## all years on same plot
hd |>
  ggplot2::ggplot()+
  ggplot2::geom_point(ggplot2::aes(y=INDWT,x=LENGTH,col = YEAR)) +
  ggplot2::scale_colour_distiller(palette = "Blues")

# faceted by year
hd |>
  ggplot2::ggplot()+
  ggplot2::geom_point(ggplot2::aes(y=INDWT,x=LENGTH)) +
  ggplot2::facet_wrap(~YEAR)

# fit each year and disply fits on same plot
df <- NULL
LENGTHS <- unique(hd$LENGTH)
for (iyr in unique(hd$YEAR)) {
  herringyr <- hd |>
    dplyr::filter(YEAR == iyr)

  fits <- lengthweight::fit_length_weight(herringyr,"Herring")
  coefs <- fits$commonSlope$coefficients
  sd <- summary(fits$commonSlope)$sigma
  fittedVals <- exp(coefs[1] + coefs[2]* log(LENGTHS) + (sd^2)/2)
  df <- rbind(df,data.frame(YEAR = iyr,LENGTH = LENGTHS, fit = fittedVals))
}

df <- df |>
  dplyr::arrange(YEAR,LENGTH)

#faceted fits over time on one plot
df |>
  ggplot2::ggplot()+
  ggplot2::geom_point(ggplot2::aes(x=LENGTH,y = fit, col = YEAR)) +
  ggplot2::scale_colour_distiller(palette = "Blues")

#faceted fits over time
df |>
  ggplot2::ggplot()+
  ggplot2::geom_point(ggplot2::aes(x=LENGTH, y = fit)) +
  ggplot2::facet_wrap(~YEAR)
