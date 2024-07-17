#' Fit length weight relationship to species data from SVDBS
#'
#'Fits a length weight relationship for use in length expansion
#'
#'@param lengthWeightData Data frame. length-weight pairs. Each row represents an individual fish
#'@param speciesName Character string. Common name for species
#'@param sex Numeric vector. The sex categories to be used. Default is all sexes (NULL)). Options 0,1,2
#'@param season Character vector. Season to be used. Default is all seasons (NULL).
#'
#'@return List of model fit objects
#'\item{commonSlope}{\code{\url{lm}} object. Fit for single slope (beta)}
#'\item{seasonalSlope}{\code{\url{lm}} object. Fit for seasonal slopes}
#'
#'@section Notes on model fitting :
#'
#'The simplest Null model (H0) is assumed to be
#'\deqn{W_i = \alpha L_i^\beta  exp(e_i)}
#'
#'where W_i = Weight and L_i = Length of fish i, e_i ~ \eqn{N(0,\sigma^2)} and
#' \eqn{\alpha} &  \eqn{\beta} are intercept and slope parameters (on the log scale) to be estimated.
#'
#'The alternative H1: \eqn{\beta_j != \beta} where \eqn{\beta_j} is the slope parameter (on the log scale) for season i.
#'i = 1, ..., 4 (spring, summer, fall, winter)
#'
#'The above hypothesis is tested and the pvalue is output in the log file.
#'Plots of model fits are also produced and saved in the output directory
#'
#'
#' @export

fit_length_weight <- function(lengthWeightData,speciesName,sex=NULL,season=NULL){

  if (is.null(sex)) {
    sex = c(0,1,2)
  }
  if (is.null(season)) {
    season = c("SPRING","SUMMER","FALL","WINTER")
  } else {
    season = toupper(season)
  }


  # filter for null values
  lwd <- lengthWeightData |>
    dplyr::filter(INDWT > 0, SEX %in% sex, SEASON %in% season) |>
    dplyr::select(INDWT,LENGTH,SEX,SEASON)
  n <-  lwd |>
    dplyr::group_by(SEASON,SEX) |>
    dplyr::count() |>
    dplyr::ungroup()
  n <- tidyr::pivot_wider(data = n,id_cols=SEASON,names_from = SEX,values_from = n)

  # fit Weight = a.Length^b.exp(E)  where E ~ N(0,sig^2)
  # fit  no seasonal effect
  fit <- lm(log(INDWT) ~ log(LENGTH) , data=lwd)
  evar <- sum(fit$residuals^2)/fit$df.residual
  lwd$predWt <- exp(fit$fitted.values + evar/2)

  if(dim(n)[1] > 1){
    # fit seasonal effect
    fit2 <- lm(log(INDWT) ~ log(LENGTH):SEASON, data=lwd )
    evar <- sum(fit2$residuals^2)/fit2$df.residual
    lwd$predSeasWt <- exp(fit2$fitted.values + evar/2)

    # test the null H0: bi=b vs alternative H1: bi != b
    reductionSS <- sum(fit$residuals^2) - sum(fit2$residuals^2)
    dfModel <- fit$df.residual- fit2$df.residual
    SSR <- sum(fit2$residuals^2)
    df <- fit2$df.residual
    Fstat <- (reductionSS/dfModel)/(SSR/df)
    pVal <- 1-pf(Fstat,dfModel,df)
  } else{
    pVal= NA
    fit2 <- list()
    fit2$coefficients <- NA
    lwd$predSeasWt <- NA
  }


  # plots common slope fit and separate seasonal fits on facet plot
  nSeasons <- length(season)
  # figText <- data.frame(SEASON = sort(unique(lwd$SEASON)),
  #                       x = c(rep(min(lwd$LENGTH),nSeasons)),
  #                       y = c(rep(max(lwd$INDWT),nSeasons)),
  #                       text = c(rep(paste0("Common slope: W = ",signif(exp(fit$coefficients[1]),6),"L^",signif(fit$coefficients[2],6)),nSeasons)),
  #                       textSeas = c(paste0("Seasonal slope: W = ",signif(exp(fit2$coefficients[1]),6),"L^",signif(fit2$coefficients[2:(nSeasons+1)],6))),
  #                       n = n )

  #print(figText)
  # figText <- figText  |>
  #   dplyr::mutate_if(is.factor, as.character)
  # figText$SEASON <- as.factor(figText$SEASON)

  #   png(paste0(outputDir,"/length_weight_relationship_",speciesName,".png"),width = 600,height = 600,units="px")

  p <- ggplot2::ggplot(data = lwd,ggplot2::aes(x=LENGTH, y = INDWT, color = as.factor(SEX))) +
    ggplot2::geom_point(shape = 1) +
    ggplot2::facet_wrap(facets="SEASON") +
    ggplot2::geom_line(ggplot2::aes(y = predWt),color = "red")+
    ggplot2::labs(color = "Sex")

  if (all(is.na(lwd$predSeasWt))) {
  } else {
    p <- p + ggplot2::geom_line(ggplot2::aes(y = predSeasWt),color = "blue")
  }
    p <- p + ggplot2::xlab("Length (cm)") +
    ggplot2::ylab("Weight (kg)") +
    ggplot2::ggtitle(paste0("Length-weight (SVDBS) relationship for ",speciesName))
    # ggplot2::geom_text(data = figText,ggplot2::aes(x=x,y=y,label = text ),show.legend = F,size=3,color="black",hjust="inward") +
    # ggplot2::geom_text(data = figText,ggplot2::aes(x=x,y=.9*y,label = textSeas ),show.legend = F,size=3,color="black",hjust="inward")

  # if (any(colnames(n)==0))      {
  #  p <- p+ ggplot2::geom_text(data = figText,ggplot2::aes(x=x,y=.8*y,label = paste("n_0 = ", n.0 )),show.legend = F,size=3,color="black",hjust="inward")
  # }
  #
  # if (any(colnames(n)==1))  {
  #   p <- p +
  #   ggplot2::geom_text(data = figText,ggplot2::aes(x=x,y=.78*y,label = paste("n_1 = ", n.1 )),show.legend = F,size=3,color="black",hjust="inward")
  # }
  #
  # if(any(colnames(n)==2)) {
  #   p <- p +
  #     ggplot2::geom_text(data = figText,ggplot2::aes(x=x,y=.76*y,label = paste("n_2 = ", n.2 )),show.legend = F,size=3,color="black",hjust="inward")
  #
  # }


  return(list(plot=p,nObs=n,commonSlope=fit,seasonSlope=fit2,pval=pVal))

}
