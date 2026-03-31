#' Build SAM configuration for Icelandic haddock
#'
#' Creates a \code{stockassessment} configuration object for the Icelandic
#' haddock stock assessment. Starts from the default configuration returned by
#' \code{\link[stockassessment]{defcon}} and applies haddock-specific settings
#' for age-structured fishing mortality, observation variance grouping, and
#' AR correlation structures for the two survey fleets.
#'
#' @param dat A SAM data object as returned by \code{\link{hr_sam_dat}}.
#' @return A SAM configuration list suitable for passing to
#'   \code{\link[stockassessment]{sam.fit}}.
#' @export
hr_sam_conf <- function(dat) {
  within(stockassessment::defcon(dat), {
    maxAgePlusGroup = c(1, 1, 1)
    stockRecruitmentModelCode = 3
    keyLogFsta[1, ] = 0:(length(keyLogFsta[1, ]) - 1)

    predVarObsLink[1, !is.na(predVarObsLink[1, ])] <- 0 #-1 #0 #0:(sum(predVarObsLink[1,]==-1,na.rm=TRUE)-1)
    predVarObsLink[2, !is.na(predVarObsLink[2, ])] <- max(
      predVarObsLink[1, ],
      na.rm = TRUE
    ) +
      1 #+ c(rep(0,7),rep(1,5))
    predVarObsLink[3, !is.na(predVarObsLink[3, 1:6])] <- max(
      predVarObsLink[2, ],
      na.rm = TRUE
    ) +
      1 #+ #c(rep(0,6))

    keyVarObs[1, keyVarObs[1, ] != -1] <- c(
      rep(0, 2),
      rep(1, 2),
      rep(2, 6),
      rep(3, 2)
    )
    keyVarObs[2, keyVarObs[2, ] != -1] <- max(keyVarObs[1, ]) + 1 #+  #c(rep(0,4),rep(1,8))
    keyVarObs[3, keyVarObs[3, ] != -1] <- max(keyVarObs[2, ]) + 1 #+  #c(rep(0,4),rep(1,6))

    obsCorStruct[2:3] <- 'AR'
    keyCorObs[2, is.na(keyCorObs[2, ])] <- c(rep(0, 7), rep(1, 4))

    keyLogFpar[2, keyLogFpar[2, ] != -1] <- c(0:4, rep(5, 7))
    keyLogFpar[3, keyLogFpar[3, ] != -1] <- max(keyLogFpar[2, ]) +
      1 +
      c(rep(0, 3), rep(1, 7))

    keyVarF[1, ] <- c(rep(0, 2), 1, rep(2, 9))
  })
}
