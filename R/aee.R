#' Creates an aee object
#' 
#' Creates an aee object for semiparametric regression with augmented
#' estimating equation.
#' 
#' 
#' @param ID Individual identifier
#' @param time Observation time
#' @param minutes Minutes of physical activity
#' @return \item{psDF}{A data frame, part of original input data frame with
#' variable "ID", "time" and "count"} \item{timeGrid}{Ordered distinct
#' observation times in the set of all observation times} \item{panelMatrix}{a
#' matrix representation of panel count data, one row per subject, one column
#' per time point in "timeGrid"}
#' @author Jaejoon Song <jjsong2@@mdanderson.org>
#' @references Wang, X. and Yan, J. (2011). Fitting semiparametric regressions
#' for panel count survival data with an R package spef. Computer Methods and
#' Programs in Biomedicine, 104, 278-285.
#' 
#' Wang, X., Ma, S., and Yan, J. (2013). Augmented estimating equations for
#' semiparametric panel count regression with informative observation times and
#' censoring time. Statistica Sinica, 23, 359-381.
#' @keywords aee
#' @examples
#' 
#' \dontrun{
#' # We illustrate the use of function aeexfit
#' # with the sample data from the National Health and Nutrition Examination Survey (NHANES) 
#' # to examine the association between the cardiorespiratory function (i.e., VO2max) 
#' # and daily minutes of moderate to vigorous physical activity (MVPA).
#' 
#' data(NHANES)
#' formula <- aee(ID, Day, mvpaMinutes) ~ VO2max
#' # Standard errors are obtained using sandwich estimation
#' fitted <- aeexfit(formula = formula, data = NHANES, se = "Sandwich")
#' summary(fitted)
#' }
#' @export
#' @importFrom stats model.matrix
#' @importFrom methods getClass
aee <- function(ID, time, minutes) {
  if (sum(time <= 0) > 0)
    stop("Observation time must be positive.")
  
  index <- which(!duplicated(ID))
  N <- length(index)
  uniqueID <- ID[index]
  timeGrid <- sort(unique(time))
  
  #runLength <- rle(as.numeric(ID))$lengths
  #cumLength <- cumsum(runLength)
  
  panelMatrix <- matrix(NA, N, length(timeGrid))
  for (i in 1:N) {
    rowSet <- which(ID == uniqueID[i])
    panelMatrix[i, which(timeGrid %in% time[rowSet])] <- minutes[rowSet]
  }
  
  #panelMatrix <-  makePanelMatrix(time,minutes,uniqueID,timeGrid,cumLength)
  
  ps <- list(psDF=data.frame(ID=ID, time=time, minutes=minutes),
             timeGrid=timeGrid, panelMatrix=panelMatrix)
  class(ps) <- "aee"
  ps
}

is.aee <- function(x) inherits(x, "aee")
