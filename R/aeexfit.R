##############################################################################
# User's Main Function for AEEX
##############################################################################




#' Fits semiparametric regression models robust to informative observation
#' times and censoring
#' 
#' Fits semiparametric regression models robust to informative observation
#' times and censoring
#' 
#' The control argument is a list athat can supply any of the following
#' components: \itemize{ \item betaInit: Initial value for covariate
#' coefficient, default is 0.
#' 
#' \item interval: Initial search interval for solving beta. Default is (-5,5).
#' 
#' \item maxIter: Maximum iterations allowed. Default is 150.
#' 
#' \item absTol: Absolute tolerance. Default is 1e-6.
#' 
#' \item relTol: Relative tolerance. Default is 1e-6.
#' 
#' \item a: A tune parameter. Default is .1. In case of gamma fraility, "a"
#' corresponds to the value of both shape and rate parameters. }
#' 
#' @param formula A formula object as returned by aee.
#' @param data A data frame which includes individuals' ID, observation times,
#' and minutes of physical activity since the last observation time.
#' @param weight A vector of sampling weights, for each individual.By default,
#' no sampling weights are applied.
#' @param se The method of estimating standard errors can be chosen by the
#' argument se. Two options are available: i) the sandwhich estimation (se =
#' 'Sandwich'), or ii) the bootstrap procedure (se = 'Bootstrap').
#' @param control A list of control parameters. See 'Details'.
#' @param boot The number of resamples generated for the bootstrap procedure.
#' @author Jaejoon Song <jjsong2@@mdanderson.org>
#' @references Wang, X. and Yan, J. (2011). Fitting semiparametric regressions
#' for panel count survival data with an R package spef. Computer Methods and
#' Programs in Biomedicine, 104, 278-285.
#' 
#' Wang, X., Ma, S., and Yan, J. (2013). Augmented estimating equations for
#' semiparametric panel count regression with informative observation times and
#' censoring time. Statistica Sinica, 23, 359-381.
#' @keywords aeefit
#' @examples
#' 
#' \dontrun{
#' 
#' data(NHANES)
#' formula <- aee(ID, Day, mvpaMinutes) ~ VO2max
#' # Standard errors are obtained using sandwich estimation
#' fitted <- aeexfit(formula = formula, data = NHANES, se = "Sandwich")
#' summary(fitted)
#' }
#' 
#' @export
#' @importFrom utils head tail 
#' @importFrom plyr ddply
#' @importFrom nleqslv nleqslv
#' @importFrom stats model.matrix
#' @importFrom methods getClass
aeexfit <- function(formula, data, weight=NULL, se="Sandwich", control=list(), boot=NULL) {
  
  method <- "AEEX"
  Call <- match.call()
  fm <- formula
  
  #if (is.null(weight)) {
  #  weight <- rep(1,nrow(X))
  #} 
  
  #if (is.null(weight)) {
  # weight <- rep(1,nrow(data[!duplicated(data[,colnames(data)==formula[[2]]$ID]),]))
  #} 
  
  # A PanelSurv object
  obj <- eval(formula[[2]], data)
  
  # Combine respones data frame and covariate data frame (remove intercept column)
  # Multiple rows per subject
  formula[[2]] <- NULL
  
  if (formula == ~ 1) {
    DF <- cbind(obj$psDF, zero=0)
  } else {
    DF <- cbind(obj$psDF, model.matrix(formula, data))[, -4]
  }
  
  DF <- DF[order(DF$ID, DF$time), ]
  
  # Design matrix, one row per subject
  X <- as.matrix(ddply(DF, "ID", head, n=1)[, -c(1:3)])
  
  # Create an Engine object
  engine.control <- control[names(control) %in% names(attr(getClass(method), "slots"))]
  engine <- do.call("new", c(list(Class=method), engine.control))
  
  if (length(engine@betaInit) == 1 & ncol(X) > 1)
    engine@betaInit <- rep(engine@betaInit, ncol(X))
  if (length(engine@betaInit) > 1 & length(engine@betaInit) != ncol(X))
    stop("Invalid length of initial beta values!")
  
  # Create a StdErr object
  #if(se == "NULL"){
  #  stdErr <- NULL}
  #if(se != "NULL"){
  stdErr.control <- control[names(control) %in% names(attr(getClass(se), "slots"))]
  stdErr <- do.call("new", c(list(Class=se), stdErr.control))
  #}
  
  if(se=="Sandwich"){
    fit <- doPanelFit.AEEX.Sandwich(DF=DF, panelMatrix=obj$panelMatrix, timeGrid=obj$timeGrid,
                                    X=X, engine=engine, stdErr ,weight=weight)
  }
  
  if(se=="Bootstrap"){
    fit <- doPanelFit.AEEX.Bootstrap(DF=DF, panelMatrix=obj$panelMatrix, timeGrid=obj$timeGrid,
                                     X=X, engine=engine, stdErr ,weight=weight, boot)
  }  
  
  ret = list(formula=fm, beta=fit$beta, 
             baseline=fit$baseline,
             timeGrid=fit$timeGrid,
             lambda=fit$lambda,
             convergence=fit$convergence,
             iter=fit$iter,
             betaSE=fit$betaSE,
             betaVar=fit$betaVar,
             baselineSE=fit$baselineSE)
             #U1=fit$U1,U2=fit$U2,U=fit$U,V=fit$V,A=fit$A,B=fit$B,V=fit$V,X=fit$X,
             #panelMatrix=fit$panelMatrix,lambda=fit$lambda)
  
  class(ret) <- "aeexfit"
  ret
  
}
