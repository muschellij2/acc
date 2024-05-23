#' Summary method for aeefit objects
#' 
#' Prints estimated parameters for \code{aeefit} object
#' 
#' 
#' @param object A \code{aeefit} object
#' @param digits Minimum number of significant digits to be used for most
#' numbers.
#' @param dig.tst Minimum number of significant digits for the test statistics
#' @param ... ...
#' @return Estimated parameters for aeefit objects
#' @author Jaejoon Song <jjsong2@@mdanderson.org>
#' @keywords summary.aeefit
#' @examples
#' 
#' ##Will put an example here
#' 
#' @export
#' @importFrom stats printCoefmat pnorm
#' @importFrom methods setClass setMethod
#' @method summary aeefit
summary.aeefit <- function(object, digits = 3, dig.tst = 2, ...) {
  #if(is.null(digits)) digits <- options()$digits
  #else options(digits = digits)
  cat("\nCall:\n");   print(object$formula)
  cat("\nCoefficients:\n");  
  z <- object$beta/object$betaSE
  pval <- (pnorm(-abs(z)))*2
  #form <- unlist(strsplit(gsub("[[:space:]]", "", as.character(fitted2$formula)), "[~,+]"))
  form <- unlist(strsplit(gsub("[[:space:]]", "", as.character(object$formula)), "[~,+]"))
  mylength <- length(form)
  #betalength <- length(fitted2$beta)
  betalength <- length(object$beta)
  coefnames <- form[(mylength-betalength+1):mylength]
  cmat <- cbind(object$beta,exp(object$beta),object$betaSE,as.vector(z),as.vector(pval))              
  colnames(cmat) <- c("coef","exp(coef)","se(coef)","z","Pr(>|z|)")
  rownames(cmat) <- coefnames
  printCoefmat(cmat, digits, dig.tst,
               signif.stars = TRUE,
               signif.legend = TRUE)
}
