



#' Summary method for aeexfit objects
#' 
#' Prints estimated parameters for \code{aeexfit} object
#' 
#' 
#' @param object A \code{aeexfit} object
#' @param digits Minimum number of significant digits to be used for most
#' numbers.
#' @param dig.tst Minimum number of significant digits for the test statistics
#' @param ... ...
#' @return Estimated parameters for aeefit objects
#' @author Jaejoon Song <jjsong2@@mdanderson.org>
#' @keywords summary.aeexfit
#' @examples
#' 
#' ##Will put an example here
#' @export
#' @importFrom stats printCoefmat pnorm
#' @importFrom methods setClass setMethod
#' @method summary aeexfit
summary.aeexfit <- function(object, digits = 3, dig.tst = 2, ...) {
  #if(is.null(digits)) digits <- options()$digits
  #else options(digits = digits)
  cat("\nCall:\n");   print(object$formula)
  cat("\nCoefficients:\n");  
  z <- object$beta/object$betaSE
  pval <- (pnorm(-abs(z)))*2
  # form <- unlist(strsplit(gsub("[[:space:]]", "", as.character(fitted2$formula)), "[~,+]"))
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
