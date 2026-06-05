





#' Inverse logit (sigmoid) function
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
inverse.logit <- function(x) {1/(exp(-x)+1)}


#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
is.formula <- function(x) {is.call(x) && x[[1]] == quote(`~`)}


#' Non-linear combinations
#'
#' @param x
#' @param coefs
#' @param exp.form
#' @param inv.logit.form
#' @param conf.level
#' @param vcov
#'
#' @returns
#' @export
#'
#' @examples
nlcombination <- function(x,coefs,vcov,exp.form=FALSE,inv.logit.form=FALSE,conf.level=0.95) {

  if (is.character(x)) {
    if (x=="difference") {
      xx <- ~ x2 - x1
    } else if (x=="ratio") {
      xx <- ~ x2/x1
    } else if (x=="oddsratio") {
      xx <- ~ (x2/(1-x2))/(x1/(1-x1))
    } else if (x=="oddsdiff") {
      xx <- ~ (x2/(1-x2)) - (x1/(1-x1))
    } else {
      stop("x is not recognized as character or formula. Character must be difference, ratio, oddsratio, or oddsdiff")
    }
  } else if (is.formula(x)) {
    xx <- x
    j <- 1
    for (elem in names(coefs)) {
      if (elem == "") {next}
      xx <- as.formula(gsub(elem, paste0("x",j), deparse(xx)))
      j <- j + 1
    }
  } else {
    stop("x is not recognized as character or formula. Character must be difference, ratio, oddsratio, or oddsdiff")
  }

  stderr <- msm::deltamethod(xx,coefs,vcov)
  if (is.character(x)) {names(coefs) <- paste0("x",1:length(coefs))}
  pe <- lazyeval::f_eval_rhs(x,data.frame(t(coefs)))
  z <- qnorm(0.5+conf.level/2)

  if (exp.form) {
    return(c("estimate"=exp(pe),"std.error"=stderr,"conf.low"=exp(pe-z*stderr),"conf.high"=exp(pe+z*stderr)))
  } else if (inv.logit.form) {
    return(c("estimate"=inverse.logit(pe),"std.error"=stderr,"conf.low"=inverse.logit(pe-z*stderr),"conf.high"=inverse.logit(pe+z*stderr)))
  } else {
    return(c("estimate"=pe,"std.error"=stderr,"conf.low"=pe-z*stderr,"conf.high"=pe+z*stderr))
  }

}









