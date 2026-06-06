





#' Inverse logit (sigmoid) function
#'
#' @param x numeric vector or scalar
#'
#' @returns numeric vector or scalar of same length as x
#' @export
#'
#' @examples inverse.logit(-3.4)
inverse.logit <- function(x) {1/(exp(-x)+1)}


#' Is object formula
#'
#' @param x any object
#'
#' @returns boolean
#' @export
#'
#' @examples is.formula(~x1+x2)
is.formula <- function(x) {is.call(x) && x[[1]] == quote(`~`)}


#' Non-linear combinations
#'
#' @param x formula or charecter.
#' @param coefs numeric vector
#' @param vcov symmetric positive definite numeric matrix with same number of rows and columns as length of coefs
#' @param exp.form boolean indicating whether point estimate and corresponding confidence intervals should be exponentially transformed. Default is FALSE
#' @param inv.logit.form boolean indicating whether point estimate and corresponding confidence intervals should be sigmoid transformed. If exp.form is TRUE, then inv.logit.form is ignored. Default is FALSE
#' @param conf.level numeric scalar between 0 and 1 indicating level of confidence. Default is 0.95
#'
#' @returns numeric vector containing point estimate, untransformed std. error, confidence interval
#' @export
#'
#' @examples increment <- 1.3
#' @examples nlcombination(~(x1+x2)/increment,c(3,3),matrix(c(2.4265256,0.7562080,0.7562080,2.1329994),nrow=2))
nlcombination <- function(x,coefs,vcov,exp.form=FALSE,inv.logit.form=FALSE,conf.level=0.95) {

  if (is.character(x)) {
    if (x=="difference") {
      xx <- ~ x2 - x1
      print("Subtracting first element from second element")
    } else if (x=="ratio") {
      xx <- ~ x2/x1
      print("Dividing second element with first element")
    } else if (x=="oddsratio") {
      xx <- ~ (x2/(1-x2))/(x1/(1-x1))
      print("Dividing the odds equivalent of second element with the odds equivalent of first element")
    } else if (x=="oddsdiff") {
      xx <- ~ (x2/(1-x2)) - (x1/(1-x1))
      print("Subtracting the odds equivalent of first element from the odds equivalent of second element")
    } else {
      stop("x is recognized as character and must be difference, ratio, oddsratio, or oddsdiff")
    }
  } else if (is.formula(x)) {
    if (!any(all.vars(x) %in% paste0("x",1:length(coefs)))) {
      stop("x is formula and must contain atleast one variable called x1, x2, ... ")
    }
    for (vars in all.vars(x)) {
      if (vars %in% c("x","coefs","vcov","exp.form","inv.logit.form","conf.level","xx","stderr")) {
        stop("x is formula and must not contain the variable xx, stderr, or any of the function arguments")
      }
      if (!(vars %in% paste0("x",1:length(coefs)))) {
        if(!(eval(call("is.numeric", as.name(vars))) && length(vars) == 1)) {
          stop(sprintf("%s is not recognized as a numeric scalar",c(vars)))
        }
      }
    }
    xx <- x
  } else {
    stop("x is not recognized as character or formula. Character must be difference, ratio, oddsratio, or oddsdiff")
  }

  stderr <- msm::deltamethod(xx,coefs,vcov)
  names(coefs) <- paste0("x",1:length(coefs))
  pe <- lazyeval::f_eval_rhs(x,data.frame(t(coefs)))
  z <- stats::qnorm(0.5+conf.level/2)

  if (exp.form) {
    return(c("estimate"=exp(pe),"std.error"=stderr,"conf.low"=exp(pe-z*stderr),"conf.high"=exp(pe+z*stderr)))
  } else if (inv.logit.form) {
    return(c("estimate"=inverse.logit(pe),"std.error"=stderr,"conf.low"=inverse.logit(pe-z*stderr),"conf.high"=inverse.logit(pe+z*stderr)))
  } else {
    return(c("estimate"=pe,"std.error"=stderr,"conf.low"=pe-z*stderr,"conf.high"=pe+z*stderr))
  }

}









