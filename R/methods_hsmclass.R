#' Summarizing Optimization of Classification Scheme
#'
#'@description
#'\code{summary} methods for class \code{'hsmclass'}
#'
#' @param object object of class \code{'hsmclass'}
#' @param ...  additional arguments, so far ignored.
#'
#' @return  \code{summary.hsmclass} returns an object of class \code{"summary.hsmclass"}.
#'
#' An object of class \code{"summary.hsmclass"} returns a \code{list} of the following components:
#'
#' \item{call}{ the function call passed to function \code{HSMclass}}
#' \item{iterations}{number of iterations used in heuristic}
#' \item{bestever.iterationmode}{number of times the heuristic should be repeated.
#'                               \code{NA} indicates that option was not used.}
#' \item{classmat}{a \code{\link[base]{data.frame}} summarizing the identified optimal classification scheme}
#'
#' @seealso \code{\link{HSMclass}}
#'
#' @export
#'
summary.hsmclass<- function(object, ...){
  # creates summary-object for hsmclass-object

  stopifnot(inherits(object, "hsmclass"))

  # ---- create result table ------ #
  no.cl<- object$settings$nclasses
  cbreaks<- data.frame(classbreaks = rep(NA_character_, no.cl),
                       classwidth = rep(NA_real_, no.cl))
  cbreaks[,"classbreaks"]<-as.character(cbreaks[,"classbreaks"])
  rownames(cbreaks)<- paste(rep("Class", no.cl), seq(1,no.cl))


  cbreaks["classbreaks"]<-  levels(cut(x=object$settings$predictions,
                                         breaks=object$best.classbreaks,
                                         include.lowest=TRUE))

  cbreaks["classwidth"]<- object$best.classwidth

  # ----------------------------- #

  result<- list(call = object$call,
                 iterations = object$settings$iterations,
                 bestever.iteration = object$settings$bestever.iteration,
                 classmat = cbreaks)

  class(result)<- "summary.hsmclass"

  # return cbreaks:
  result

}






