#' Summarizing  Evaluation of Classification Accuracy
#'
#'@description
#'\code{summary} methods for class \code{'classaccur'}
#'
#' @param object object of class \code{'classaccur'}
#' @param ...  additional arguments, so far ignored.
#'
#' @return  \code{summary.classaccur} returns an object of class \code{"summary.classaccur"}.
#'
#' An object of class \code{"summary.classaccur"} returns a \code{list} of the following components:
#'
#' \item{accmat}{a \code{\link[base]{data.frame}} summarizing the user's- and producer's accuracy}
#' \item{rsquared_val}{the coefficient of determination of the prediction model, calulated based on
#'                      \code{refdata} and \code{predictions}}
#' \item{predictions}{ \code{vector} containing the predictions given to \code{classAccuracy} }
#' \item{overall.accuracy}{the overall accuracy of the classification scheme}
#' \item{conf.oaa}{ \code{vector} containing \code{[1]} the lower confidence limit of the OAA,
#'                                            \code{[2]} the upper confidence limit of the OAA,
#'                                        and \code{[3]} the confidence level}
#'  \item{cohenskappa}{Cohen's Kappa Coefficient}
#'  \item{Quantity.Disagreement}{ the quantity disagreement}
#'  \item{Allocation.Disagreement}{the allocation disagreement}
#'
#'
#' @examples
#'
#' ## -- Summarize classification accuracy:
#'
#' acc.equal<- classAccuracy(refdata.gr, predictions.gr, equal.int = 100)
#' summ.<- summary(acc.equal)
#'
#' # print summary-object:
#' summ.
#'
#' # extract accuracy-data.frame:
#' summ.$accmat
#'
#' @export
#'
summary.classaccur<- function(object, ...){
  # creates summary-object for classaccur-object

  stopifnot(inherits(object, "classaccur"))

  # ---- create result table ------ #
  no.cl<- length(object$classwidth)
  caccs<- data.frame(classbreaks = rep(NA_character_, no.cl),
                     classwidth = rep(NA_real_, no.cl),
                     usersaccuracy =rep(NA_real_, no.cl),
                     prodaccuracy = rep(NA_real_, no.cl),
                     no.references = rep(NA_integer_, no.cl))
  caccs[,"classbreaks"]<-as.character(caccs[,"classbreaks"])
  rownames(caccs)<- paste(rep("Class", no.cl), seq(1,no.cl))

  caccs$classwidth<- object$classwidth
  caccs$prodaccuracy<- round(as.numeric(object$prodaccuracy[,2]), digits = 2)
  caccs$usersaccuracy<- round(as.numeric(object$usersaccuracy[,2]), digits = 2)
  caccs$no.references<- object$no.ref.classes


  if(any(is.na(object$def.classbreaks))){

    caccs["classbreaks"]<-  levels(cut(x=object$predictions,
                                       breaks=rep(object$equal.classbreaks,
                                                  include.lowest=TRUE)))
  } else {

    caccs["classbreaks"]<-  levels(cut(x=object$predictions,
                                       breaks=rep(object$def.classbreaks,
                                                  include.lowest=TRUE)))
  }

  # ----------------------------- #

  result<- list(rsquared_val = object$rsquared_val,
                predictions = object$predictions,
                overall.accuracy = object$overall.accuracy,
                conf.oaa = object$conf.oaa,
                map.accuracy = object$map.accuracy,
                cohenskappa = object$cohenskappa,
                Quantity.Disagreement = object$Quantity.Disagreement,
                Allocation.Disagreement = object$Allocation.Disagreement,
                accmat = caccs
                )

  class(result)<- "summary.classaccur"

  result

} # end of summary.twophase









































