#' optcrit
#'
#' @description
#' Displays the values of the 3 optimization criteria of \code{\link{HSMclass}}
#'
#' @param object  object of class \code{'summary.classaccur'} containing an evaluated classification scheme.
#' @param ...  additional arguments, so far ignored.
#'
#' @export
#'
optcrit<- function(object, ...){
  UseMethod("optcrit")
}


#' @rdname optcrit
#'
#' @export
#' @method optcrit summary.classaccur
optcrit.summary.classaccur<- function(object, ...){

  stopifnot(inherits(object, "summary.classaccur"))

  camat<- object$accmat

  crit1<- sum(camat$usersaccuracy, na.rm = T)

  crit2<- sum(camat$classwidth^2)

  crit3<- sum(sapply(camat$no.references, function(noref){ (mean(camat$no.references) - noref)^2}))

  cat("Opt.Criterion 1 --> Maximize: \n")
  print(paste("Sum of User's Accuracies:",crit1), quote = F)
  cat("\n")
  cat("Opt.Criterion 2 --> Minimize: \n")
  print(paste("Square Sum of class widths:",crit2), quote = F)
  cat("\n")
  cat("Opt.Criterion 3 --> Minimize: \n")
  print(paste("Distribution of refdata:",crit3), quote = F)
  cat("\n")


}
