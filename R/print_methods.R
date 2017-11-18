
#' @export
print.summary.hsmclass<- function(x, ...){

  stopifnot(inherits(x, "summary.hsmclass"))

  cat("\n")
  cat("Optimal Classification Schmeme by Heuristic Search Method")
  cat("\n \n")
  cat("Call: ")
  cat("\n")
  print(x$call)
  cat("\n")
  cat(paste("Iterations used by HSM:",x$iterations))
  cat("\n \n")

  if(x$bestever.iteration > 1){
    cat(paste("Heuristic was applied",x$bestever.iteration),"times and best overall solution was chosen.")
    cat("\n \n")
  } else {
    cat(paste("Heuristic was applied",x$bestever.iteration),"time.")
    cat("\n \n")
  }

  cat("Optimal class breaks:")
  cat("\n")
  print(x$classmat)
  cat("\n")

}

# ----------------------------------------------------------------------- #


#' @export
print.summary.classaccur<- function(x, ...){

  stopifnot(inherits(x, "summary.classaccur"))

  cat("\n")
  cat("Classification Accuracies for Models with continous response variable")

  cat("\n \n")
  cat(paste("Number of classes:", nrow(x$accmat)))
  cat("\n \n")

  cat(paste("Overall Accuracy with ",100*x$conf.oaa[3],"% Confidence Interval: ", sep = ""))
  cat("\n")
  cat(paste(round(x$overall.accuracy,digits=3),
            " (",round(x$conf.oaa[1],digits=2),", ",
            round(x$conf.oaa[2],digits=2),")",sep=""))
  cat("\n\n")

  cat("Cohen's Kappa: ")
  cat(round(x$cohenskappa, digits = 2))
  cat("\n\n")

  cat("Class Accuracies in [%]:")
  cat("\n")
  print(x$accmat,quote=FALSE)
  cat("\n")

  cat("Additional accuracy measures for classification:")
  cat("\n")
  cat(paste("Map Accuracy:",round(x$map.accuracy*100,digits=2),"%; ",
            "Quantity Disagreement:",round(x$Quantity.Disagreement,digits=2),"%"))
  cat("\n\n")
  cat(paste("R-squared of prediction model:", round(x$rsquared_val, digits = 2)))
  cat("\n\n")


}


# ----------------------------------------------------------------------- #

#' @export
print.hsmclass<- function(x, ...){

  stopifnot(inherits(x, "hsmclass"))

  cat("\n")
  cat("Call: \n")
  print(x$call)
  cat("\n")

  if(x$bestever.iteration > 1){
    cat(paste("Heuristic was applied",x$bestever.iteration),"times and best overall solution was chosen.")
    cat("\n \n")
  } else {
    cat(paste("Heuristic was applied",x$bestever.iteration),"time.")
    cat("\n \n")
  }

}

# ----------------------------------------------------------------------- #

#' @export
print.classaccur<- function(x, ...){

  stopifnot(inherits(x, "classaccur"))

  cat("\n")
  cat("Call: \n")
  print(x$call)
  cat("\n")
  cat(paste("Classification Accuracy calculated for", nrow(x$accmat), "classes"))
  cat("\n \n")

}





