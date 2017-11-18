#' classoptimr: A package for identifying optimal classification schemes for models
#'              with continuous response and predictions
#'
#' Core of this package is a heuristic optimization procedure
#' (Simulated Annealing) that allows for identifying optimal classification schemes
#' for models that use continuous response variables and produce predictions on a
#' continuous scale.
#' The implemented methods were primarily developed to quantify the classification accuracy of
#' prediction maps based on statistical models that provide predictions on a
#' continuous scale (see \emph{references}). In many cases, these continuous predictions are afterwards
#' discretized into classes for better visualization purposes without considering
#' the resulting accuracies of the created classification scheme. In a more general
#' modelling context, the optimization method can also be used to detect non-constant
#' prediction performance of statistical models.
#'
#'
#'@section Functions:
#'
#' The package provides three main functions to apply:
#'
#' \itemize{
#'  \item \code{\link{HSMclass}}       Function to identify an optimal classification scheme for a predefined number of classes
#'                                     GIVEN a set of response \emph{reference} and corresponding \emph{predicted} values.
#'  \item \code{\link{classAccuracy}}  Function to evaluate a classification scheme by calculating various classification
#'                                     accuracy measures.
#'  \item \code{\link{create_qml}}     Function to create a qml-file of a classification scheme for visualization in the open source
#'                                    Geographical Information System \emph{QGIS}.
#' }
#'
#'
#' @references Hill, A., Breschan, J., & Mandallaz, D. (2014). Accuracy assessment of timber
#'             volume maps using forest inventory data and LiDAR canopy height models.
#'             \emph{Forests}, \strong{5(9)}, 2253-2275.
#'
#' @docType package
#' @name classoptimr
NULL
#> NULL
