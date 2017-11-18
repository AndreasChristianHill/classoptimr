#' classAccuracy
#'
#'@description
#'Calculates common classification accuracies measures known from remote sensing
#'applications based on a Confusion Matrix that compares Reference classes to their
#'respective class predictions. The computed accuracy measures are:
#'\itemize{
#'         \item \emph{Overall Classification Accuracy (OAA)}
#'         \item \emph{Producer's Accuracy (PA)}
#'         \item \emph{User's Accuracy (UA)}
#'         \item \emph{Cohen's Kappa Coefficient (k)}
#'         \item \emph{Quantity Disagreement}
#'         \item \emph{Allocation Disagreement}
#'          }
#'
#' @param refdata  \code{vector} containing the values of the continuous
#'                  response variable used in the prediction model.
#' @param predictions \code{vector} containing the predictions for the response values
#'                    of the prediction model.
#'                    \strong{Note:}(\code{refdata} and \code{predictions} have to correspond to each other).
#' @param object object of class \code{'hsmclass'} created by function \code{\link{HSMclass}}.
#' @param equal.int an equidistant class interval to be evaluated as classifciation scheme. Defaults to \code{NA}.
#'                  Can only be set if \code{def.int} is \code{NA}.
#' @param def.int a \code{vector} defining an arbitrary set of class breaks to be evaluated as classifciation scheme.
#'                Defaults to \code{NA}. Can only be set if \code{def.int} is \code{NA}.
#' @param conf.level the confidence level used to compute the confidence intervals of the overall accuracy.
#' @param ... further arguments passed to or used by methods.
#'
#'
#' @return \code{classAccuracy} returns an object of class \code{"classaccur"}
#'
#' An object of class \code{"classaccur"} returns a \code{list} of the following components:
#'
#' \item{rsquared_val}{the coefficient of determination of the prediction model, calulated based on
#'                      \code{refdata} and \code{predictions} }.
#'  \item{predictions}{ \code{vector} containing the predictions given to \code{classAccuracy} }
#'  \item{classwidth}{ \code{vector} containing the class width}
#'  \item{def.classbreaks}{ \code{vector} containing arbitrary class break values}
#'  \item{equal.classbreaks}{ \code{vector} containing equidistant class break values}
#'  \item{overall.accuracy}{the overall accuracy of the classification scheme}
#'  \item{conf.oaa}{ \code{vector} containing \code{[1]} the lower confidence limit of the OAA,
#'                                            \code{[2]} the upper confidence limit of the OAA,
#'                                        and \code{[3]} the confidence level}
#'  \item{prodaccuracy}{a \code{\link[base]{matrix}} containing the producer's accuracy for each class}
#'  \item{usersaccuracy}{a \code{\link[base]{matrix}} containing the user's accuracy for each class}
#'  \item{no.ref.classes}{ a \code{vector} containing the number of reference data for each class}
#'  \item{cohenskappa}{Cohen's Kappa Coefficient}
#'  \item{map.accuracy}{the map accuracy}
#'  \item{Quantity.Disagreement}{ the quantity disagreement}
#'  \item{Allocation.Disagreement}{the allocation disagreement}
#'
#'
#' @references
#' Congalton,  R.G.;  Green,  K.  \emph{Assessing  the  Accuracy  of  Remotely  Sensed  Data:  Principles  and
#' Practices}; Lewis Publications: Boca Raton, FL, USA, 1999; p. 137.
#'
#' Richards, J.A. \emph{Remote Sensing Digital Image Analysis: An Introduction}, 5th ed.; Springer: Berlin, Germany, 2013.
#'
#' Hill, A., Breschan, J., & Mandallaz, D. (2014). Accuracy assessment of timber
#'             volume maps using forest inventory data and LiDAR canopy height models.
#'             \emph{Forests}, \strong{5(9)}, 2253-2275.
#'
#'
#' @examples
#'
#'
#' #----
#' # 1.) Example: classification accuracy for equidistant class width of 100:
#' #----
#'
#' acc.equal<- classAccuracy(refdata.gr, predictions.gr, equal.int = 100)
#' summary(acc.equal)
#'
#'
#' #----
#' # 2.) Example: classification accuracy for arbitrary class breaks:
#' #----
#'
#' acc.def<- classAccuracy(refdata.gr, predictions.gr,
#'                        def.int = c(0, 150, 200, 430, 610, 880))
#' summary(acc.def)
#'
#'
#' #----
#' # 3.) Example: classification accuracy for optimal class breaks:
#' #----
#'
#' # run HSMclass:
#' \dontrun{
#' hsm<- HSMclass(refdata.gr, predictions.gr, nclasses = 6,
#'               iterations = 1000, coolfactor=0.99, InitTemp = 80,
#'               weight.norefs = 2, weight.classwidth = 2)
#'
#' # calculate accuracy:
#' acc.opti<- classAccuracy(hsm)
#' summary(acc.opti)
#'}
#'
#' @importFrom Hmisc binconf
#' @export


classAccuracy<- function(..., conf.level=0.95){
 UseMethod("classAccuracy")
}



#' @rdname classAccuracy
#' @export
#' @method classAccuracy hsmclass
classAccuracy.hsmclass<- function(object, conf.level=0.95, ...){

  stopifnot(inherits(object, "hsmclass"))

  #----------------------------------------------------------
  call<- match.call()

  #----------------------------------------------------------
  # PERFORMING CLASSIFICATION

  val.data<- data.frame(refdata=object$settings$refdata, predictions=object$settings$predictions)

  ss_tot<- sum((val.data$refdata  -mean(val.data$refdata, na.rm=TRUE))^2, na.rm=TRUE)
  ss_res<- sum((val.data$refdata  -val.data$predictions)^2,na.rm=TRUE)
  r.sq.pred<- 1-(ss_res/ss_tot)


  # Classify Ground Truth Data and Predictions:
  classbreaks<- object$best.classbreaks
  classwidth<- abs(classbreaks[-length(classbreaks)] - classbreaks[-1])
  val.data$class.class<- cut(x=val.data$predictions, breaks=classbreaks, include.lowest=TRUE)
  val.data$truth.class<- cut(x=val.data$refdata, breaks=classbreaks, include.lowest=TRUE)




  #----------------------------------------------------------
  # ACCURACY ASSESSMENT FOR CLASSIFICATION


  # calculation of error matrix:
  norefperclass<-matrix(nrow=nlevels(val.data$truth.class),ncol=2)
  for (i in 1:nlevels(val.data$truth.class)){
    norefperclass[i,1]<-levels(val.data$truth.class)[i]
    norefperclass[i,2]<-sum(val.data$truth.class==norefperclass[i,1])
  }

  noclassifiedperclass<-matrix(nrow=nlevels(val.data$class.class),ncol=2)
  for (i in 1:nlevels(val.data$class.class)){
    noclassifiedperclass[i,1]<-levels(val.data$class.class)[i]
    noclassifiedperclass[i,2]<-sum(val.data$class.class==noclassifiedperclass[i,1])
  }

  nocorrectperclass<-matrix(nrow=nlevels(val.data$class.class),ncol=2)
  for (i in 1:nlevels(val.data$class.class)){
    nocorrectperclass[i,1]<-levels(val.data$class.class)[i]
    nocorrectperclass[i,2]<-sum((val.data$class.class==levels(val.data$class.class)[i])*(val.data$truth.class==levels(val.data$class.class)[i]))
  }

  # Overall-Accuracy [%]:
  n.references<- nrow(val.data)
  nocorrclasstotal<-sum(val.data$class.class==val.data$truth.class)
  overall_accuracy<-(nocorrclasstotal / n.references) *100


  # Producers Accuracy [%]:
  prod.accuracy<-matrix(nrow=nlevels(val.data$class.class),ncol=2)
  for (i in 1:nlevels(val.data$class.class)){
    prod.accuracy[i,1]<-levels(val.data$class.class)[i]
    prod.accuracy[i,2]<-(as.numeric(nocorrectperclass[i,2])/as.numeric(norefperclass[i,2]))*100
  }

  # Users Accuracy [%]:
  users.accuracy<-matrix(nrow=nlevels(val.data$class.class),ncol=2)
  for (i in 1:nlevels(val.data$class.class)){
    users.accuracy[i,1]<-levels(val.data$class.class)[i]
    users.accuracy[i,2]<-(as.numeric(nocorrectperclass[i,2])/as.numeric(noclassifiedperclass[i,2]))*100
  }

  # Map Accuracy [%]:
  prob.refs.perclass<- as.numeric(table(val.data$truth.class)) / nrow(val.data)
  weighted.prod.accuracy<- (as.numeric(prod.accuracy[,2])/100) * prob.refs.perclass
  map.accuracy<- sum(weighted.prod.accuracy)


  # Cohen's Kappa:
  nominator<-(n.references*nocorrclasstotal)-sum(as.numeric(norefperclass[,2])*as.numeric(noclassifiedperclass[,2]))
  denominator<-(n.references^2)-sum(as.numeric(norefperclass[,2])*as.numeric(noclassifiedperclass[,2]))
  kappa<-nominator/denominator


  # Quantity Disagreement Q [%]:
  prop.classifiedperclass<- as.numeric(table(val.data$class.class)) / nrow(val.data)
  abs.diffs<- 100 * abs(prob.refs.perclass - prop.classifiedperclass)
  Q<- 0.5* sum(abs.diffs)


  # Allocation Disagreement A [%]:
  comission.error<- prop.classifiedperclass - (as.numeric(nocorrectperclass[,2]) / n.references) # equal to 1-users accuracy
  omission.error<- prob.refs.perclass - (as.numeric(nocorrectperclass[,2]) / n.references)     # equal to 1-producers accuracy
  A<- 100*(apply(rbind(comission.error,omission.error),2,min))

  # Number of Reference Data per Class:
  no.referencedata.perclass<- table(val.data$truth.class)

  #----------------------------------------------------------
  # Confidence Intervalls:

  # CI for overall accuracy:
  conf.oaa<- confint_oaa(n.references, nocorrclasstotal, conf.level = conf.level)

  # CI for users and producers accuracy:

  # to be implemented

  #----------------------------------------------------------

  # RETURN RESULTS:
  out<-list(call=call,
            rsquared_val=r.sq.pred,
            predictions=val.data$predictions,
            classwidth=classwidth,
            def.classbreaks = object$best.classbreaks,
            overall.accuracy=overall_accuracy,
            conf.oaa=c(conf.oaa$Lower95CI, conf.oaa$Upper95CI, conf.level),
            map.accuracy=map.accuracy,
            prodaccuracy=prod.accuracy,
            usersaccuracy=users.accuracy,
            no.ref.classes=no.referencedata.perclass,
            cohenskappa=round(kappa,digits=2),
            Quantity.Disagreement=Q,
            Allocation.Disagreement=A)

  class(out)<- "classaccur"

  out

}



#' @rdname classAccuracy
#' @export
#' @method classAccuracy default
classAccuracy.default<- function(refdata, predictions, equal.int=NA,
                                 def.int=NA, conf.level=0.95, ...){

  #----------------------------------------------------------
  call<- match.call()

  #----------------------------------------------------------
  # PERFORMING CLASSIFICATION

      val.data<- data.frame(cbind(refdata, predictions))

      ss_tot<- sum((val.data$refdata  -mean(val.data$refdata, na.rm=TRUE))^2, na.rm=TRUE)
      ss_res<- sum((val.data$refdata  -val.data$predictions)^2,na.rm=TRUE)
      r.sq.pred<- 1-(ss_res/ss_tot)


    # Classify Ground Truth Data and Predictions:
      if (!is.na(equal.int)){
        maxvalue<-max(max(val.data$predictions),max(val.data$refdata))
        upperval<-equal.int*ceiling(maxvalue/equal.int)
        classbreaks<- seq(0,ceiling(upperval),equal.int)
        classwidth<- abs(classbreaks[-length(classbreaks)] - classbreaks[-1])
        val.data$class.class<- cut(x=val.data$predictions, breaks=seq(0,ceiling(upperval), equal.int), include.lowest=TRUE)
        val.data$truth.class<- cut(x=val.data$refdata, breaks=seq(0,ceiling(upperval), equal.int), include.lowest=TRUE)
      }

      if (!is.na(def.int[1])){

        model.range<- range(floor(c(refdata, predictions)/100)*100,
                            ceiling(c(refdata, predictions)/100)*100)

        ## restrict def.int to valid model range:
        exceed.vec<- sapply(def.int,  function(x) {x < model.range[1] | x > model.range[2]})
        if(any(exceed.vec)){
        def.int<- def.int[-which(exceed.vec)]
        warning("defined classes exceed valid model range. defined class intervals have been cut")
        }

        ## make def.int ready to use:
        def.int<- unique(sort(c(model.range, def.int))) # add lower and upper model range (round to 100)

        classbreaks<- def.int
        classwidth<- abs(classbreaks[-length(classbreaks)] - classbreaks[-1])
        val.data$class.class<- cut(x=val.data$predictions, breaks=classbreaks, include.lowest=TRUE)
        val.data$truth.class<- cut(x=val.data$refdata, breaks=classbreaks, include.lowest=TRUE)
      }



  #----------------------------------------------------------
  # ACCURACY ASSESSMENT FOR CLASSIFICATION


    # calculation of error matrix:
      norefperclass<-matrix(nrow=nlevels(val.data$truth.class),ncol=2)
      for (i in 1:nlevels(val.data$truth.class)){
        norefperclass[i,1]<-levels(val.data$truth.class)[i]
        norefperclass[i,2]<-sum(val.data$truth.class==norefperclass[i,1])
      }

      noclassifiedperclass<-matrix(nrow=nlevels(val.data$class.class),ncol=2)
      for (i in 1:nlevels(val.data$class.class)){
        noclassifiedperclass[i,1]<-levels(val.data$class.class)[i]
        noclassifiedperclass[i,2]<-sum(val.data$class.class==noclassifiedperclass[i,1])
      }

      nocorrectperclass<-matrix(nrow=nlevels(val.data$class.class),ncol=2)
      for (i in 1:nlevels(val.data$class.class)){
        nocorrectperclass[i,1]<-levels(val.data$class.class)[i]
        nocorrectperclass[i,2]<-sum((val.data$class.class==levels(val.data$class.class)[i])*(val.data$truth.class==levels(val.data$class.class)[i]))
      }

    # Overall-Accuracy [%]:
      n.references<- nrow(val.data)
      nocorrclasstotal<-sum(val.data$class.class==val.data$truth.class)
      overall_accuracy<-(nocorrclasstotal / n.references) *100


    # Producers Accuracy [%]:
      prod.accuracy<-matrix(nrow=nlevels(val.data$class.class),ncol=2)
      for (i in 1:nlevels(val.data$class.class)){
        prod.accuracy[i,1]<-levels(val.data$class.class)[i]
        prod.accuracy[i,2]<-(as.numeric(nocorrectperclass[i,2])/as.numeric(norefperclass[i,2]))*100
      }

    # Users Accuracy [%]:
      users.accuracy<-matrix(nrow=nlevels(val.data$class.class),ncol=2)
      for (i in 1:nlevels(val.data$class.class)){
        users.accuracy[i,1]<-levels(val.data$class.class)[i]
        users.accuracy[i,2]<-(as.numeric(nocorrectperclass[i,2])/as.numeric(noclassifiedperclass[i,2]))*100
      }

    # Map Accuracy [%]:
      prob.refs.perclass<- as.numeric(table(val.data$truth.class)) / nrow(val.data)
      weighted.prod.accuracy<- (as.numeric(prod.accuracy[,2])/100) * prob.refs.perclass
      map.accuracy<- sum(weighted.prod.accuracy)


    # Cohen's Kappa:
      nominator<-(n.references*nocorrclasstotal)-sum(as.numeric(norefperclass[,2])*as.numeric(noclassifiedperclass[,2]))
      denominator<-(n.references^2)-sum(as.numeric(norefperclass[,2])*as.numeric(noclassifiedperclass[,2]))
      kappa<-nominator/denominator


    # Quantity Disagreement Q [%]:
      prop.classifiedperclass<- as.numeric(table(val.data$class.class)) / nrow(val.data)
      abs.diffs<- 100 * abs(prob.refs.perclass - prop.classifiedperclass)
      Q<- 0.5* sum(abs.diffs)


    # Allocation Disagreement A [%]:
      comission.error<- prop.classifiedperclass - (as.numeric(nocorrectperclass[,2]) / n.references) # equal to 1-users accuracy
      omission.error<- prob.refs.perclass - (as.numeric(nocorrectperclass[,2]) / n.references)     # equal to 1-producers accuracy
      A<- 100*(apply(rbind(comission.error,omission.error),2,min))

   # Number of Reference Data per Class:
    no.referencedata.perclass<- table(val.data$truth.class)

  #----------------------------------------------------------
  # Confidence Intervalls:

    # CI for overall accuracy:
      conf.oaa<- confint_oaa(n.references, nocorrclasstotal, conf.level = conf.level)

    # CI for users and producers accuracy:

     # to be implemented

  #----------------------------------------------------------

  # RETURN RESULTS:
  out<-list(call=call,
            rsquared_val=r.sq.pred,
            predictions=val.data$predictions,
            classwidth=classwidth,
            def.classbreaks = def.int,
            equal.classbreaks = if(!is.na(equal.int)) classbreaks else equal.int,
            overall.accuracy=overall_accuracy,
            conf.oaa=c(conf.oaa$Lower95CI, conf.oaa$Upper95CI, conf.level),
            map.accuracy=map.accuracy,
            prodaccuracy=prod.accuracy,
            usersaccuracy=users.accuracy,
            no.ref.classes=no.referencedata.perclass,
            cohenskappa=round(kappa,digits=2),
            Quantity.Disagreement=Q,
            Allocation.Disagreement=A)

  class(out)<- "classaccur"

  out

}































