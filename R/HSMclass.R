#' HSMclass
#'
#' @description
#' Identification of optimal classification schemes by Heuritic Search Method (HSM) by Simulated Annealing
#'
#' @param refdata \code{\link[base]{vector}} containing the values of the continuous
#'                  response variable used in the prediction model
#' @param predictions predictions for the response values of the prediction model.
#'                    \strong{Note:}(\code{refdata} and \code{predictions} have to correspond to each other)
#' @param nclasses number of classes for which an optimal classification scheme should be computed
#' @param moveinterval controls if classes should be whole numbers (default is 10).
#' @param iterations number of iterations used in heuristic
#' @param coolfactor cooling factor of Simulated Annealing algorithm
#' @param InitTemp intial Temperature of Simulated Annealing algorithm
#' @param weight.norefs weight for maximizing number of reference data pefor each class
#' @param weight.classwidth weight for minimizing the classwidth for each class
#' @param bestever.iteration  number of times the heuristic is repeatedly applied.
#'                            If > 1, the best solution over all runs will be chosen
#'                            as the optimal solution. Defaults to 10.
#' @param progressbar Shows the progress of the heuristic. Defaults to \code{TRUE}.
#' @param trace logical. If \code{TRUE}, prints current solution- and penalty-term values to the console.
#'
#' @return \code{HSMclass} returns an object of class \code{"hsmclass"}.
#'
#'
#' An object of class \code{"hsmclass"} returns a \code{list} of the following components:
#'
#'  \item{best.classbreaks}{code{vector} containing the class break values of the optimal classification scheme}
#'  \item{best.classwidth}{{code{vector} containing the class width of each class}}
#'  \item{no.refs.best}{{code{vector} containing the number of reference data in each class}}
#'  \item{BestSolution}{value of best solution found by heuristic}
#'  \item{Solutions}{code{vector} containing all solution values of heuristic}
#'  \item{bestever.iterationmode}{code{vector} containing the class break values of the optimal classification scheme}
#'  \item{Temperature}{code{vector} containing the temperature values of the Simulated Annealing algorithm}
#'  \item{deltaF}{ code{vector} containing the differences between new solution and best solution at the
#'                 respective iteration of the heuristic}
#'  \item{p}{code{vector} containing the p-values (...). Improvement of Sol.Best always yields p = 1}
#'  \item{moved.per.iteration}{code{vector} containing the number of classbreaks moved for respective iteration}
#'  \item{comp.time}{information about the computation time}
#'  \item{call}{ the function call passed to function \code{HSMclass}}
#'  \item{settings}{a \code{list} containing the function's inputs:
#'                    \itemize{
#'                    \item \code{refdata:}
#'                    \item \code{predictions:}
#'                    \item \code{nclasses:}
#'                    \item \code{iterations:}
#'                    \item \code{moveinterval:}
#'                    \item \code{coolfactor:}
#'                    \item \code{InitTemp:}
#'                    \item \code{weight.norefs:}
#'                    \item \code{weight.classwidth:}
#'                    }}
#'
#' @references Hill, A., Breschan, J., & Mandallaz, D. (2014). Accuracy assessment of timber
#'             volume maps using forest inventory data and LiDAR canopy height models.
#'             \emph{Forests}, \strong{5(9)}, 2253-2275.
#'
#' @examples
#' # ------------------------------------------------#
#' ## PERFORM OPTIMIZATION OF CLASSIFICATION SCHEME:
#'
#' #----
#' # 1.) Example: optimization for 6 classes:
#' #----
#'
#' hsm.1<- HSMclass(refdata = refdata.gr, predictions = predictions.gr,
#'                  nclasses = 6,iterations = 1000, coolfactor=0.99,
#'                  InitTemp = 80, weight.norefs = 2, weight.classwidth = 2)
#' summary(hsm.1)
#'
#'
#' #----
#' # 2.) Example: optimization for 6 classes, run heuristic 100 times
#' #----           and pick best solution over all runs:
#'  \dontrun{
#'  hsm.2<- HSMclass(refdata = refdata.gr, predictions = predictions.gr,
#'                   nclasses = 6,
#'                   iterations = 1000, coolfactor=0.99, InitTemp = 80,
#'                   weight.norefs = 2, weight.classwidth = 2,
#'                   bestever.iteration = 100)
#' summary(hsm.2)
#' }
#'
#'
#' # ------------------------------------------------#
#' ##  PERFORM ENTIRE ANALYSIS:
#'
#' # define a set of equidistant intervals to evaluate:
#' equal.intervals<- seq(100,300,20)
#'
#' # define corresponding number of classes:
#' n.classes<- ceiling(max(refdata.gr, predictions.gr)/equal.intervals)
#'
#' # Chain of analysis:
#' # --> 1. Identify optimal classification scheme for all given number of classes
#' # --> 2. Calculate classification accuracy for equidistant class intervals
#' # --> 3. Calculate classification accuracy for corresponding optimal no. of classes
#'
#'\dontrun{
#' acc.equal<- list()
#' acc.opti<-  list()
#'
#' lapply(seq_along(n.classes), function(x){
#'
#'   hsm<- HSMclass(refdata.gr, predictions.gr, nclasses = n.classes[x],
#'                  iterations = 1000, coolfactor = 0.99, InitTemp = 80,
#'                  weight.norefs = 2, weight.classwidth = 2)
#'
#'  acc.equal[[x]]<- classAccuracy(refdata.gr, predictions.gr, equal.int = equal.intervals[x])
#'  acc.opti[[x]]<- classAccuracy(hsm)
#'
#'})
#'}
#'
#' @export
#' @import zoo
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom stats qnorm runif

HSMclass<- function(refdata, predictions, nclasses, moveinterval=10,
                    iterations, coolfactor, InitTemp,
                    weight.norefs, weight.classwidth,
                    bestever.iteration=10,
                    progressbar=TRUE, trace=FALSE){

  #----------------------------------------------------------
  call<- match.call()

  #----------------------------------------------------------
  # starting time of optimization computation:
    ptm<- proc.time()

  #----------------------------------------------------------
  # "BESTEVER MODE"

    # save original InitTemp:
    # InitTemp.orig<- InitTemp
    InitTemp.vec<- c()

    # check if "bestever.mode" desired by user (if no, then be.iterations is 1)
    be.iterations<- bestever.iteration

    # initialize progressbar:
    if (progressbar) {
      pb <- txtProgressBar(min = 0, max = be.iterations * iterations, style = 3)
      it.total<- 0
    }

    # initialize "bestever" - solution:
    Sol.Bestever<- -Inf

    # loop over bestever-iterations:
      for (b in 1: be.iterations){

          #----------------------------------------------------------
          # INITIALIZATION


            # NEW(28.02.2017): adapt cooling scheme (InitTemp) according to former realized p.acc.solworse
              if (b > 1){

                # closure for calculating moving avarage:
               # ma <- function(x,windsize){na.omit(as.numeric(filter(x,rep(1/windsize,windsize), sides=2)))}

                acc.dat<- data.frame(p.acc=p.acc.vec, p=p.vec, sol.worse = dF.vec > 0, sol.worse.acc = 1*(p.acc.vec<= p.vec))
                acc.dat[acc.dat$sol.worse == FALSE, "sol.worse.acc"]<- NA


                # dataset of pacc and p for cases of new solution = worse than best solution:
                #acc.dat.solworse<- acc.dat[acc.dat$sol.worse == TRUE, ]

                # compute moving average of p.acc.worse (prob. of accepting a worse solution):
                #p.acc.worse<- ma(x = acc.dat.solworse$sol.worse.acc, windsize = round(nrow(acc.dat.solworse)*0.05))
                p.acc.worse<- rollapply(acc.dat$sol.worse.acc, round(nrow(acc.dat)*0.2), mean, align = "center", na.rm = TRUE)


                  # if p.acc.worse for the first n moves < 0.4, we raise the initTemp for the
                  # next best-ever-iteration run by 0.5; if p.acc.worse for the first n moves > 0.5, lower initTemp by 0.5
                  if(p.acc.worse[1] < 0.4){
                    InitTemp<- InitTemp*1.5
                  }

                  if(p.acc.worse[1] > 0.5){
                    InitTemp<- InitTemp*0.5
                  }

              }


            # create initialize classbreaks:
              maxvalue<- max(refdata, predictions)
              class.step.init<- ceiling( (maxvalue/nclasses) / moveinterval ) * moveinterval
              upperval<- class.step.init * ceiling(maxvalue/class.step.init)
              classbreaks.init<- seq(0, upperval, class.step.init)
              nclassbreaks<- length(classbreaks.init)

            # compute average classwidth for equally sized classes and threshold classwidth:
              ave.equal.classwidth<- ceiling((maxvalue/nclasses)/moveinterval)*moveinterval

            # compute initial "Best" solution:
              class.class.init<- cut(x=predictions,breaks=classbreaks.init, include.lowest=TRUE)
              truth.class.init<- cut(x=refdata,breaks=classbreaks.init, include.lowest=TRUE)
              penalty1.init<- sum((table(truth.class.init) - ceiling(length(refdata)/nclasses))^2) * weight.norefs # that's third term in article
              penalty2.init<- sum(ave.equal.classwidth^2) * weight.classwidth # that's second term in article

              Sol.Best.init<- sum(class.class.init==truth.class.init) - (penalty1.init + penalty2.init) # that's calculating the main equation (formula 6)
              Sol.Best<- Sol.Best.init
              classbreaks<- classbreaks.init

            # initialize documentation vectors:
              no.moved<- c()
              temp.vec<- c()
              dF.vec<- c()
              p.vec<- c()
              p.acc.vec<- c()
              best.sol.vec<- c()

            # initialize iteration.no:
              iteration.no<- 0


          #----------------------------------------------------------
          # ITERATIONS

           # apply simulated annealing (for-loop):
            for (i in 1:iterations){

              # increase iteration.no:
                iteration.no<- iteration.no + 1

              # determine number of classbounds moved for this iteration:
                move.number<- sample(seq(1,(nclassbreaks-2),1),1) # upper and lower class boundaries stay fixed (?)
                no.moved<- append(no.moved, move.number)

              # choose #move.number Classbreaks randomly:
                ind.classbreaks2move<- sample(seq(2,(nclassbreaks-1),1),size=move.number,replace=F)

              # determine rank of selected classbreaks to move: (new: ich glaub das ist C<berflC<ssig..., da darC<ber reihenfolge schon zufC$llig)
                rank2move<- sample(seq(1,move.number,1),size=move.number,replace=F)
                ind.classbreaks2move<- ind.classbreaks2move[c(rank2move)]


            #---------------------------------------

            # create new classbreaks:
              for (j in 1:move.number){

                # initialize classbreaks.new:
                  classbreaks.new<- classbreaks

                # determine range of move:
                  ind.move<- ind.classbreaks2move[j]
                  range2right<- (abs(classbreaks[ind.move] - classbreaks[ind.move+1]))
                  range2left<- (abs(classbreaks[ind.move] - classbreaks[ind.move-1]))

                # possible move is 80 % of range:
                  steps2right<- seq(0, round((0.8*range2right)/moveinterval) * moveinterval, moveinterval)
                  steps2left<- seq(0, round((-0.8*range2left)/moveinterval) * moveinterval, -moveinterval)
                  moveit<- sample(c(steps2right,steps2left),size=1)

                # create new classbreaks-vector:
                  classbreaks.new[ind.move]<- classbreaks[ind.move] + moveit

              } # end of loop over classbreaks

            #---------------------------------------

            # calculate New Solution:

              # checking, if classbreaks are not identical:
                if (sum(duplicated(classbreaks.new))==0){

                # devide data in new classes:
                  class.class<- cut(x=predictions, breaks=classbreaks.new, include.lowest=TRUE)
                  truth.class<- cut(x=refdata, breaks=classbreaks.new, include.lowest=TRUE)
                  classwidth<- abs(classbreaks.new[-nclassbreaks] - classbreaks.new[-1])

                # compute penalty term:
                  # P1: aim: equally distribute the reference data over all classes:
                  penalty1<- sum((table(truth.class) - ceiling(length(refdata)/nclasses))^2) * (0.01 * weight.norefs)
                  # P1: aim: equally distribute the reference data over all classes:
                  penalty2<- sum(classwidth^2) * (0.0001 * weight.classwidth)

                # Compute new Solution:
                  Sol.New<- (sum(class.class==truth.class)) - (penalty1 + penalty2)

                } else {
                  Sol.New<- Sol.New
                  classbreaks.new<- classbreaks
                }

                if(trace){
                  cat("\n")
                  print(paste("P_noref:", penalty1))
                  print(paste("P_classwidth:",penalty2))
                  print(paste("corrclass:", sum(class.class==truth.class)))
                  print(paste("newSolution:",Sol.New))
                  cat("\n")
                }

            #----------------------------------------------------------
            # ACCEPTANCE CRITERION (Simulated Annealing):

              Temp<- InitTemp * (coolfactor^iteration.no)
              dF<- Sol.Best - Sol.New

              p<- min(1,exp(-dF/Temp)) # Improvement of Sol.Best always yields p = 1 !!!!
              p.acc<- runif(1)

              if (p.acc <= p){ # this ensures, that also a worse solution can be accepted (prevents from getting trapped in local optimum)!
                Sol.Best<- Sol.New
                classbreaks<- classbreaks.new
                nrefsclass<- table(truth.class)
                if (Sol.New >= Sol.Bestever){
                  Sol.Bestever<- Sol.New
                  classbreaks.bestever<- classbreaks.new
                  # cat("\n")
                  # print(paste("classbreaks.bestever:", classbreaks.bestever))
                  # cat("\n")
                  nrefsclass.bestever<-table(truth.class)
                }
              }

              temp.vec<- append(temp.vec,Temp)
              dF.vec<- append(dF.vec,dF)
              p.vec<- append(p.vec, p)
              p.acc.vec<- append(p.acc.vec,p.acc)
              best.sol.vec<- append(best.sol.vec, Sol.Best)


            #---------------------------------------

             if(progressbar){
               it.total<- it.total + 1
               setTxtProgressBar(pb, it.total)
             }

          } # end of loop over iterations

              InitTemp.vec<- append(InitTemp.vec, InitTemp)

    } # end of bestever-loop

    # close progress:
    if(progressbar){close(pb)}


  #----------------------------------------------------------
  # RETURN RESULT:

    best.classwidth<- abs(classbreaks[-nclassbreaks] - classbreaks[-1])
    bestever.classwidth<- abs(classbreaks.bestever[-nclassbreaks] - classbreaks.bestever[-1])

  # overwrite results with "bestever"-results (if bestever.iterations = 1, they are identical):
    classbreaks<- classbreaks.bestever
    best.classwidth<- bestever.classwidth
    nrefsclass<- nrefsclass.bestever
    Sol.Best<- Sol.Bestever

    # end of computing time for optimization:
    opti.time<- proc.time()-ptm

    opti.result<- list(best.classbreaks=classbreaks,
                       best.classwidth=best.classwidth,
                       no.refs.best=nrefsclass,
                       BestSolution=Sol.Best,
                       Solutions=best.sol.vec,
                       bestever.iteration=bestever.iteration,
                       Temperature=temp.vec,
                       deltaF=dF.vec,
                       p=p.vec,
                       p.acc=p.acc.vec,
                       InitTemp=InitTemp.vec,
                       moved.per.iteration=no.moved,
                       comp.time=opti.time,
                       call=call,
                       settings=list(refdata=refdata, predictions=predictions,
                                     nclasses=nclasses,
                                     iterations=iterations, moveinterval=moveinterval,
                                     coolfactor=coolfactor,InitTemp=InitTemp,
                                     weight.norefs=weight.norefs,
                                     weight.classwidth=weight.classwidth,
                                     bestever.iteration=bestever.iteration))

    class(opti.result)<- "hsmclass"

    # return result:
    opti.result

} # END OF FUNCTION


