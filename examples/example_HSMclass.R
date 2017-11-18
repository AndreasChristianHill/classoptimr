
# ------------------------------------------------#
## PERFORM OPTIMIZATION OF CLASSIFICATION SCHEME:

#----
# 1.) Example: optimization for 6 classes:
#----

hsm.1<- HSMclass(refdata, predictions, nclasses = 6,
                 iterations = 1000, coolfactor=99, InitTemp = 80,
                 weight.norefs = 2, weight.classwidth = 2)
summary(hsm.1)

#----
# 2.) Example: optimization for 6 classes, run heuristic 100 times
#----           and pick best solution over all runs:

hsm.2<- HSMclass(refdata, predictions, nclasses = 6,
                 iterations = 1000, coolfactor=99, InitTemp = 80,
                 weight.norefs = 2, weight.classwidth = 2,
                 bestever.iterationmode = 100)
summary(hsm.2)


# ------------------------------------------------#
##  PERFORM ENTIRE ANALYSIS:

# define a set of equidistant intervals to evaluate:
equal.intervals<- seq(100,300,20)

# define corresponding number of classes:
n.classes<- ceiling(max(refdata,predictions)/equal.intervals)

# Chain of analysis:
# --> 1. Identify optimal classification scheme for all given number of classes
# --> 2. Calculate classification accuracy for equidistant class intervals
# --> 3. Calculate classification accuracy for corresponding optimal no. of classes

lapply(seq_along(n.classes), function(x){

  hsm<- HSMclass(refdata, predictions, nclasses = n.classes[x],
                 iterations = 1000, coolfactor = 99, InitTemp = 80,
                 weight.norefs = 2, weight.classwidth = 2,
                 bestever.iterationmode = 5)

  acc.equal<- class_accuracy(refdata, pedictions, equal.int = equal.intervals[x])
  acc.opti<- class_accuracy(refdata, pedictions, def.int = hsm$best.classbreaks)

})
