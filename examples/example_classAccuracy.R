
#----
# 1.) Example: classification accuracy for equidistant class width of 100:
#----

acc.equal<- classAccuracy(refdata, predictions, equal.int = 100)
summary(acc.equal)


#----
# 2.) Example: classification accuracy for arbitrary class breaks:
#----

acc.def<- classAccuracy(refdata, predictions,
                         def.int = c(0, 150, 200, 430, 610, 880))
summary(acc.def)


#----
# 3.) Example: classification accuracy for optimal class breaks:
#----

# run HSMclass:
hsm<- HSMclass(refdata, predictions, nclasses = 6,
                 iterations = 1000, coolfactor=99, InitTemp = 80,
                 weight.norefs = 2, weight.classwidth = 2)

# calculate accuracy:
acc.opti<- classAccuracy(refdata, predictions, def.int = hsm$best.classbreaks)
summary(acc.opti)


