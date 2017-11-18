
## -- Summarize classification accuracy:

acc.equal<- classAccuracy(refdata, predictions, equal.int = 100)
summ.<- summary(acc.equal)

# print summary-object:
summ.

# extract accuracy-data.frame:
summ.$accmat
