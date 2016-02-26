# assume training data recovered from .RData

target <- training[, "target"]
cat (sprintf ("base rate=%f\n", base.rate (target)))

for (i in 1:ncol(training)) {
  X <- training[, i]
  if (sum(is.na(X)) > 0) {
    target.for.missing.X <- target[is.na(X)]
    cat (sprintf ("%s: #missing=%d, rate for missing data=%f\n", colnames(training)[i], sum(is.na(X)), base.rate (target.for.missing.X)))
  }
  else {
    cat (sprintf ("%s: no missing data\n", colnames(training)[i]))
  }
}
