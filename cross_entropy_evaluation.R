source ("cross_entropy.R")

# assume that training data is loaded via .RData and is named 'training'

target <- training[, "target"]

for (i in 1:ncol(training)) {
  if (is.factor (training[,i]) && length (levels (training[, i])) > 200) {
    cat (sprintf ("training[,%d] is a factor with %d levels; just skip it.\n", i, length (levels (training[, i]))))
  }
  else {
    cat (sprintf ("%s %f\n", colnames(training)[i], cross.entropy (training[, i], target)))
  }
}
