# wilcoxon.R -- Wilcoxon-Mann-Whitney rank sum statistic
# adapted from Maxima code:
# wilcoxon (X, C) := block ([X1, X0, X10, R, n1, n0, Rsum, W],
# 
#     X1 : X [C = 1, 1],
#     X0 : X [C = 0, 1],
#     n1 : nrows (X1),
#     n0 : nrows (X0),
#     if n1 = 0 or n0 = 0
#         then 1/2
#         else
#            (X10 : append (elements (X1), elements (X0)),
#             R : ranks (X10),
#             Rsum : sum (R[i], i, 1, n1),
#             W : (Rsum - n1 * (n1 + 1) / 2) / (n1 * n0)));

wilcoxon <- function (X, C) {
  X1 <- X[C == 1]
  X0 <- X[C == 0]
  n1 <- length(X1)
  n0 <- length(X0)
  if (n1 == 0 || n0 == 0) { 1/2 }
  else {
    X10 <- c(X1, X0)
    R <- rank(X10, na.last=T)
    Rsum <- sum(as.double(R[1:n1]))
    W <- (Rsum - n1 * as.double(n1 + 1) / 2) / (n1 * as.double(n0))
    W
  }
}
