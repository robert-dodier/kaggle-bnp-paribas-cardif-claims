base.rate <- function (C) {
  sum (C) / length (C)
}

cross.entropy <- function (X, C) {
  if (is.factor(X) || length (unique (X)) <= 12) {
    cross.entropy.discrete (X, C)
  }
  else {
    cross.entropy.continuous (X, C)
  }
}

cross.entropy.discrete <- function (X, C) {
  count.missing <- sum(is.na(X))
  if (count.missing > 0) {
      C.for.missing.X <- C[is.na(X)]
      count.C.for.missing.X <- length(C.for.missing.X)
      count.C.for.missing.X.1 <- sum(C.for.missing.X)
      base.rate.C.for.missing.X <- count.C.for.missing.X.1 / count.C.for.missing.X

      X.present <- X[!is.na(X)]
      C.for.present.X <- C[!is.na(X)]

      scores <- vector (length=length(C))
      scores[is.na(X)] <- base.rate.C.for.missing.X
      scores[!is.na(X)] <- scores.discrete (X.present, C.for.present.X)
      cross.entropy.for.label.and.score (C, scores)
  }
  else {
    scores <- scores.discrete (X, C)
    cross.entropy.for.label.and.score (C, scores)
  }
}

scores.discrete <- function (X, C) {
  p.C.given.X.table <- p.C.given.X (X, C)
  scores <- vector (length=length(C))
  U <- sort (unique (X))
  for (i in 1:length(U)) {
    scores[X==U[i]] <- p.C.given.X.table[i]
  }

  scores
}

p.C.given.X <- function (X, C) {
  U <- sort (unique (X))
  p.C.given.X.table <- vector (length=length(U))
  for (i in 1:length(U)) {
    Xi <- X[X==U[i]]
    Ci <- C[X==U[i]]
    n <- length (Ci)
    n1 <- sum (Ci)
    p.C.given.X.table[i] <- n1/n
  }

  p.C.given.X.table
}

cross.entropy.for.label.and.score <- function (C, scores) {
  n <- length(C)
  - (sum(p.log.q(C, scores)) + sum(p.log.q(1 - C, 1 - scores))) / n
}

p.log.q <- function (p, q) {
  foo <- p*log(q)
  foo[p == 0 & q == 0] <- 0
  foo
}

cross.entropy.continuous <- function (X, C) {
  count.missing <- sum(is.na(X))
  if (count.missing > 0) {
      C.for.missing.X <- C[is.na(X)]
      count.C.for.missing.X <- length(C.for.missing.X)
      count.C.for.missing.X.1 <- sum(C.for.missing.X)
      base.rate.C.for.missing.X <- count.C.for.missing.X.1 / count.C.for.missing.X

      X.present <- X[!is.na(X)]
      C.for.present.X <- C[!is.na(X)]

      scores <- vector (length=length(C))
      scores[is.na(X)] <- base.rate.C.for.missing.X
      scores[!is.na(X)] <- scores.continuous (X.present, C.for.present.X)
      cross.entropy.for.label.and.score (C, scores)
  }
  else {
    scores <- scores.continuous (X, C)
    cross.entropy.for.label.and.score (C, scores)
  }
}

scores.continuous <- function (X, C) {
  n <- length (X)
  X1 <- X[C == 1]
  n1 <- length (X1)
  X1.mean <- mean (X1)
  X1.sd <- sd (X1)
  X0 <- X[C == 0]
  n0 <- length (X0)
  X0.mean <- mean (X0)
  X0.sd <- sd (X0)
  X.sd.pooled <- sqrt (n0/n*X0.sd^2 + n1/n*X1.sd^2)

  pC1 <- base.rate (C)
  pC0 <- 1 - pC1

  Z1 <- (X - X1.mean)/X.sd.pooled
  Z0 <- (X - X0.mean)/X.sd.pooled

  1/(1 + (pC0/pC1)*exp(-(1/2)*(Z0^2 - Z1^2)))
}
