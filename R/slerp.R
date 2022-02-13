slerpfun <- function(x, q) {
  if (!all(diff(x) > 0))
    stop("x must be strictly increasing")
  q <- q/sqrt(apply(q^2, 1, sum))
  if (nrow(q) != length(x))
    stop("q must hold one unit vector for each x value")
  index <- approxfun(x, seq_along(x), method = "constant", rule = 2)
  function(t) {
    drop <- function(cond) {
      if (any(cond)) {
        i0 <<- i0[!cond]
        i1 <<- i1[!cond]
        j <<- j[!cond]
        p0 <<- p0[!cond, , drop = FALSE]
        p1 <<- p1[!cond, , drop = FALSE]
        t <<- t[!cond]
      }
    }
    result <- matrix(NA, nrow=length(t), ncol = ncol(q))
    j <- 1:length(t)
    i0 <- index(t)
    # Get rid of out of range values
    j <- j[!is.na(i0)]
    if (!length(j)) return(result)
    i0 <- i0[j]
    t <- t[j]
    # Upper limit is fixed
    result[j[i0 == length(x)], ] <- q[length(x), ]
    j <- j[i0 < length(x)]
    if (!length(j)) return(result)
    t <- t[i0 < length(x)]
    i0 <- i0[i0 < length(x)]
    # The rest need to be interpolated
    i1 <- i0 + 1
    p0 <- q[i0, , drop = FALSE]
    p1 <- q[i1, , drop = FALSE]
    # Drop any NAs
    isna <- apply(cbind(p0, p1), 1, is.na)
    drop(isna)
    if (!length(j)) return(result)

    dot <- apply(p0*p1, 1, sum)
    neg <- dot < 0
    p1[neg, ] <- -p1[neg, ]
    dot[neg] <- -dot[neg]

    # If the vals are constant, don't bother interpolating
    const <- dot >= 1
    result[j[const], ] <- p0[const, ]
    drop(const)
    if (!length(j)) return(result)

    dot <- dot[!const]
    Omega <- acos(dot)
    t0 <- (t - x[i0])/(x[i1] - x[i0])
    result[j, ] <- (sin((1-t0)*Omega)*p0 + sin(t0*Omega)*p1)/sin(Omega)

    result
  }
}

lerpfun <- function(x, v) {
  fns <- apply(v, 2, function(col) approxfun(x, col, rule = 2))
  function(t)
    sapply(fns, function(f) f(t))
}

stepfun <- function(x, v) {
  fns <- apply(v, 2, function(col) approxfun(x, col, rule = 2))
  function(t)
    sapply(fns, function(f) f(t))
}
