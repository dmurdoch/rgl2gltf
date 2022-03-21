# This function expects a number of matrices or dataframes
# each containing the same number of rows as input.
# It will look for duplicates in the column binding of
# all inputs, and reduce the inputs to unique rows,
# with an indices vector.

reindex <- function(...) {
  allcolumns <- as.data.frame(do.call(cbind, list(...)))
  o <- do.call(order, unname(allcolumns))
  ordered <- allcolumns[o,]
  dup <- duplicated(ordered)
  index <- integer(nrow(allcolumns))
  index[o] <- cumsum(!dup)
  c(list(indices = matrix(index, ncol = 1)),
    lapply(list(...), function(x) x[o,][!dup,]))
}
