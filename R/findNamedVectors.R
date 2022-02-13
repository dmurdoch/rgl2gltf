findNamedVectors <- function(x, path = c()) {
  toIndex <- function(s) {
    if (nchar(s) &&
        substr(s, 1, 1) %in% c(letters, LETTERS, ".", "_") &&
        grepl("^[[:alnum:]_.]*$", s)) {
      if (is.recursive(x))
        paste0("$", s)
      else
        paste0("['", s, "']" )
    } else
      paste0("[['", s, "']]")
  }
  result <- list()
  if (is.recursive(x) && length(x)) {
    logi <- sapply(x, function(elem) !is.recursive(elem) && !is.null(names(elem)))
    hits <- which(logi)
    if (length(hits)) {
      if (!is.null(names(x)))
        hits <- names(x)[hits]
      result <- c(result, sapply(hits,
                               function(hit) {
                                 res <- c(path, hit)
                                 names(res)[length(res)] <- toIndex(names(x)[hit])
                                 res
                               }))
    }
    for (i in seq_along(x)) {
      newpath <- c(path, i)
      if (!is.null(names(x)))
        names(newpath)[length(newpath)] <- toIndex(names(x)[i])
      else
        names(newpath)[length(newpath)] <- paste0("[[", i, "]]")
      result <- c(result, findNamedVectors(x[[i]], newpath))
    }
  }
  result
}
