# Find a name matching a pattern in a recursive object

findName <- function(pattern, x, path = c()) {
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
  if (length(hits <- grep(pattern, names(x))))
    result <- c(result, lapply(hits,
                               function(hit) {
                                 res <- c(path, hit)
                                 names(res)[length(res)] <- toIndex(names(x)[hit])
                                 res
                                 }))
  if (is.recursive(x))
    for (i in seq_along(x)) {
      newpath <- c(path, i)
      if (!is.null(names(x)))
        names(newpath)[length(newpath)] <- toIndex(names(x)[i])
      else
        names(newpath)[length(newpath)] <- paste0("[[", i, "]]")
      result <- c(result, findName(pattern, x[[i]], newpath))
    }
  result
}
