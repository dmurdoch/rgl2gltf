
# Find entries meeting a condition in a recursive object

findEntry <- function(x, test, ..., path = c()) {
  # This function converts a string or numeric index
  # to something that could be used in code, e.g. "a"
  # would become $a, "1" would become [["1"]], etc.
  toIndex <- function(s) {
    if (is.numeric(s)) {
      if (is.recursive(x))
        paste0("[[", s, "]]")
      else
        paste0("[", s, "]")
    } else if (nchar(s) &&
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

  if (is.null(xnames <- names(x))) {
    xnames <- rep("", length(x))
    pathnames <- seq_along(x)
  } else
    pathnames <- xnames

  tests <- vapply(seq_along(x), function(i) test(name = xnames[i], value = x[[i]]), TRUE, USE.NAMES = FALSE)
  hits <- which(tests)

  if (length(hits)) {
    if (is.recursive(x) || length(tests) > 1) {
      result <- c(result, lapply(hits,
                               function(hit) {
                                 res <- c(path, hit)
                                 names(res)[length(res)] <- toIndex(pathnames[hit])
                                 res
                                 }))
    } else # Not recursive, one positive, no negatives
      result <- c(result, list(path))
  }
  if (is.recursive(x))
    for (i in seq_along(x)) {
      if (i %in% hits && !is.recursive(x[[i]]))
        next
      newpath <- c(path, i)
      names(newpath)[length(newpath)] <- toIndex(pathnames[i])
      result <- c(result, findEntry(x[[i]], test, path = newpath))
    }
  result
}

# Test elements for a name matching a pattern

namePattern <- function(pattern) {
  force(pattern)
  function(name, value) grepl(pattern, name)
}

# Test elements for a specific class in a recursive object

hasClass <- function(class) {
  force(class)
  function(name, value) inherits(value, class)
}
