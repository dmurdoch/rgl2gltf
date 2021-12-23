as.mesh3d.gltf <- function(x, ...) {
  s <- as.rglscene(x, ...)
  outmeshes <- list()
  for (i in seq_along(s$objects)) {
    m <- as.mesh3d(s$objects[[i]])
    if (!is.null(m))
      outmeshes <- c(outmeshes, list(m))
  }
  if (length(outmeshes) > 1)
    shapelist3d(outmeshes, plot = FALSE)
  else if (length(outmeshes) > 0)
    outmeshes[[1]]
  else
    NULL
}
