
readglTF <- function(path, defaultbin = NULL, ...) {
  gltf <- read_json(path, ...)
  if (is.null(gltf[["scene"]]) && length(gltf$scenes))
    gltf$scene <- 0
  if (is.null(gltf$asset) || as.numeric_version(gltf$asset$version) != "2.0")
    stop("This function can only read GLtf version 2.0 files")
  Gltf$new(gltf, defaultbin = defaultbin)
}
