.onLoad <- function(lib, pkg) {
  if (!("indices" %in% names(as.list(args(rgl.primitive)))))
    stop("rgl2gltf requires a build of rgl from the 'indices' branch")
}
