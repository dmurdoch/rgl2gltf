as.mesh3d.gltf <- function(x, meshes = seq_along(x$meshes), ...) {
  readBufferview <- function(buf) {
    bufferview <- x$bufferViews[[buf+1]]
    stop("not finished readBufferview")
  }
  readAccessor <- function(acc) {
    accessor <- x$accessors[[acc+1]]
    view <- readBufferview(accessor$bufferView)
    stop("not finished readAccessor")
  }

  for (m in meshes) {
    inmesh <- x$meshes[[m]]
    for (p in seq_along(inmesh$primitives)) {
      prim <- inmesh$primitives[[p]]
      normals <- NULL
      position <- NULL
      for (a in seq_along(prim$attributes)) {
        attr <- prim$attributes[a]
        values <- readAccessor(attr[[1]])
        switch (names(attr),
          NORMAL = normals <- values,
          POSITION = positions <- values
        )
      }
    }
  }
}
