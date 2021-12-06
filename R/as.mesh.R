as.mesh3d.gltf <- function(x, scene = x$scene, nodes = NULL, ...) {

  processNode <- function(n, parent) {
    node <- x$getNode(n)
    transform <- x$getTransform(n, parent)
    if (!is.null(node$mesh) && n %in% convertNodes) {
      inmesh <- x$getMesh(node$mesh)
      for (p in seq_along(inmesh$primitives)) {
        prim <- inmesh$primitives[[p]]
        class(prim) <- "gltfPrimitive"
        if (!is.null(prim$targets)) {
          print(prim)
          # What are prim targets????
          browser()
        }
        mat <- x$getRglMaterial(prim$material)
        normals <- NULL
        positions <- NULL
        texcoords <- NULL
        for (a in seq_along(prim$attributes)) {
          attr <- unlist(prim$attributes[a])
          values <- x$readAccessor(attr[1])
          switch (names(attr),
                  NORMAL = normals <- values,
                  POSITION = positions <- values,
                  COLOR_0 = {
                    mat$color <- rgb(values[,1], values[,2], values[,3])
                    if (ncol(values) == 4)
                      mat$alpha <- values[,4]
                  }
          )
          if (!is.null(mat$texture)) {
            if (is.null(coord <- mat$gltftexCoord))
              coord <- 0
            mat$gltftexCoord <- NULL
            if (names(attr) == paste0("TEXCOORD_", coord))
              texcoords <- cbind(values[,1], -values[,2])
          }
        }
        if (is.null(prim$indices))
          indices <- seq_len(nrow(positions))
        else
          indices <- x$readAccessor(prim$indices) + 1

        if (is.null(mode <- prim$mode))
          mode <- 4
        ninds <- length(indices)
        newmesh <- switch(as.character(mode),
          "0" = mesh3d(x = positions,    # points
                       normals = normals,
                       texcoords = texcoords,
                       points = indices,
                       material = mat),
          "1" = mesh3d(x = positions,    # segments
                       normals = normals,
                       texcoords = texcoords,
                       segments = matrix(indices, nrow = 2),
                       material = mat),
          "2" = mesh3d(x = positions,    # loop
                       normals = normals,
                       texcoords = texcoords,
                       segments = rbind(indices,
                                        c(indices[-1], indices[1])),
                       material = mat),
          "3" = mesh3d(x = positions,    # strip
                       normals = normals,
                       texcoords = texcoords,
                       segments = rbind(indices[-length(indices)],
                                        indices[-1]),
                       material = mat),
          "4" = mesh3d(x = positions,    # triangles
                       normals = normals,
                       texcoords = texcoords,
                       triangles = matrix(indices, nrow = 3),
                       material = mat),
          "5" = mesh3d(x = positions,    # triangle strip
                       normals = normals,
                       texcoords = texcoords,
                       triangles = rbind(indices[-c(ninds, ninds-1)],
                                         indices[-c(1, ninds)],
                                         indices[-c(1,2)]),
                       material = mat),
          "6" = mesh3d(x = positions,    # triangle fan
                       normals = normals,
                       texcoords = texcoords,
                       triangles = rbind(indices[1],
                                         indices[-c(1, ninds)],
                                         indices[-c(1,2)]),
                       material = mat))
        newmesh <- rotate3d(newmesh, matrix = t(transform))
        outmeshes[[nextmesh]] <<- newmesh
        nextmesh <<- nextmesh + 1
      }
    }

    children <- unlist(node$children)

    if (n %in% convertNodes)
      convertNodes <<- union(convertNodes, children)

    for (child in children)
      processNode(child, n)
  }

  if (is.null(scene))
    scene <- 0

  if (is.null(convertNodes <- nodes))
    convertNodes <- seq_len(x$listCount("nodes")) - 1

  if (scene + 1 > x$listCount("scenes"))
    stop("scene ", scene, " not found.")

  defaultmaterial <- list()
  if (!is.null(extras <- x$getExtras()) &&
      !is.null(extras$RGL_material))
    defaultmaterial <- extras$RGL_material

  outmeshes <- list()
  nextmesh <- 1
  nodes <- x$getScene(scene)$nodes
  for (n in nodes) {
    processNode(n, NULL)
  }
  shapelist3d(outmeshes, plot = FALSE)
}
