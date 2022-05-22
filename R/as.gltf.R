as.node <- function(x, ...) {
  class(x) <- "gltfNode"
  x
}

as.gltf <- function(x, ...) {
  UseMethod("as.gltf")
}

as.gltf.default <- function(x, y = NULL, z = NULL, vertices,
                            material = NULL,
                            normals = NULL,
                            texcoords = NULL,
                            points = NULL, segments = NULL,
                            triangles = NULL,
                            quads = NULL,
                            transform = diag(4),
                            extras = NULL,
                            ...,
                            rglscene = list(),
                            previous = Gltf$new(),
                            newScene = FALSE,
                            parentNode = NULL,
                            dir = tempdir(),
                            scale = c(1,1,1)) {

  modePoints <- 0
  modeSegments <- 1
  modeTriangles <- 4

  addPrimitive <- function(indices, mode) {
    result$makePrimitive(indices, mode, attributes, matnum)
  }

  writeColors <- function(mat) {
    # We know we're multicolored already
    n <- max(length(mat$color), length(mat$alpha))
    if (length(mat$color))
      col <- col2rgb(rep_len(mat$color, n))
    else
      col <- matrix(255L, nrow = 3, ncol = n)
    if (length(mat$alpha) && any(mat$alpha != 1))
      col <- rbind(col, rep_len(round(mat$alpha*255), n))
    result$writeVectors(col, types = "ubyte", normalized = TRUE)
  }

  makeSphere <- function(sections = 18, segments = 24) {
    phi <- rep((1:(sections-1))/sections - 0.5, segments)
    theta <- rep(2*(0:(segments-1))/segments, each = sections - 1)

    x <- c(sinpi(theta)*cospi(phi), 0, 0)
    y <- c(sinpi(phi), -1, 1)
    z <- c(cospi(theta)*cospi(phi), 0, 0)
    s <- c(theta/2, 0, 0)
    t <- c(phi + 0.5, 0, 1)

    pole <- length(theta) # zero based index

    mod1 <- segments*(sections - 1)
    ind <- rep(0:(sections - 3), segments) + (sections - 1)*rep(0:(segments-1), each = sections - 2)
    it <- rbind(ind %% mod1,
                (ind + sections - 1) %% mod1,
                (ind + sections) %% mod1,
                ind %% mod1,
                (ind + sections) %% mod1,
                (ind + 1) %% mod1)
    it <- cbind(it, rbind(rep(pole, segments),
                          ((1:segments)*(sections - 1)) %% mod1,
                          ((1:segments)*(sections - 1) - sections + 1) %% mod1,
                          rep(pole + 1, segments),
                          ((1:segments)*(sections - 1) - 1) %% mod1,
                          ((1:segments)*(sections - 1) + sections - 2) %% mod1))
    sphereAttributes <- list(vertices = result$writeVectors(rbind(x, y, z)),
                             texcoords = result$writeVectors(rbind(s, t)),
                             indices = result$addAccessor(c(it), targetElementArray))
    result$setExtras(c(result$getExtras(), list(RGL_sphere = sphereAttributes)))
  }

  addSpheres <- function(x) {
    if (!length(extras <- result$getExtras()) || is.null(extras$RGL_sphere)) {
      makeSphere()
      extras <- result$getExtras()
    }
    sphere <- extras$RGL_sphere

    vertices <- x$vertices
    n <- nrow(vertices)
    material <- x$material
    radii <- rep(x$radii, length = n)
    i <- seq_len(nrow(x$colors))
    colors <- x$colors[rep(i, length.out = n),,drop = FALSE]
    children <- c()
    primitive <- list(list(attributes = c(POSITION = sphere$vertices,
                                          NORMALS = sphere$vertices,
                                          TEXCOORDS = sphere$texcoords),
                           mode = modeTriangles,
                           indices = sphere$indices))
    for (i in seq_len(n)) {
      material$color <- rgb(colors[i, 1], colors[i, 2],
                            colors[i, 3], colors[i, 4])
      matnum <- result$addMaterial(material)
      primitive[[1]]$material <- matnum
      mesh <- result$addMesh(primitive)
      child <- result$addNode(mesh)
      children <- c(children, child)
      node <- result$getNode(child)
      node$scale <- rep(radii[i], 3)/scale
      node$translation <- vertices[i,]
      result$setNode(child, node)
    }
    main <- result$addNode(extras = asRGLobj(x))
    node <- result$getNode(main)
    node$children <- I(children)
    result$setNode(main, node)
    if (newScene)
      scene <- result$addScene()
    else
      scene <- result$defaultScene()
    main
  }

  # This handles 3D sprites only
  # There's one node for the object (thisNodeNum).  It has one child
  # for each instance of the sprite that includes the translation
  # and scaling.  Each of those has a node for each of the
  # component objects within the sprite.

  addSprites <- function(x) {
    vertices <- x$vertices
    n <- nrow(vertices)
    radii <- rep(x$radii, length = n)
    material <- x$material
    i <- seq_len(nrow(x$colors))
    colors <- x$colors[rep(i, length.out = n),]
    transform <- x$usermatrix
    if (is.null(transform)) transform <- diag(4)

    # add the main node
    thisNodeNum <- result$addNode()
    thisNode <- result$getNode(thisNodeNum)

    # Add nodes for each of the objects in the sprite
    childnodes <- integer(length(x$objects))
    prevlastnode <- result$listCount("nodes") - 1
    for (i in seq_along(childnodes)) {
      # This assumes that if a child adds multiple nodes,
      # the main one comes first.
      childnodes[i] <- result$listCount("nodes")
      result <- as.gltf(x$objects[[i]], previous = result,
                          newScene = FALSE,
                          rglscene = rglscene,
                          parentNode = NULL,
                          transform = transform,
                          ...)
    }

    # Now remove all those nodes:  they belong lower
    # in the hierarchy

    scene <- result$getScene(result$scene)
    scene$nodes <- with(scene, nodes[nodes <= prevlastnode])
    result$setScene(result$scene, scene)

    # Add nodes for each rep of the sprite.  Really we'd
    # like to have each of these refer to the child nodes
    # created above, but that's not allowed, so we make
    # copies.

    children <- c()
    for (i in seq_len(n)) {
      child <- result$addNode()
      children <- c(children, child)
      node <- result$getNode(child)
      node$scale <- rep(radii[i], 3)/scale
      node$translation <- vertices[i,]
      if (i > 1) {
        for (j in seq_along(childnodes)) {
          prevnode <- result$getNode(childnodes[j])
          newnum <- result$addNode()
          result$setNode(newnum, prevnode)
          childnodes[j] <- newnum
        }
      }
      node$children <- childnodes
      result$setNode(child, node)
    }
    thisNode$children <- children
    x$objects <- NULL # They have been copied into their own objects
    thisNode$extras <- asRGLobj(x)
    result$setNode(thisNodeNum, thisNode)
    thisNodeNum
  }

  result <- previous
  if (!missing(rglscene))
    defaultMaterial <- rglscene$material
  else
    defaultMaterial <- material3d()

  sprites <- FALSE
  if (spheres <- isRGL(x, "spheres"))
    node <- addSpheres(x)
  else if (sprites <- isRGL(x, "sprites"))
    node <- addSprites(x) # only 3D sprites here
  else {
    if (missing(vertices)) {
      if (!missing(x)) {
        xyz <- xyz.coords(x, y, z, recycle = TRUE)
        vertices <- rbind(xyz$x, xyz$y, xyz$z)
      } else
        vertices <- NULL
    } else if (length(vertices))
      vertices <- asEuclidean2(vertices)
    else
      vertices <- NULL

    if (!is.null(texcoords)) {
      texcoords[,2] <- 1-texcoords[,2]
    }
    attributes <- as.list(c(POSITION = result$writeVectors(vertices, types = "float"),
                            NORMAL = result$writeVectors(tnonnull(normals), types = "float"),
                            TEXCOORD_0 = result$writeVectors(tnonnull(texcoords), types = "float"),
                            COLOR_0 = if (is.multicolored(material)) writeColors(material)
    ))

    matnum <- result$addMaterial(material, defaultMaterial)

    primitives <- c(addPrimitive(points, modePoints),
                    addPrimitive(segments, modeSegments),
                    addPrimitive(triangles, modeTriangles),
                    addPrimitive(cbind(quads[1:3,], quads[c(1,3,4),]),
                                 modeTriangles))

    mesh <- result$addMesh(primitives)
    node <- result$addNode(mesh, matrix = transform, extras = extras)
  }
  if (is.null(parentNode) && !spheres && !sprites) {
    if (newScene)
      scene <- result$addScene()
    else
      scene <- result$defaultScene()

    result$addToScene(scene, node)
  } else
    result$addChild(parentNode, node)

  result$closeBuffers()

  if (!length(result$getAsset()))
    result$setAsset(version = "2.0",
                    generator = paste("rgl2gltf version ", packageVersion("rgl2gltf")))

  result
}

as.gltf.rglspheres <- function(x, ...)
  as.gltf.default(x, ...)

as.gltf.rglsprites <- function(x, scale = c(1,1,1), ...) {
  if (is.null(x$objects)) {
    quad <- cbind(x = c(-1, 1, 1, -1) / scale[1],
                  y = c( 0, 0, 0, 0) / scale[2],
                  z = c(-1, -1, 1, 1) / scale[3])/2
    texcoords <- cbind(s = c(0, 1, 1, 0),
                       t = c(0, 0, 1, 1))
    vertices <- x$vertices
    n <- nrow(vertices)
    radii <- rep(x$radii, length = n)
    xyz <- matrix(NA_real_, ncol = 3, nrow = 4*n)
    for (i in seq_len(n)) {
      xyz[4*(i-1) + 1:4,] <- translate3d(scale3d(quad, radii[i], radii[i], radii[i]), vertices[i, 1], vertices[i, 2], vertices[i, 3])
    }
    mat <- x$material
    if (!is.null(mat) && length(mat$color) > 1)
      mat$color <- rep(mat$color, each = 4)
    as.gltf.default(vertices = t(xyz),
                    material = mat,
                    texcoords = texcoords[rep(1:4, n),],
                    quads = matrix(seq_len(4*n), nrow=4),
                    extras = asRGLobj(x),
                    ...
    )
  } else  # 3D sprites are handled by the default method
    as.gltf.default(x, scale = scale, ...)
}

as.gltf.rglsubscene <- function(x, previous = Gltf$new(), rglscene = list(), parentNode = NULL, ...) {
  transform <- x$par3d$userMatrix
  if (!is.null(scale <- x$par3d$scale))
    transform <- transform %*% scaleMatrix(scale[1], scale[2], scale[3])
  else
    scale <- c(1,1,1)
  subscenes <- x$subscenes
  x$subscenes <- NULL
  previous <- as.gltf.default(vertices = NULL,
                  transform = transform,
                  previous = previous,
                  parentNode = parentNode,
                  extras = asRGLobj(x), ...)

  thisNode <- previous$listCount("nodes") - 1
  for (i in seq_along(x$objects)) {
    previous <- as.gltf(rglscene$objects[[as.character(x$objects[i])]], previous = previous,
                        newScene = FALSE,
                        rglscene = rglscene,
                        parentNode = thisNode,
                        scale = scale)
  }
  for (i in seq_along(subscenes)) {
    previous <- as.gltf(subscenes[[i]],
                        previous = previous,
                        rglscene = rglscene,
                        parentNode = thisNode)
  }
  previous
}

as.gltf.rglbackground <- function(x, ...) {
  as.gltf(vertices = NULL, extras = asRGLobj(x), ...)
}

as.gltf.rglbboxdeco <- function(x, parentNode = NULL, previous = Gltf$new(), ...) {
  if (!is.null(parentNode) && !is.null(parent <- previous$getNode(parentNode))) {
    subscene <- parent$extras$RGL_obj
    bbox <- subscene$par3d$bbox
    cube <- cube3d()
    cube <- scale3d(translate3d(cube, 1, 1, 1), 0.5, 0.5, 0.5)
    cube <- scale3d(cube, bbox[2]-bbox[1], bbox[4]-bbox[3], bbox[6]-bbox[5])
    cube <- translate3d(cube, bbox[1], bbox[3], bbox[5])
    indices <- cbind(cube$ib[1:2,], cube$ib[2:3,], cube$ib[3:4], cube$ib[c(4, 1),])
    ind1 <- apply(indices, 2, sort)
    keep <- !duplicated(t(ind1))
    indices <- indices[,keep]
    vertices <- cube$vb
  } else {
    indices <- NULL
    vertices <- NULL
  }
  as.gltf(vertices = vertices,
          segments = indices,
          parentNode = parentNode,
          previous = previous,
          material = x$material,
          extras = asRGLobj(x),
          ...)
}

as.gltf.rgltext <- function(x, ...) {
  if (!is.null(x$material) && isTRUE(x$material$floating))
    as.gltf.default(extras = asRGLobj(x),
                    ...)
  else
    as.gltf.default(x = x$vertices,
                  material = x$material,
                  points = seq_len(nrow(x$vertices)),
                  extras = asRGLobj(x),
                  ...)
}

as.gltf.rglobject <- function(x, ..., previous = Gltf$new()) {
  # Some objects can't be converted
  if (x$type %in% c("light")) {
    previous <- as.gltf.default(extras = asRGLobj(x),
                                previous = previous,
                                ...)
  } else if (x$type %in% c("points", "linestrip", "lines",
                           "triangles", "quads")) {
    if (is.null(indices <- x$indices))
      indices <- seq_len(nrow(x$vertices))
    n <- length(indices)
    points <- NULL
    segments <- NULL
    triangles <- NULL
    quads <- NULL
    switch(x$type,
      points =    { points <- indices },
      linestrip = { segments <- rbind(indices[-n],
                                     indices[-1])},
      lines =     { segments <- matrix(indices, nrow = 2)},
      triangles = { triangles <- matrix(indices, nrow = 3)},
      quads =     { quads <- matrix(indices, nrow = 4)})

    vertices <- x$vertices
    texcoords <- x$texcoords
    normals <- x$normals
    x$vertices <- x$texcoords <- x$normals <- x$indices <- NULL
    previous <- as.gltf.default(x = vertices,
                    texcoords = texcoords,
                    normals = normals,
                    material = x$material,
                    points = points,
                    segments = segments,
                    triangles = triangles,
                    quads = quads,
                    extras = asRGLobj(x),
                    previous = previous,
                    ...)
  } else {
    # Not a type we know how to handle yet; try to convert to a mesh first
    m <- as.mesh3d(x)
    if (is.null(m))
      warning("Objects of type ", x$type, " are not yet supported.",
            call. = FALSE)
    else
      previous <- as.gltf(m, ..., previous = previous)
  }
  previous
}

as.gltf.rglscene <- function(x, ..., previous = Gltf$new(), newScene = FALSE) {
  if (!is.null(x$material)) {
    extras <- previous$getExtras()
    if (is.null(extras$RGL_material)) {
      extras$RGL_material <- x$material
      previous$setExtras(extras)
    }
  }

  previous <- as.gltf(x$rootSubscene, previous = previous, newScene = newScene, rglscene = x)
  previous
}


# Convert a mesh3d object to glTF JSON and associated files

as.gltf.mesh3d <- function(x, ...) {
  if (is.null(x$normals) && (!is.null(x$it) || !is.null(x$ib)))
    x <- addFaceNormals(x)
  as.gltf.default(vertices = x$vb,
                  material = x$material,
                  normals = euclidean(x$normals, TRUE),
                  texcoords = if (!is.null(x$texcoords)) t(x$texcoords),
                  points = x$ip,
                  segments = x$is,
                  triangles = x$it,
                  quads = x$ib, ...)
}

# Convert a shapelist3d object to glTF
as.gltf.shapelist3d <- function(x, previous = Gltf$new(), newScene = FALSE, ...) {
  for (i in seq_along(x)) {
    previous <- as.gltf(x[[i]],
                      previous = previous,
                      newScene = newScene && (i == 1),
                      ...)
  }
  previous
}
