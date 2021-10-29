as.node <- function(x, ...) {
  class(x) <- "gltfNode"
  x
}

as.gltf <- function(x, ...) {
  UseMethod("as.gltf")
}

euclidean <- function(x, transpose = TRUE) {
  if (!is.null(x))
    if (any(is.na(x))) {
      warning("Cannot write NA values")
      NULL
    } else if (transpose)
      t(asEuclidean2(x))
  else asEuclidean(x)
}

# Convert a mesh3d object to glTF JSON and associated files

dot <- function(v, w) sum(v*w)

cross <- function(v, w) c( v[2]*w[3] - v[3]*w[2],
                  v[3]*w[1] - v[1]*w[3],
                  v[1]*w[2] - v[2]*w[1] )

addFaceNormals <- function(x) {
  x <- as.tmesh3d(x)

  vertices <- asEuclidean2(x$vb)
  normals <- NA*vertices
  texcoords <- x$texcoords
  it <- x$it
  for (i in seq_len(ncol(x$it))) {
    normal <- cross(vertices[,it[2,i]] - vertices[,it[1,i]],
                                  vertices[,it[3,i]] - vertices[,it[2,i]])
    normal <- normal/sqrt(dot(normal, normal))
    for (j in 1:3) {
      if (is.na(normals[1, it[j, i]]))
        normals[, it[j, i]] <- normal
      else if (!isTRUE(all.equal(normal, normals[, it[j, i]]))) {
        # duplicate the vertex
        vertices <- cbind(vertices, vertices[,it[j, i]])
        normals <- cbind(normals, normal)
        if (!is.null(texcoords))
          texcoords <- cbind(texcoords, texcoords[,it[j, i]])
        it[j, i] <- ncol(vertices)
      }
    }
  }
  x$vb <- asHomogeneous2(vertices)
  x$normals <- asHomogeneous2(normals)
  if (!is.null(texcoords))
    x$texcoords <- texcoords
  x$it <- it
  x
}

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

as.gltf.rglpoints <- function(x, ...) {
  if (is.null(indices <- x$indices))
    indices <- seq_len(nrow(x$vertices))
  as.gltf.default(x = x$vertices,
                  material = x$material,
                  points = indices, ...)
}

as.gltf.rgllinestrip <- function(x, ...) {
  if (is.null(indices <- x$indices))
    indices <- seq_len(nrow(x$vertices))
  n <- length(indices)
  as.gltf.default(x = x$vertices,
                  material = x$material,
                  segments = rbind(indices[-n],
                                   indices[-1]),
                  ...)
}

as.gltf.rgllines <- function(x, ...) {
  if (is.null(indices <- x$indices))
    indices <- seq_len(nrow(x$vertices))
  as.gltf.default(x = x$vertices,
                  material = x$material,
                  segments = matrix(indices, nrow = 2),
                  ...)
}

as.gltf.rgltriangles <- function(x, ...) {
  if (is.null(indices <- x$indices))
    indices <- seq_len(nrow(x$vertices))
  as.gltf.default(x = x$vertices,
                  normals = x$normals,
                  texcoords = x$texcoords,
                  material = x$material,
                  triangles = matrix(indices, nrow = 3),
                  ...)
}

as.gltf.rglquads <- function(x, ...) {
  if (is.null(indices <- x$indices))
    indices <- seq_len(nrow(x$vertices))
  as.gltf.default(x = x$vertices,
                  normals = x$normals,
                  texcoords = x$texcoords,
                  material = x$material,
                  quads = matrix(indices, nrow = 4),
                  ...)
}

as.gltf.rglspheres <- function(x, previous = list(), parentNode = NULL, ...) {
  as.gltf.default(x, spheres = TRUE,
                  previous = previous,
                  parentNode = parentNode)
}

as.gltf.rglsubscene <- function(x, previous = list(), rglscene = list(), parentNode = NULL, ...) {
  transform <- x$par3d$userMatrix
  if (!is.null(scale <- x$par3d$scale))
    transform <- transform %*% scaleMatrix(scale[1], scale[2], scale[3])
  previous <- as.gltf.default(vertices = NULL,
                  transform = transform,
                  previous = previous,
                  parentNode = parentNode,
                  extras = asRGLobj(x), ...)

  thisNode <- length(previous$nodes) - 1
  for (i in seq_along(x$objects)) {
    previous <- as.gltf(rglscene$objects[[as.character(x$objects[i])]], previous = previous,
                        newScene = FALSE,
                        rglscene = rglscene,
                        parentNode = thisNode)
  }
  for (i in seq_along(x$subscenes)) {
    previous <- as.gltf(x$subscenes[[i]],
                        previous = previous,
                        rglscene = rglscene,
                        parentNode = thisNode)
  }
  previous
}

as.gltf.rglbackground <- function(x, ...) {
  as.gltf(vertices = NULL, extras = asRGLobj(x), ...)
}

as.gltf.rglbboxdeco <- function(x, parentNode = NULL, previous = list(), ...) {
  if (!is.null(parentNode) && !is.null(parent <- previous$nodes[[parentNode + 1]])) {
    class(parent) <- "gltfNode"
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
          extras = asRGLobj(x),
          ...)
}

asRGLobj <- function(x) {
  x <- c(unclass(x), class=class(x))
  list(RGL_obj = x)
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

as.gltf.rglobject <- function(x, ..., previous = list()) {
  # Some objects can't be converted
  if (x$type %in% c("light")) {
    # do nothing
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

as.gltf.rglscene <- function(x, ..., previous = list(), newScene = FALSE) {
  previous <- as.gltf(x$rootSubscene, previous = previous, newScene = newScene, rglscene = x)
  previous
}

as.gltf.default <- function(x, y = NULL, z = NULL, vertices,
                            material = NULL,
                            normals = NULL,
                            texcoords = NULL,
                            points = NULL, segments = NULL,
                            triangles = NULL,
                            quads = NULL,
                            spheres = FALSE,
                            transform = NULL,
                            extras = NULL,
                            ...,
                            rglscene = list(),
                            previous = list(),
                            newScene = FALSE,
                            parentNode = NULL,
                            dir = tempdir()) {

  typeUnsignedByte <- 5121
  typeUnsignedShort <- 5123
  typeUnsignedInt <- 5125
  typeDouble <- 5126

  targetArray <- 34962
  targetElementArray <- 34963

  modePoints <- 0
  modeSegments <- 1
  modeTriangles <- 4

  getBuffer <- function() {
    if (is.null(result$buffers)) {
      filename <- tempfile(fileext = "bin", tmpdir = dir)
      result$buffers <<- list(list(uri = filename,
                                byteLength = 0))
    }
    buffer <- result$buffers[[1]]
    if (is.null(buffer$bufferdata)) {
      filename <- buffer$uri
      buffer$bufferdata <- file(filename, open = "ab")
      result$buffers[[1]] <<- buffer
    }

    result$buffers[[1]]
  }

  writeBuffer <- function(values, type, size) {
    buffer <- getBuffer()
    byteOffset <- buffer$byteLength
    seek(buffer$bufferdata, byteOffset)
    byteOffset <- bitwAnd(byteOffset + size - 1, bitwNot(size - 1))
    if (byteOffset > buffer$byteLength) {
      writeBin(raw(byteOffset - buffer$byteLength),
               buffer$bufferdata)
      buffer$byteLength <- byteOffset
    }
    if (type == typeDouble)
      values <- as.numeric(values)
    else if (type == typeUnsignedInt)
      values <- as.integer(values)
    writeBin(values, buffer$bufferdata, size = size, endian = "little")
    buffer$byteLength <- seek(buffer$bufferdata, NA)
    result$buffers[[1]] <<- buffer
    byteOffset
  }

  addBufferView <- function(values, type, size, target = NULL) {
    bufferview <- list()
    bufferview$buffer <- 0
    bufferview$byteLength <- size*length(values)
    bufferview$byteOffset <- writeBuffer(values, type, size)
    if (!is.null(target))
      bufferview$target <- target
    result$bufferViews <<- c(result$bufferViews, list(bufferview))
    length(result$bufferViews) - 1
  }

  getType <- function(x) {
    if (is.integer(x))
      typeUnsignedInt
    else if (is.numeric(x))
      typeDouble
    else
      stop('Unrecognized type')
  }

  toSingle <- function(x) {
    con <- rawConnection(raw(), "r+b")
    on.exit(close(con))
    writeBin(x, con, size = 4)
    seek(con, 0)
    readBin(con, "double", n = length(x), size=4)
  }

  minS <- function(x) min(toSingle(x))
  maxS <- function(x) max(toSingle(x))

  addAccessor <- function(coords, target = NULL) {
    componentType <- getType(coords)
    if (componentType == typeDouble) {
      min <- minS  # These will only last for one call!
      max <- maxS
      size <- 4
    } else {
      r <- range(coords)
      if (r[1] < 0) {
        size <- 4
        warning("values appear to be signed integer!")
      } else if (r[2] < 2^8) {
        size <- 1
        componentType <- typeUnsignedByte
      } else if (r[2] < 2^16) {
        size <- 2
        componentType <- typeUnsignedShort
      }
    }
    bufferView <- addBufferView(c(coords), componentType,
                                size = size, target = target)
    if (is.matrix(coords)) {
      count <- ncol(coords)
      type <- paste0("VEC", nrow(coords))
      max <- I(apply(coords, 1, max))
      min <- I(apply(coords, 1, min))
      if (any(is.na(min)))
        browser()
    } else {
      count <- length(coords)
      type <- "SCALAR"
      max <- I(max(coords))
      min <- I(min(coords))
    }
    accessor <- list(bufferView = bufferView,
                     componentType = componentType,
                     count = count,
                     type = type,
                     max = max,
                     min = min)
    result$accessors <<- c(result$accessors, list(accessor))
    length(result$accessors) - 1
  }

  is.multicolored <- function(mat)
    length(unique(mat$color)) > 1 || length(unique(mat$alpha)) > 1

  writeColors <- function(mat) {
    # We know we're multicolored already
    n <- max(length(mat$color), length(mat$alpha))
    if (length(mat$color))
      col <- col2rgb(rep_len(mat$color, n))/255
    else
      col <- matrix(1, nrow = 3, ncol = n)
    if (length(mat$alpha) && any(mat$alpha != 1))
      col <- rbind(col, rep_len(mat$alpha, n))
    writeVectors(col)
  }

  # The material is in glTF format; have
  # we recorded it already?
  getMaterialNumber <- function(material) {
    materials <- result$materials
    for (i in seq_along(materials))
      if (identical(materials[[i]], material)) {
        return(i - 1)
      }

    result$materials <<- c(result$materials, list(material))
    length(result$materials) - 1
  }

  addMaterial <- function(mat) {
    newmat <- defaultMaterial
    newmat[names(mat)] <- mat
    mat <- newmat
    material <- list()

    pbrMetallicRoughness <- list()
    col <- c(1,1,1,1)
    if (!is.multicolored(mat)) {
      if (!is.null(mat$color))
        col[1:3] <- col2rgb(mat$color)/255
      if (!is.null(mat$alpha))
        col[4] <- mat$alpha
      if (any(col != 1))
        pbrMetallicRoughness$baseColorFactor <- col
    }
    if (!is.null(mat$emission))
      material$emissiveFactor <- c(col2rgb(mat$emission)/255)
    if (!is.null(mat$texture))
      pbrMetallicRoughness$baseColorTexture <- addTexture(mat)
    if (length(pbrMetallicRoughness))
      material$pbrMetallicRoughness <- pbrMetallicRoughness

    # Some properties have already been handled
    mat$color <- mat$alpha <- mat$emission <- mat$texture <- NULL
    # Include the rest as an extension
    material$extras <- list(RGL_material_properties = mat)
    getMaterialNumber(material)
  }

  addTexture <- function(mat) {
    texture <- list()
    texture$source <- addImage(mat)
    texture$sampler <- addSampler(mat)
    texture$name <- basename(mat$texture)
    result$textures <<- c(result$textures, list(texture))
    list(index = length(result$textures) - 1)
  }

  addImage <- function(mat) {
    image <- list()
    bytes <- readBin(mat$texture, "raw", file.size(mat$texture))
    image$bufferView <- addBufferView(bytes, typeUnsignedInt, size = 1)
    image$mimeType <- "image/png"
    image$name <- basename(mat$texture)
    result$images <<- c(result$images, list(image))
    length(result$images) - 1
  }

  getFilter <- function(filter) {
    if (!is.null(filter))
      c("nearest" = 9728, "linear" = 9729,
        "nearest.mipmap.nearest" = 9984,
        "linear.mipmap.nearest" = 9985,
        "nearest.mipmap.linear" = 9986,
        "linear.mipmap.linear" = 9987)[filter]
  }

  addSampler <- function(mat) {
    sampler <- list()
    sampler$magFilter <- getFilter(mat$texmagfilter)
    sampler$minFilter <- getFilter(mat$texminfilter)
    if (length(sampler)) {
      result$samplers <<- c(result$samplers, list(sampler))
      length(result$samplers) - 1
    }
  }

  writeVectors <- function(coords) {
    if (!is.null(coords)) {
      addAccessor(coords, targetArray)
    } else
      NULL
  }

  addPrimitive <- function(inds, mode) {
    indices <- as.integer(inds)
    if (length(indices)) {
      indices <- addAccessor(indices - 1L, targetElementArray)
      primitive <- list(attributes = attributes,
                        material = matnum,
                        mode = mode,
                        indices = indices
                       )
      list(primitive)
    }
  }

  addMesh <- function(primitives) {
    if (length(primitives)) {
      mesh <- list(primitives = primitives)
      result$meshes <<- c(result$meshes, list(mesh))
      length(result$meshes) - 1
    }
  }

  addNode <- function(mesh = NULL, matrix = NULL, extras = NULL) {
    node <- list()
    node$mesh <- mesh
    if (!is.null(matrix))
      node$matrix <- as.numeric(matrix)
    if (!is.null(extras))
      node$extras <- extras
    result$nodes <<- c(result$nodes, list(node))
    length(result$nodes) - 1
  }

  addScene <- function() {
    scene <- list()
    result$scenes <<- c(result$scenes, list(scene))
    result$scene <<- length(result$scenes) - 1
    result$scene
  }

  defaultScene <- function() {
    if (is.null(result$scene))
      addScene()
    result$scene
  }

  addToScene <- function(scene, node) {
    sceneobj <- result$scenes[[scene + 1]]
    sceneobj$nodes <- I(c(sceneobj$nodes, node))
    result$scenes[[scene + 1]] <<- sceneobj
  }

  addChild <- function(parent, node) {
    parentobj <- result$nodes[[parent + 1]]
    parentobj$children <- I(c(parentobj$children, node))
    result$nodes[[parent + 1]] <<- parentobj
  }

  tnonnull <- function(x)
    if(!is.null(x)) t(x)

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
    sphereAttributes <- list(vertices = writeVectors(rbind(x, y, z)),
                              texcoords = writeVectors(rbind(s, t)),
                              indices = addAccessor(c(it), targetElementArray))
    result$extras <<- c(result$extras, list(RGL_sphere = sphereAttributes))
  }

  addSpheres <- function(x) {
    if (is.null(result$extras) || is.null(result$extras$RGL_sphere))
      makeSphere()
    sphere <- result$extras$RGL_sphere

    scale <- c(1,1,1)
    if (!is.null(parentNode)) {
      parent <- result$nodes[[parentNode + 1]]
      if (!is.null(parent$extras) &&
          !is.null(parentSub <- parent$extras$RGL_obj) &&
          !is.null(par3d <- parentSub$par3d) &&
          !is.null(par3d$scale)) {
        scale <- scale/par3d$scale
      }
    }
    vertices <- x$vertices
    n <- nrow(vertices)
    material <- x$material
    radii <- rep(x$radii, length = n)
    i <- seq_len(nrow(x$colors))
    colors <- x$colors[rep(i, length.out = n),]
    children <- c()
    primitive <- list(list(attributes = c(POSITION = sphere$vertices,
                                          NORMALS = sphere$vertices,
                                          TEXCOORDS = sphere$texcoords),
                           mode = modeTriangles,
                           indices = sphere$indices))
    for (i in seq_len(n)) {
      material$color <- rgb(colors[i, 1], colors[i, 2],
                            colors[i, 3], colors[i, 4])
      matnum <- addMaterial(material)
      primitive[[1]]$material <- matnum
      mesh <- addMesh(primitive)
      child <- addNode(mesh)
      children <- c(children, child)
      node <- result$nodes[[child + 1]]
      node$scale <- rep(radii[i], 3)*scale
      node$translation <- vertices[i,]
      result$nodes[[child + 1]] <<- node
    }
    main <- addNode(extras = asRGLobj(x))
    node <- result$nodes[[main + 1]]
    node$children <- I(children)
    result$nodes[[main + 1]] <<- node
    if (newScene)
      scene <- addScene()
    else
      scene <- defaultScene()
    main
  }

  result <- previous
  if (!missing(rglscene))
    defaultMaterial <- rglscene$material
  else
    defaultMaterial <- material3d()

  if (spheres)
    node <- addSpheres(x)
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

    if (!is.null(texcoords))
      texcoords[,2] <- -texcoords[,2]

    attributes <- as.list(c(POSITION = writeVectors(vertices),
                            NORMAL = writeVectors(tnonnull(normals)),
                            TEXCOORD_0 = writeVectors(tnonnull(texcoords)),
                            COLOR_0 = if (is.multicolored(material)) writeColors(material)
    ))

    matnum <- addMaterial(material)

    primitives <- c(addPrimitive(points, modePoints),
                    addPrimitive(segments, modeSegments),
                    addPrimitive(triangles, modeTriangles),
                    addPrimitive(cbind(quads[1:3,], quads[c(1,3,4),]),
                                 modeTriangles))

    mesh <- addMesh(primitives)
    node <- addNode(mesh, matrix = transform, extras = extras)
  }
  if (is.null(parentNode) && !spheres) {
    if (newScene)
      scene <- addScene()
    else
      scene <- defaultScene()

    addToScene(scene, node)
  } else
    addChild(parentNode, node)

  if (!is.null(result$buffers) &&
      !is.null(result$buffers[[1]]) &&
      !is.null(result$buffers[[1]]$bufferdata)) {
    close(result$buffers[[1]]$bufferdata)
    result$buffers[[1]]$bufferdata <- NULL
  }

  if (is.null(result$asset))
    result$asset <- list(version = "2.0",
                         generator = paste("rgl2gltf version ", packageVersion("rgl2gltf")))

  structure(result, class = "gltf")
}

as.gltf.shapelist3d <- function(x, previous = list(), newScene = FALSE, ...) {
  for (i in seq_along(x)) {
    previous <- as.gltf(x[[i]],
                      previous = previous,
                      newScene = newScene && (i == 1),
                      ...)
  }
  previous
}
