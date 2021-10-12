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

as.gltf.mesh3d <- function(x, ...) {
  as.gltf.default(vertices = x$vb,
                  material = x$material,
                  normals = euclidean(x$normals, TRUE),
                  texcoords = if (!is.null(x$texcoords)) t(x$texcoords),
                  points = x$ip,
                  segments = x$is,
                  triangles = x$it,
                  quads = x$ib, ...)
}

as.gltf.rglobject <- function(x, ..., previous = list()) {
  m <- as.mesh3d(x)
  if (is.null(m))
    warning("Objects of type ", x$type, " are not yet supported.",
          call. = FALSE)
  else previous <- as.gltf(m, ..., previous = previous)
  previous
}

as.gltf.rglscene <- function(x, ..., previous = list(), newScene = FALSE) {
  for (i in seq_along(x$objects))
    previous <- as.gltf(x$objects[[i]], previous = previous,
                        newScene = newScene && i == 1)
  previous
}

as.gltf.default <- function(x, y = NULL, z = NULL, vertices,
                            material = NULL,
                            normals = NULL,
                            texcoords = NULL,
                            points = NULL, segments = NULL,
                            triangles = NULL,
                            quads = NULL,
                            ...,
                            previous = list(),
                            newScene = FALSE,
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

  addMaterial <- function(mat) {
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
    result$materials <<- c(result$materials, list(material))
    length(result$materials) - 1
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
    mesh <- list(primitives = primitives)
    result$meshes <<- c(result$meshes, list(mesh))
    length(result$meshes) - 1
  }

  addNode <- function(mesh) {
    node <- list(mesh = mesh)
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

  tnonnull <- function(x)
    if(!is.null(x)) t(x)

  result <- previous

  if (missing(vertices)) {
    xyz <- xyz.coords(x, y, z, recycle = TRUE)
    vertices <- rbind(xyz$x, xyz$y, xyz$z)
  } else
    vertices <- asEuclidean2(vertices)

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
  node <- addNode(mesh)
  if (newScene)
    scene <- addScene()
  else
    scene <- defaultScene()

  addToScene(scene, node)

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
