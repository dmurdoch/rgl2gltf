as.gltf <- function(x, ...) {
  UseMethod("as.gltf")
}

# Convert a mesh3d object to glTF JSON and associated files

as.gltf.mesh3d <- function(x, result = list(), newScene = FALSE, dir = tempdir(), ...) {

  typeUnsignedInt <- 5125
  typeDouble <- 5126

  targetArray <- 34962
  targetElementArray <- 34963

  modePoints <- 0
  modeSegments <- 1
  modeTriangles <- 4


  cleanVertices <- function(x)
    x

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

  writeBuffer <- function(values, type) {
    buffer <- getBuffer()
    result <- buffer$byteLength
    seek(buffer$bufferdata, result)
    if (type == typeDouble)
      values <- as.numeric(values)
    else if (type == typeUnsignedInt)
      values <- as.integer(values)
    writeBin(values, buffer$bufferdata, size = 4, endian = "little")
    buffer$byteLength <- seek(buffer$bufferdata, NA)
    result$buffers[[1]] <<- buffer
    result
  }

  addBufferView <- function(values, type) {
    buffer <- 0
    byteLength <- 4*length(values)
    byteOffset <- writeBuffer(values, type)
    target <- if (is.integer(values)) targetElementArray else
                                      targetArray
    bufferview <- list(buffer = buffer,
                       byteLength = byteLength,
                       byteOffset = byteOffset,
                       target = target)
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

  addAccessor <- function(coords) {
    componentType <- getType(coords)
    if (componentType == typeDouble) {
      min <- minS  # These will only last for one call!
      max <- maxS
    }
    bufferView <- addBufferView(c(coords), componentType)
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
    if (length(pbrMetallicRoughness))
      material$pbrMetallicRoughness <- pbrMetallicRoughness
    result$materials <<- c(result$materials, list(material))
    length(result$materials) - 1
  }

  writeVectors <- function(coords) {
    if (!is.null(coords)) {
      addAccessor(coords)
    } else
      NULL
  }

  addPrimitive <- function(inds, mode) {
    indices <- as.integer(inds)
    if (length(indices)) {
      indices <- addAccessor(indices - 1L)
      primitive <- list(attributes = attributes,
                        material = material,
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

  euclidean <- function(x) {
    if (!is.null(x))
      if (any(is.na(x))) {
        warning("Cannot write NA values")
        NULL
      } else
        asEuclidean2(x)
  }

  x <- cleanVertices(x)  # Remove any NA, NaN or Inf values

  attributes <- as.list(c(POSITION = writeVectors(euclidean(x$vb)),
                     NORMAL = writeVectors(euclidean(x$normals)),
                     TEXCOORD_0 = writeVectors(x$texCoords),
                     COLOR_0 = if (is.multicolored(x$material)) writeColors(x$material)
                       ))

  material <- addMaterial(x$material)

  primitives <- c(addPrimitive(x$ip, modePoints),
                  addPrimitive(x$is, modeSegments),
                  addPrimitive(x$it, modeTriangles),
                  addPrimitive(cbind(x$ib[1:3,], x$ib[c(1,3,4),]),
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

as.gltf.shapelist3d <- function(x, result = list(), newScene = FALSE, dir = tempdir(), ...) {
  for (i in seq_along(x)) {
    result <- as.gltf(x[[i]], result,
                      newScene = newScene && (i == 1),
                      dir = dir)
  }
  result
}
