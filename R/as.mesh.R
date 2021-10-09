as.mesh3d.gltf <- function(x, scene, verbose = FALSE, ...) {
  readBuffer <- function(buf) {
    buffer <- x$buffers[[buf+1]]
    bufferdata <- buffer$bufferdata
    if (is.null(bufferdata)) {
      uri <- buffer$uri
      if (is.null(uri))
        uri <- attr(x, "defaultbin")
      bufferdata <- rawConnection(readBin(uri, "raw", n = buffer$byteLength))
      x$buffers[[buf+1]]$bufferdata <<- bufferdata
    }
    bufferdata
  }

  readBufferview <- function(bufv) {
    bufferview <- x$bufferViews[[bufv+1]]
    class(bufferview) <- "gltfBufferview"
    bufferview$bufferdata <- readBuffer(bufferview$buffer)
    bufferview
  }

  readAccessor <- function(acc) {
    typenames <- c("5120" = "byte", "5121" = "unsigned_byte",
                   "5122" = "short", "5123" = "unsigned_short",
                   "5125" = "unsigned_int", "5126" = "float")
    types <- c("5120" = "int", "5121" = "int",
               "5122" = "int", "5123" = "int",
               "5125" = "int", "5126" = "double")
    sizes <- c("5120" = 1, "5121" = 1,
               "5122" = 2, "5123" = 2,
               "5125" = 4, "5126" = 4)
    signeds <- c("5120" = TRUE, "5121" = FALSE,
                 "5122" = TRUE, "5123" = FALSE,
                 "5125" = TRUE, # not really, but make readBin happy
                 "5126" = TRUE)
    lens <- c(SCALAR = 1, VEC2 = 2, VEC3 = 3, VEC4 = 4,
              MAT2 = 4, MAT3 = 9, MAT4 = 16)
    accessor <- x$accessors[[acc+1]]
    class(accessor) <- "gltfAccessor"
    view <- readBufferview(accessor$bufferView)
    con <- view$bufferdata
    ctype <- as.character(accessor$componentType)
    atype <- accessor$type
    type <- types[ctype]
    len <- lens[atype]
    size <- sizes[ctype]
    signed <- signeds[ctype]
    count <- accessor$count
    if (is.null(view$byteStride)) {
      skip <- 0
    } else
      skip <- len*size - view$byteStride
    if (is.null(byteOffset <- accessor$byteOffset))
      byteOffset <- 0
    start <- view$byteOffset + byteOffset

    if (skip == 0) {
      seek(con, start)
      values <- readBin(con, type, n = len*count,  size = size,
                        signed = signed, endian = "little")
    } else {
      values <- numeric(count*len)
      for (i in seq_len(count)) {
        seek(con, start + (i-1)*view$byteStride)
        values[(i-1)*len + seq_len(len)] <-
          readBin(con, type, n = len,  size = size,
                  signed = signed, endian = "little")
      }
    }
    if (ctype == "5125") { # fix up unsigned integers
      values[is.na(values)] <- 2^31
      values[values < 0] <- values[values < 0] + 2^32
    }
    if (len > 1)
      if (grepl("MAT", atype)) {
        values <- matrix(values, ncol = sqrt(len), byrow = TRUE)
      } else
        values <- matrix(values, ncol = len, byrow = TRUE)
    values
  }

  getTransform <- function(node, parentTransform) {
    if (!is.null(node$matrix)) {
      transform <- matrix(unlist(node$matrix), 4, 4)
    } else {
      transform <- diag(4)
      if (!is.null(node$scale)) {
        scale <- unlist(node$scale)
        transform <- t(scaleMatrix(scale[1], scale[2], scale[3])) %*% transform
      }
      if (!is.null(node$rotation)) {
        rot <- unlist(node$rotation)
        transform <- t(rotationMatrix(rot[4], rot[1], rot[2], rot[3])) %*% transform
      }
      if (!is.null(node$translation)) {
        trans <- unlist(node$translation)
        transform <- t(translationMatrix(trans[1], trans[2], trans[3]))
      }
    }
    parentTransform %*% transform
  }

  processNode <- function(n, parentTransform) {
    node <- x$nodes[[n + 1]]
    class(node) <- "gltfNode"
    transform <- getTransform(node, parentTransform)
    if (!is.null(node$mesh)) {
      inmesh <- x$meshes[[node$mesh+1]]
      if (verbose && !is.null(inmesh$name))
        cat(inmesh$name, "\n")
      for (p in seq_along(inmesh$primitives)) {
        prim <- inmesh$primitives[[p]]
        class(prim) <- "gltfPrimitive"
        if (!is.null(prim$targets)) {
          print(prim)
          browser()
        }
        normals <- NULL
        positions <- NULL
        for (a in seq_along(prim$attributes)) {
          attr <- prim$attributes[a]
          values <- readAccessor(attr[[1]])
          switch (names(attr),
                  NORMAL = normals <- values,
                  POSITION = positions <- values
          )
        }
        if (is.null(prim$indices))
          indices <- seq_len(nrow(positions))
        else
          indices <- readAccessor(prim$indices) + 1
        if (is.null(mode <- prim$mode))
          mode <- 4
        ninds <- length(indices)
        newmesh <- switch(as.character(mode),
          "0" = mesh3d(x = positions,    # points
                            normals = normals,
                            points = indices,
                            material = list(color = "black")),
          "1" = mesh3d(x = positions,    # segments
                            normals = normals,
                            segments = matrix(indices, nrow = 2),
                            material = list(color = "black")),
          "2" = mesh3d(x = positions,    # loop
                            normals = normals,
                            segments = rbind(indices,
                                             c(indices[-1], indices[1])),
                            material = list(color = "black")),
          "3" = mesh3d(x = positions,    # strip
                            normals = normals,
                            segments = rbind(indices[-length(indices)],
                                             indices[-1]),
                            material = list(color = "black")),
          "4" = mesh3d(x = positions,    # triangles
                            normals = normals,
                            triangles = matrix(indices, nrow = 3),
                            material = list(color = "white")),
          "5" = mesh3d(x = positions,    # triangle strip
                       normals = normals,
                       triangles = rbind(indices[-c(ninds, ninds-1)],
                                         indices[-c(1, ninds)],
                                         indices[-c(1,2)]),
                       material = list(color = "white")),
          "6" = mesh3d(x = positions,    # triangle fan
                       normals = normals,
                       triangles = rbind(indices[1],
                                         indices[-c(1, ninds)],
                                         indices[-c(1,2)]),
                       material = list(color = "white")))
        newmesh <- rotate3d(newmesh, matrix = t(transform))
        outmeshes[[nextmesh]] <<- newmesh
        nextmesh <<- nextmesh + 1
      }
    }

    for (child in unlist(node$children))
      processNode(child, transform)
  }

  on.exit({
    for (i in seq_along(x$buffers))
      if (!is.null(con <- x$buffers[[i]]$bufferdata))
        close(con)
  })

  if (missing(scene)) {
    scene <- 0
    if (!is.null(x$scene))
      scene <- x$scene
  }
  if (scene + 1 > length(x$scenes))
    stop("scene ", scene, " not found.")

  outmeshes <- list()
  nextmesh <- 1
  nodes <- x$scenes[[scene+1]]$nodes
  for (n in nodes) {
    processNode(n, parentTransform = diag(4))
  }
  shapelist3d(outmeshes)
}
