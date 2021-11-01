
as.rglscene.gltf <- function(x, scene = x$scene, nodes = NULL,
                             useRGLinfo = TRUE, ...) {

  matdiff <- function(mat) {
    for (m in names(mat)) {
      if (identical(mat[[m]], defaultmaterial[[m]]))
        mat[[m]] <- NULL
    }
    mat
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
    if (!is.null(accessor$sparse))
      warning("sparse accessors are not supported.")
    read <- readBufferview(accessor$bufferView, x)
    x <<- read[[2]]
    view <- read[[1]]
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
    if (!is.null(accessor$normalized) && accessor$normalized)
      values <- switch(ctype,
             "5120" = (values + 128)/255 - 1, # byte
             "5121" = values/255,             # u byte
             "5122" = (values + 2^15)/65535 - 1, # short
             "5123" = values/65535,           # u short
                      values)                 # default
    if (len > 1)
      if (grepl("MAT", atype)) {
        values <- matrix(values, ncol = sqrt(len), byrow = TRUE)
      } else
        values <- matrix(values, ncol = len, byrow = TRUE)
    values
  }

  getMaterial <- function(n) {
    if (is.null(n))
      result <- list()
    else {
      material <- x$materials[[n+1]]
      class(material) <- "gltfMaterial"
      result <- list(color = "white", alpha = 1)
      if (!is.null(pbrm <- material$pbrMetallicRoughness)) {
        if (!is.null(col <- unlist(pbrm$baseColorFactor))) {
          result$color <- rgb(col[1], col[2], col[3])
          result$alpha <- col[4]
        }
        if (!is.null(texture <- pbrm$baseColorTexture)) {
          texturefile <- extractTexture(x, texture$index,
                                        verbose = FALSE,
                                        closeConnections = FALSE)
          x <<- attr(texturefile, "gltf")
          mime <- attr(texturefile, "mimeType")
          if (!is.null(mime) && mime != "image/png")
            warning(sprintf("MIME type %s not supported as texture in rgl (texture %d).", mime, texture$index))
          attributes(texturefile) <- NULL
          result <- c(result, texture = texturefile,
                      list(gltftexCoord = texture$texCoord))
        }
      }
      if (!is.null(col <- unlist(material$emissiveFactor)))
        result$emission <- rgb(col[1], col[2], col[3])
      else
        result$emission <- "black"

      if (!is.null(ext <- material$extras)
          && !is.null(props <- ext$RGL_material_properties)) {
        result[names(props)] <- props
      } else
        result$specular <- "gray10"
    }
    result
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
        transform <- t(translationMatrix(trans[1], trans[2], trans[3])) %*% transform
      }
    }
    parentTransform %*% transform
  }

  saveTranslation <- function(oldid, newid) {
    idTranslations <<- c(idTranslations, newid)
    names(idTranslations)[length(idTranslations)] <<- oldid
  }

  applyidTranslations <- function(sub) {
    if (!is.null(idTranslations))
      sub$par3d$listeners <- idTranslations[as.character(sub$par3d$listeners)]
    sub
  }

  getId <- function(oldid) {
    lastid <<- lastid + 1
    if (!is.null(oldid))
      saveTranslation(oldid, lastid)
    lastid
  }

  newObj <- function(xyz, material = NULL, normals = NULL,
                           texcoords = NULL,
                           type, flags = NULL, attribs = NULL,
                           indices = indices,
                           oldid = NULL) {

    result <- list(id=getId(oldid), type=type)

    if (!(type %in% c("light", "clipplanes"))) {
      result$material <- matdiff(material)
    } else
      lit <- FALSE

    result$vertices <- xyz
    result$normals <- normals
    result$texcoords <- texcoords
    result$indices <- indices

    result <- c(result, attribs)

    if (length(flags)) {
      if ("ignoreExtent" %in% rownames(flags))
        result$ignoreExtent <- flags["ignoreExtent", 1]
      if ("fixedSize" %in% rownames(flags))
        result$fixedSize <- flags["fixedSize", 1]
      if ("fastTransparency" %in% rownames(flags))
        result$fastTransparency <- flags["fastTransparency", 1]
      if ("flipped" %in% rownames(flags))
        result$flipped <- flags["flipped", 1]
      if (type == "background") {
        result$sphere <- flags["sphere", 1]
        result$fogtype <- if (flags["linear_fog", 1]) "linear"
        else if (flags["exp_fog", 1]) "exp"
        else if (flags["exp2_fog", 1]) "exp2"
        else "none"
        result$fogscale <- attribs$fogscale
      } else if (type == "bboxdeco") {
        result$draw_front <- flags["draw_front", 1]
      } else if (type == "light") {
        result$viewpoint <- flags["viewpoint", 1]
        result$finite    <- flags["finite", 1]
      }
    }
    class(result) <- c(paste0("rgl", type), "rglobject")
    result
  }

  newSubscene <- function(parent) {
    result <- structure(list(id = getId(NULL),
                   par3d = list()),
              class = c("rglsubscene", "rglobject"))
    if (missing(parent)) { # i.e. root
      result$embeddings <- c(viewport = "replace",
                             projection = "replace",
                             model = "replace",
                             mouse = "replace")
      result$par3d$windowRect <- getDefaults("par3d", "windowRect",
          c(x = 0, y = 40, width = 512, height = 552))
      result$par3d$viewport <- getDefaults("par3d", "windowRect", result$par3d$windowRect - c(0, 40, 0, 40))
      result$par3d$userMatrix <- diag(4)
    } else {
      result$embeddings <- c(viewport = "inherit",
                             projection = "inherit",
                             model = "replace",
                             mouse = "inherit")
      result$par3d$viewport <- parent$par3d$viewport
      result$par3d$userMatrix <- parent$par3d$userMatrix
    }
    result$par3d$listeners <- result$id
    result
  }

  processPrimitive <- function(prim, transform) {
    class(prim) <- "gltfPrimitive"

    if (!is.null(prim$targets)) {
      print(prim)
      # What are prim targets????
      browser()
    }
    mat <- getMaterial(prim$material)
    normals <- NULL
    positions <- NULL
    texcoords <- NULL
    for (a in seq_along(prim$attributes)) {
      attr <- unlist(prim$attributes[a])
      values <- readAccessor(attr[1])
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
    else {
      indices <- readAccessor(prim$indices) + 1 # R indices start at 1
    }

    positions <- asEuclidean(asHomogeneous(positions) %*% t(transform))

    if (!is.null(normals)) {
      nt <- transform
      nt[4,1:3] <- nt[1:3, 4] <- 0
      nt <- solve(nt)
      normals <- normalize(asEuclidean(rotate3d(cbind(normals,1), matrix = nt)))
    }

    if (is.null(mode <- prim$mode))
      mode <- 4
    ninds <- length(indices)

    newobj <- switch(as.character(mode),
                     "0" = newObj(xyz = positions,    # points
                                  normals = normals,
                                  texcoords = texcoords,
                                  material = mat,
                                  indices = indices,
                                  type = "points"),
                     "1" = newObj(xyz = positions,    # segments
                                  normals = normals,
                                  texcoords = texcoords,
                                  material = mat,
                                  indices = indices,
                                  type = "lines"),
                     "2" = newObj(x = positions,    # loop
                                  normals = normals,
                                  texcoords = texcoords,
                                  material = mat,
                                  indices = c(indices, indices[1]),
                                  type = "linestrip"),
                     "3" = newObj(x = positions,    # strip
                                  normals = normals,
                                  texcoords = texcoords,
                                  material = mat,
                                  indices = indices,
                                  type = "linestrip"),
                     "4" = newObj(x = positions,    # triangles
                                  normals = normals,
                                  texcoords = texcoords,
                                  material = mat,
                                  indices = indices,
                                  type = "triangles"),
                     "5" = newObj(x = positions,    # triangle strip
                                  normals = normals,
                                  texcoords = texcoords,
                                  indices = rbind(indices[-c(ninds, ninds-1)],
                                                  indices[-c(1, ninds)],
                                                  indices[-c(1,2)]),
                                  material = mat),
                     "6" = newObj(x = positions,    # triangle fan
                                  normals = normals,
                                  texcoords = texcoords,
                                  indices = rbind(indices[1],
                                                  indices[-c(1, ninds)],
                                                  indices[-c(1,2)]),
                                  material = mat))
    activeSubscene$objects <<- union(activeSubscene$objects, newobj$id)
    objects <- c(rglscene$objects, list(newobj))
    names(objects)[length(objects)] <- newobj$id
    rglscene$objects <<- objects
  }

  processMesh <- function(m, transform) {
    mesh <- x$meshes[[m+1]]
    class(mesh) <- "gltfMesh"

    for (p in seq_along(mesh$primitives))
      processPrimitive(mesh$primitives[[p]], transform)
  }

  processPerspective <- function(persp) {
    par3d <- activeSubscene$par3d
    viewport <- par3d$viewport
    if (!is.null(ar <- persp$aspectRatio)) {
      viewport["width"] <- viewport["height"] * ar
      par3d$viewport <- viewport
      windowRect <- par3d$windowRect
      windowRect["width"] <- windowRect["width"] * ar
      par3d$windowRect <- windowRect
    }

    if (!is.null(fov <- persp$yfov))
      par3d$FOV <- fov*180/pi

    activeSubscene$par3d <<- par3d

    # We ignore znear and zfar
  }

  processOrthographic <- function(ortho) {
    # We ignore all the parameters here, and just set
    # up an orthographic FOV

    activeSubscene$par3d$FOV <<- 0

  }

  processCamera <- function(cam) {
    camera <- x$cameras[[cam + 1]]
    class(camera) <- "gltfCamera"

    if (camera$type == "orthographic")
      processOrthographic(camera$orthographic)
    else if (camera$type == "perspective")
      processPerspective(camera$perspective)
  }

  # Special nodes have extras$RGL_obj containing
  # the rglobject value.  They may also have
  # a mesh and other attributes like a normal mode;
  # rgl2gltf ignores those, but other software (e.g. blender)
  # could use it to approximate the special.

  restoreRGLclass <- function(obj) {
    if (!is.null(obj)) {
      class(obj) <- c(obj$class1, obj$class2)
      obj$class1 <- obj$class2 <- NULL
    }
    obj
  }

  processSpecial <- function(node, parentTransform) {
    newobj <- restoreRGLclass(node$extras$RGL_obj)
    newobj$id <- getId(newobj$id)
    activeSubscene$objects <<- c(activeSubscene$objects, newobj$id)
    objects <- c(rglscene$objects, list(newobj))
    names(objects)[length(objects)] <- newobj$id
    rglscene$objects <<- objects
  }

  processSubscene <- function(node, parentTransform) {
    newobj <- restoreRGLclass(node$extras$RGL_obj)
    newobj$id <- getId(newobj$id)
    newobj$objects <- c()
    activeSubscene <<- newobj
  }

  processNode <- function(n, parentTransform) {
    node <- x$nodes[[n + 1]]
    class(node) <- "gltfNode"

    if (!is.null(node$camera))
      processCamera(node$camera)

    transform <- getTransform(node, parentTransform)

    children <- unlist(node$children)

    isSubscene <- FALSE
    isSpheres <- FALSE
    if (n %in% convertNodes) {
      isSpecial <- useRGLinfo &&
                   !is.null(node$extras) &&
                   !is.null(obj <- node$extras$RGL_obj)
      if (isSpecial) {
        isSubscene <- isRGL(obj, "subscene")
        isSpheres <- isRGL(obj, "spheres")
      }
      if (isSubscene) {
        saveActive <- activeSubscene
        processSubscene(node, parentTransform)
        transform <- diag(4)
      } else if (isSpecial) {
        processSpecial(node, parentTransform)
      } else {
        if (is.null(activeSubscene))
          activeSubscene <<- newSubscene()
        if (!is.null(m <- node$mesh))
          processMesh(m, transform)
      }

      convertNodes <<- union(convertNodes, children)
    }

    if (isSpheres)
      children <- NULL

    for (child in children)
      processNode(child, transform)

    if (isSubscene)
      activeSubscene <- applyidTranslations(activeSubscene)

    if (isSubscene && !is.null(saveActive)) {
      subscenes <- c(saveActive$subscenes, list(activeSubscene))
      names(subscenes)[length(subscenes)] <- activeSubscene$id
      saveActive$subscenes <- subscenes
      activeSubscene <<- saveActive
    }
  }

  if (length(list(...)))
    warning("These arguments ignored: ", paste(names(list(...)), collapse = ", "))

  if (is.null(scene))
    scene <- 0

  if (is.null(convertNodes <- nodes))
    convertNodes <- seq_along(x$nodes) - 1

  sc <- x$scenes[[scene + 1]]
  if (is.null(sc))
    return()

  on.exit(closeBuffers(x))

  lastid <- 0
  rglscene <- list()

  activeSubscene <- NULL

  defaultmaterial <- list()

  nodes <- sc$nodes

  if (length(nodes) > 1)
    activeSubscene <- newSubscene()

  idTranslations <- NULL  # translations of id values

  for (n in nodes)
    processNode(n, parentTransform = diag(4))

  rglscene$rootSubscene <- applyidTranslations(activeSubscene)

  structure(rglscene, class = "rglscene")
}
