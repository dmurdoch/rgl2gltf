
as.rglscene.gltf <- function(x, scene = x$scene, nodes = NULL,
                             useRGLinfo = TRUE, ...) {

  saveTranslation <- function(oldid, newid) {
    idTranslations <<- c(idTranslations, newid)
    names(idTranslations)[length(idTranslations)] <<- oldid
  }

  applyidTranslations <- function(sub) {
    if (!is.null(idTranslations))
      sub$par3d$listeners <- as.integer(idTranslations[as.character(sub$par3d$listeners)])
    sub
  }

  getId <- function(oldid) {
    lastid <<- lastid + 1L
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
      result$material <- matdiff(material, defaultmaterial)
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
                             type = "subscene",
                             par3d = list()),
              class = c("rglsubscene", "rglobject"))
    if (missing(parent)) { # i.e. root
      result$embeddings <- c(viewport = "replace",
                             projection = "replace",
                             model = "replace",
                             mouse = "replace")
      result$par3d$windowRect <- getDefaults("par3d", "windowRect",
          c(x = 0, y = 40, width = 256, height = 296))
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

  primToRglobj <- function(prim, transform) {
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
    else {
      indices <- x$readAccessor(prim$indices) + 1 # R indices start at 1
    }

    positions <- asEuclidean(asHomogeneous(positions) %*% t(transform))
    colnames(positions) <- c("x", "y", "z")

    if (!is.null(normals)) {
      nt <- transform
      nt[4,1:3] <- nt[1:3, 4] <- 0
      nt <- solve(nt)
      normals <- normalize(asEuclidean(rotate3d(cbind(normals,1), matrix = nt)))
    }

    if (is.null(mode <- prim$mode))
      mode <- 4
    ninds <- length(indices)

    switch(as.character(mode),
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
  }

  processPrimitive <- function(prim, transform) {

    newobj <- primToRglobj(prim, transform)
    activeSubscene$objects <<- union(activeSubscene$objects, newobj$id)
    objects <- c(rglscene$objects, list(newobj))
    names(objects)[length(objects)] <- newobj$id
    rglscene$objects <<- objects
  }

  processMesh <- function(m, transform) {
    mesh <- x$getMesh(m)

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
    camera <- x$getCamera(cam)

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

  processSprites <- function(node, parentTransform) {
    main <- restoreRGLclass(node$extras$RGL_obj)
    main$id <- getId(main$id)
    children <- node$children
    if (!is.null(children)) {
      firstborn <- x$getNode(children[[1]])
      children <- unlist(firstborn$children)
    }
    saveSubscene <- activeSubscene
    on.exit(activeSubscene <<- saveSubscene)

    activeSubscene <<- main

    for (child in children)
      processNode(child, parentTransform)

    main <- activeSubscene
    main$ids <- main$objects
    main$objects <- NULL
    rglscene$objects[[as.character(main$id)]] <<- main
    # activeSubscene will be restored by on.exit
  }

  processSpecial <- function(node, parentTransform) {
    primobj <- NULL
    m <- node$mesh
    if (!is.null(m)) {
      mesh <- x$getMesh(m)
      if (!is.null(mesh$primitives)) {
        primobj <- primToRglobj(mesh$primitives[[1]], parentTransform)
      }
    }
    newobj <- restoreRGLclass(node$extras$RGL_obj)
    newobj$id <- getId(newobj$id)
    if (!is.null(primobj)) {
      newobj <- merge(newobj, primobj)
      newobj$type <- primobj$type # quads may have changed to triangles
    }
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
    node <- x$getNode(n)

    if (!is.null(node$camera))
      processCamera(node$camera)

    transform <- x$getTransform(node, parentTransform)

    children <- unlist(node$children)

    isSubscene <- FALSE
    isSpheres <- FALSE
    isSprites <- FALSE

    if (n %in% convertNodes) {
      isSpecial <- useRGLinfo &&
                   !is.null(node$extras) &&
                   !is.null(obj <- node$extras$RGL_obj)
      if (isSpecial) {
        isSubscene <- isRGL(obj, "subscene")
        isSpheres <- isRGL(obj, "spheres")
        isSprites <- isRGL(obj, "sprites")
      }
      if (isSubscene) {
        saveActive <- activeSubscene
        processSubscene(node, parentTransform)
        transform <- diag(4)
      } else if (isSprites) {
        processSprites(node, parentTransform)
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
    convertNodes <- seq_len(x$listCount("nodes")) - 1

  sc <- x$getScene(scene)
  if (is.null(sc))
    return()

  lastid <- 0L
  rglscene <- list(material = NULL, rootSubscene = NULL)

  activeSubscene <- NULL

  defaultmaterial <- list()
  if (useRGLinfo &&
      !is.null(extras <- x$getExtras()) &&
      !is.null(extras$RGL_material))
    defaultmaterial <- extras$RGL_material

  nodes <- sc$nodes

  if (length(nodes) > 1)
    activeSubscene <- newSubscene()

  idTranslations <- NULL  # translations of id values

  for (n in nodes)
    processNode(n, parentTransform = diag(4))

  rglscene$rootSubscene <- applyidTranslations(activeSubscene)
  rglscene$material <- defaultmaterial

  structure(rglscene, class = "rglscene")
}
