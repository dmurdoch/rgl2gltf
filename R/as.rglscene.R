
as.rglscene.gltf <- function(x, scene = x$scene, nodes = NULL,
                             useRGLinfo = TRUE,
                             time = NULL,
                             ani = 0, ...) {

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
                             par3d = list(bbox = c(Inf, -Inf, Inf, -Inf, Inf, -Inf))),
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

  primToRglobj <- function(prim, transform, skinnum) {
    class(prim) <- "gltfPrimitive"

    if (!is.null(prim$targets)) {
      print(prim)
      # What are prim targets????
      browser()
    }
    mat <- x$getRglMaterial(prim$material)

    normals <- positions <- texcoords <- joints <- weights <- NULL
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
              },
              JOINTS_0 = joints <- values,
              WEIGHTS_0 = weights <- values
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

    if (!is.null(joints) && !is.null(skinnum)) {
      skin <- x$getSkin(skinnum)
      jnt <- unique(as.numeric(joints))

      if (is.null(time)) {
        # if we're doing dynamic updates, we can
        # only work with one transformation per
        # rgl object, so we'll use the one with the
        # highest total weight.  But for
        # static updates (specified time) we can
        # modify the individual vertices as specified.

        wt <- 0*jnt
        for (j in seq_along(jnt)) {
          wt[j] <- sum(weights[joints == jnt[j]])
        }
        tag <- jnt[which.max(wt)]

        # For dynamic updates, we need to put this in
        # its own subscene
        wrapper <- newSubscene(activeSubscene)
        wrapper$par3d$userMatrix <- transform
        wrapper$par3d$listeners <- activeSubscene$id
        transformed <- asEuclidean(asHomogeneous(positions) %*% t(transform))
        ranges <- apply(transformed, 2, range)
        wrapper$par3d$bbox <- as.numeric(ranges)
        wrapper$embeddings["model"] <- "modify"
        transform <- diag(4)
        saveActive <- activeSubscene
        activeSubscene <<- wrapper
        on.exit(activeSubscene <<- saveActive)

        mat$tag <- paste(tag, wrapper$id)
      } else {
        # If time is non-NULL, we'll use the
        # animation values for the specified time
        # to modify the vertices

        # We compute transforms for all the different
        # combinations in this primitive.
        nj <- ncol(joints)
        if (ncol(weights) != nj)
          stop("joints and weights don't match")
        both <- cbind(joints, weights)
        bothfirst <- both[!duplicated(both),,drop=FALSE]
        matrices <- skin$matrices
        if (is.null(matrices)) {
          skin$matrices <- matrices <- x$getInverseBindMatrices(skin)
          x$setSkin(skinnum, skin)
        }
        for (i in seq_len(nrow(bothfirst))) {
          joint <- bothfirst[i, 1:nj]
          wt <- bothfirst[i, nj + 1:nj]
          wt <- wt/sum(wt)
          transform <- matrix(0, 4,4)
          for (j in seq_along(joint)) {
            if (wt[j] == 0) next
            n <- x$getJoint(skin, joint[j])
            transform <- transform + wt[j] * x$getTransform(n) %*% matrices[,,joint[j]+1]
          }
          sel <- apply(both, 1, function(row) all(row == bothfirst[i,]))
          positions[sel,] <- asEuclidean(asHomogeneous(positions[sel,,drop = FALSE]) %*% t(transform))

          if (!is.null(normals)) {
            nt <- transform
            nt[4,1:3] <- nt[1:3, 4] <- 0
            nt <- solve(nt)
            normals[sel,] <- normalize(asEuclidean(rotate3d(cbind(normals[sel,,drop=FALSE],1), matrix = nt)))
          }
        }
      }
    }
#
#     positions <- asEuclidean(asHomogeneous(positions) %*% t(transform))
    colnames(positions) <- c("x", "y", "z")

    # if (!is.null(normals)) {
    #   nt <- transform
    #   nt[4,1:3] <- nt[1:3, 4] <- 0
    #   nt <- solve(nt)
    #   normals <- normalize(asEuclidean(rotate3d(cbind(normals,1), matrix = nt)))
    # }

    if (is.null(mode <- prim$mode))
      mode <- 4
    ninds <- length(indices)

    result <- switch(as.character(mode),
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
    # if (!is.null(joints)) {
    #   wrapper <- insertObject(result, wrapper)
    #   result <- wrapper
    # }
    result
  }

  mergeRanges <- function(r1, r2) {
    i <- c(1, 3, 5)
    r1[i] <- pmin(r1[i], r2[i])
    i <- c(2, 4, 6)
    r1[i] <- pmax(r1[i], r2[i])
    r1
  }

  insertObject <- function(newobj, parent = activeSubscene) {
    if (newobj$type == "subscene") {
      subscenes <- c(parent$subscenes, list(newobj))
      names(subscenes)[length(subscenes)] <- newobj$id
      parent$subscenes <- subscenes
      newbbox <- newobj$par3d$bbox
    } else {
      parent$objects <- union(activeSubscene$objects, newobj$id)
      newbbox <- as.numeric(apply(newobj$vertices, 2, range))
      objects <- c(rglscene$objects, list(newobj))
      names(objects)[length(objects)] <- newobj$id
      rglscene$objects <<- objects
    }
    parent$par3d$bbox <- mergeRanges(parent$par3d$bbox, newbbox)
    parent
  }

  processPrimitive <- function(prim, transform, skin) {
    activeSubscene <<- insertObject(primToRglobj(prim, transform, skin))
  }

  processMesh <- function(m, transform, skin) {
    mesh <- x$getMesh(m)
    for (p in seq_along(mesh$primitives)) {
      processPrimitive(mesh$primitives[[p]], transform, skin)
    }
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

  processSprites <- function(n, parent) {
    node <- self$getNode(n)
    main <- restoreRGLclass(node$extras$RGL_obj)
    main$id <- getId(main$id)
    children <- node$children
    if (!is.null(children)) {
      firstborn <- x$getNode(children[[1]])
      children <- unlist(firstborn$children)
    }
    saveSubscene <- activeSubscene

    activeSubscene <<- main

    for (child in children)
      processNode(child, parent)

    main <- activeSubscene
    main$ids <- main$objects
    main$objects <- NULL
    rglscene$objects[[as.character(main$id)]] <<- main
    activeSubscene <<- saveSubscene
    activeSubscene$objects <<- c(activeSubscene$objects, main$id)
  }

  processSpecial <- function(n, parent) {
    node <- x$getNode(n)
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
    activeSubscene <<- insertObject(newobj)
  }

  processSubscene <- function(n) {
    node <- x$getNode(n)
    newobj <- restoreRGLclass(node$extras$RGL_obj)
    newobj$id <- getId(newobj$id)
    newobj$objects <- c()
    activeSubscene <<- newobj
  }

  processNode <- function(n, parent) {
    node <- x$getNode(n)
    if (!is.null(node$camera))
      processCamera(node$camera)

    skin <- node$skin

    transform <- x$getTransform(n, parent)

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
        processSubscene(n)
        transform <- diag(4)
      } else if (isSprites) {
        processSprites(n, parent)
      } else if (isSpecial) {
        processSpecial(n, parent)
      } else {
        if (is.null(activeSubscene))
          activeSubscene <<- newSubscene()
        if (!is.null(m <- node$mesh))
          processMesh(m, transform, skin)
      }

      convertNodes <<- union(convertNodes, children)
    }

    if (isSpheres)
      children <- NULL

    for (child in children)
      processNode(child, n)

    if (isSubscene)
      activeSubscene <<- applyidTranslations(activeSubscene)

    if (isSubscene && !is.null(saveActive))
      activeSubscene <<- insertObject(activeSubscene, saveActive)
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
    processNode(n, NULL)

  rglscene$rootSubscene <- applyidTranslations(activeSubscene)
  rglscene$material <- defaultmaterial

  structure(rglscene, class = "rglscene")
}
