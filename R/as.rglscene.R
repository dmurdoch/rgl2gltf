newObj <- function(xyz = NULL, material = NULL, normals = NULL,
                   texcoords = NULL,
                   type, attribs = NULL,
                   indices = NULL,
                   id = NULL) {

  result <- list(id=id, type=type)

  if (!(type %in% c("light", "clipplanes")))
    result$material <- material

  result$vertices <- xyz
  result$normals <- normals
  result$texcoords <- texcoords
  if (!is.null(indices))
    indices <- matrix(indices, ncol = 1, dimnames = list(NULL, "vertex"))
  result$indices <- indices

  result <- c(result, attribs)

  class(result) <- c(paste0("rgl", type), "rglobject")
  result
}

primToRglobj <- function(prim, skinnum, gltf, defaultmaterial = NULL, id = NULL, doTransform = TRUE) {
  class(prim) <- "gltfPrimitive"

  if (!is.null(prim$targets)) {
    print(prim)
    # What are prim targets????
    browser()
  }
  mat <- gltf$getRglMaterial(prim$material)

  normals <- positions <- texcoords <- joints <- weights <- NULL
  for (a in seq_along(prim$attributes)) {
    attr <- unlist(prim$attributes[a])
    values <- gltf$readAccessor(attr[1])
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
    indices <- gltf$readAccessor(prim$indices) + 1 # R indices start at 1
  }

  if (doTransform && !is.null(joints) && !is.null(skinnum)) {
    skin <- gltf$getSkin(skinnum)
    jnt <- unique(as.numeric(joints))

    # We compute transforms for all the different
    # combinations in this primitive.
    nj <- ncol(joints)
    if (ncol(weights) != nj)
      stop("joints and weights don't match")
    both <- cbind(joints, weights)
    bothfirst <- both[!duplicated(both),,drop=FALSE]
    backward <- gltf$getInverseBindMatrices(skin)
    forward <- skin$forward
    for (i in seq_len(nrow(bothfirst))) {
      joint <- bothfirst[i, 1:nj]
      wt <- bothfirst[i, nj + 1:nj]
      transform <- weightedTransform(joint, wt, forward, backward)
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
  colnames(positions) <- c("x", "y", "z")

  if (is.null(mode <- prim$mode))
    mode <- 4
  ninds <- length(indices)

  mat <- matdiff(mat, defaultmaterial)

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
                   "2" = newObj(xyz = positions,    # loop
                                normals = normals,
                                texcoords = texcoords,
                                material = mat,
                                indices = c(indices, indices[1]),
                                type = "linestrip"),
                   "3" = newObj(xyz = positions,    # strip
                                normals = normals,
                                texcoords = texcoords,
                                material = mat,
                                indices = indices,
                                type = "linestrip"),
                   "4" = newObj(xyz = positions,    # triangles
                                normals = normals,
                                texcoords = texcoords,
                                material = mat,
                                indices = indices,
                                type = "triangles"),
                   "5" = newObj(xyz = positions,    # triangle strip
                                normals = normals,
                                texcoords = texcoords,
                                indices = rbind(indices[-c(ninds, ninds-1)],
                                                indices[-c(1, ninds)],
                                                indices[-c(1,2)]),
                                material = mat),
                   "6" = newObj(xyz = positions,    # triangle fan
                                normals = normals,
                                texcoords = texcoords,
                                indices = rbind(indices[1],
                                                indices[-c(1, ninds)],
                                                indices[-c(1,2)]),
                                material = mat))
  if (!is.null(id))
    result$id <- as.numeric(id)
  result
}

as.rglscene.gltf <- function(x, scene = x$scene, nodes = NULL,
                             useRGLinfo = TRUE,
                             time = NULL,
                             ani = 0, clone = TRUE,
                             quick = FALSE, ...) {

  if (clone) {
    # We'll be caching various things, so make a
    # copy to avoid messing up the original.
    x$closeBuffers()  # Can't clone connections
    gltf <- x$clone()
    on.exit(gltf$closeBuffers())
  } else
    gltf <- x

  if (!is.null(time))
    gltf$settime(time)

  saveTranslation <- function(from, to) {
    if (from %in% idTranslations[, 1])
      stop("internal error saving from=", from, " to=", to)
    idTranslations[nrow(idTranslations) + 1,] <<- c(from, to)
  }

  applyidTranslations <- function(sub, translations = idTranslations) {
    if (!is.null(translations))
      sub$par3d$listeners <- with(translations,
                                  to[match(sub$par3d$listeners, from)])
    sub
  }

  applyAllidTranslations <- function(sub, translations = idTranslations) {
    sub <- applyidTranslations(sub, translations)
    sub$id <- with(translations, to[match(sub$id, from)])
    for (i in seq_along(sub$subscenes))
      sub$subscenes[[i]] <- applyAllidTranslations(sub$subscenes[[i]], translations)
    sub
  }

  setListeners <- function(sub, listeners) {
    if (is.null(sub$par3d$listeners))
      sub$par3d$listeners <- listeners
    else
      listeners <- sub$par3d$listeners
    for (i in seq_along(sub$subscenes))
      sub$subscenes[[i]] <- setListeners(sub$subscenes[[i]], listeners)
    sub
  }

  getId <- function(oldid = NULL) {
    lastid <<- lastid + 1L
    if (!is.null(oldid) && !(oldid %in% idTranslations$from))
      saveTranslation(oldid, lastid)
    as.numeric(lastid)
  }

  newSubscene <- function(id, root = FALSE) {
    result <- structure(list(id = as.numeric(id),
                             type = "subscene",
                             par3d = list(bbox = c(Inf, -Inf, Inf, -Inf, Inf, -Inf),
                                          userMatrix = diag(4))),
              class = c("rglsubscene", "rglobject"))
    if (root) { # i.e. root subscene
      result$embeddings <- c(viewport = "replace",
                             projection = "replace",
                             model = "replace",
                             mouse = "replace")
      result$par3d$userMatrix <- diag(4)
    } else {
      result$embeddings <- c(viewport = "inherit",
                             projection = "inherit",
                             model = "modify",
                             mouse = "inherit")
      result$par3d$userMatrix <- diag(4)
    }
    result$par3d$windowRect <- getDefaults("par3d", "windowRect",
                                           c(x = 0, y = 40, width = 512, height = 512))
    result$par3d$viewport <- getDefaults("par3d", "windowRect", result$par3d$windowRect - c(0, 40, 0, 0))
    result
  }

  insertObject <- function(newobj, parent) {
    if (is.null(newobj)) {
      return(parent)
    }
    if (newobj$type == "subscene") {
      subscenes <- c(parent$subscenes, list(newobj))
      # names(subscenes)[length(subscenes)] <- newobj$id
      parent$subscenes <- subscenes
      newbbox <- newobj$par3d$bbox
    } else {
      parent$objects <- as.numeric(union(parent$objects, newobj$id))
      newbbox <- getObjBBox(newobj)
      objects <- c(rglscene$objects, list(newobj))
      names(objects)[length(objects)] <- newobj$id
      rglscene$objects <<- objects
    }
    if (parent$type == "subscene") {
      if (all(is.finite(newbbox)))
        parent$par3d$bbox <- mergeBBox(parent$par3d$bbox, transformBBox(parent$par3d$userMatrix, newbbox))
      else
        parent$par3d$bbox <- mergeBBox(parent$par3d$bbox, newbbox)
    }
    parent
  }

  insertObjects <- function(objs, parent) {
    for (o in objs)
      parent <- insertObject(o, parent)
    parent
  }

  processPrimitive <- function(prim, skin) {
    primToRglobj(prim, skin, gltf, defaultmaterial,
                 id = getId())
  }

  processMesh <- function(m, skin) {
    mesh <- gltf$getMesh(m)
    result <- vector("list", length(mesh$primitives))
    for (p in seq_along(mesh$primitives)) {
      tag <- paste(m, p, sep = ":")
      prim <- mesh$primitives[[p]]
      result[[p]] <- processPrimitive(prim, skin)
      result[[p]]$material$tag <- tag
    }
    result
  }

  processPerspective <- function(persp, sub) {
    par3d <- sub$par3d
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

    sub$par3d <- par3d

    sub
    # We ignore znear and zfar
  }

  processOrthographic <- function(ortho, sub) {
    # We ignore all the parameters here, and just set
    # up an orthographic FOV

    sub$par3d$FOV <- 0
    sub

  }

  processCamera <- function(cam, sub) {
    camera <- gltf$getCamera(cam)

    if (camera$type == "orthographic")
      sub <- processOrthographic(camera$orthographic, sub)
    else if (camera$type == "perspective")
      sub <- processPerspective(camera$perspective, sub)
    sub
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

  processSprites <- function(n) {
    node <- gltf$getNode(n)
    main <- restoreRGLclass(node$extras$RGL_obj)
    main$id <- getId(main$id)
    children <- node$children
    if (!is.null(children)) {
      firstborn <- gltf$getNode(children[[1]])
      children <- unlist(firstborn$children)
    }
    objects <- list()
    for (child in children) {
      objects <- c(objects, list(processNode(child)))
      main <- insertObject(objects[[length(objects)]], main)
    }
    main$ids <- main$objects
    main$objects <- objects

    main
  }

  processSpecial <- function(n) {
    node <- gltf$getNode(n)
    primobj <- NULL
    m <- node$mesh
    if (!is.null(m)) {
      mesh <- gltf$getMesh(m)
      if (!is.null(mesh$primitives)) {
        primobj <- primToRglobj(mesh$primitives[[1]], gltf$getTransform(n), gltf, defaultmaterial)
      }
    }
    newobj <- restoreRGLclass(node$extras$RGL_obj)
    newobj$id <- getId(newobj$id)
    if (!is.null(primobj) && newobj$type != "bboxdeco") {
      newobj <- merge(newobj, primobj)
      newobj$type <- primobj$type # quads may have changed to triangles
    }
    newobj
  }

  processSubscene <- function(n) {
    node <- gltf$getNode(n)
    newobj <- restoreRGLclass(node$extras$RGL_obj)
    newobj$id <- getId(newobj$id)
    newobj
  }

  processNode <- function(n, root = FALSE) {
    node <- gltf$getNode(n)

    result <- newSubscene(n, root = root)

    result$par3d$userMatrix = gltf$getTransform(n)

    skinnum <- node$skin
    if (!is.null(skinnum)) {
      skin <- gltf$getSkin(skinnum)
      if (is.null(skin$forward)) {
        skin$forward <- gltf$getForwardBindMatrices(skin)
        gltf$setSkin(skinnum, skin)
      }
    }

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
        result <- processSubscene(n)
        result$objects <- NULL # They'll be inserted below
      } else if (isSprites) {
        result <- processSprites(n)
      } else if (isSpecial) {
        result <- processSpecial(n)
      } else {
        if (!is.null(m <- node$mesh))
          result <- insertObjects(processMesh(m, skinnum), result)
      }

      convertNodes <<- union(convertNodes, children)
    }

    if (isSpheres)
      children <- NULL

    for (child in children) {
      result <- insertObject(processNode(child), result)
    }

    if (isSubscene)
      result <- applyidTranslations(result)

    if (!is.null(node$camera))
      result <- processCamera(node$camera, result)

    result
  }

  if (length(list(...)))
    warning("These arguments ignored: ", paste(names(list(...)), collapse = ", "))

  if (is.null(scene))
    scene <- 0

  if (is.null(convertNodes <- nodes))
    convertNodes <- seq_len(gltf$listCount("nodes")) - 1

  sc <- gltf$getScene(scene)
  if (is.null(sc))
    return()

  lastid <- gltf$listCount("nodes")

  rglscene <- list(material = NULL)

  defaultmaterial <- list()
  if (useRGLinfo &&
      !is.null(extras <- gltf$getExtras()) &&
      !is.null(extras$RGL_material))
    defaultmaterial <- extras$RGL_material

  nodes <- unlist(sc$nodes)

  idTranslations <- data.frame(from = numeric(), to = numeric())  # translations of id values

  if (length(nodes) > 1) {
    rootSubscene <- newSubscene(getId(), root = TRUE)
    for (n in nodes) {
      newobj <- processNode(n)
      if (!is.null(newobj))
        rootSubscene <- insertObject(newobj, rootSubscene)
    }
    rootSubscene <- applyidTranslations(rootSubscene)
  } else
    rootSubscene <- processNode(nodes, root = TRUE)

  rootSubscene <- setListeners(rootSubscene, rootSubscene$id)

  if (is.null(rootSubscene$par3d$scale)) {
    bbox <- rootSubscene$par3d$bbox
    scale <- max(bbox[c(2,4,6)] - bbox[c(1,3,5)])
    rootSubscene$par3d$scale <- c(2/scale, 2/scale, 2/scale)
  }
  rglscene$rootSubscene <- rootSubscene
  rglscene$material <- defaultmaterial

  class(rglscene) <- "rglscene"

  # The calculations above are sufficient
  # for plot3d(), but not sufficient for
  # rglwidget().  If quick is TRUE, leave it
  # at that.
  if (!quick) {
    plot3d(rglscene, useNULL = TRUE)
    newscene <- scene3d()
    close3d()
    oldids <- getSubsceneIds(rglscene$rootSubscene)
    newids <- getSubsceneIds(newscene$rootSubscene)
    newscene$rootSubscene <- applyAllidTranslations(newscene$rootSubscene,
                                                    data.frame(from = newids, to = oldids))
    rglscene <- newscene
  }

  rglscene
}

getSubsceneIds <- function(sub) {
  result <- c(sub$id,
                 if (!is.null(sub$subscenes))
                   unlist(lapply(sub$subscenes, getSubsceneIds)))
}
