getAffectedObjects <- function(gltf) {
  nnodes <- gltf$listCount("nodes")
  result <- vector("list", nnodes)
  for (i in seq_len(nnodes)) {
    node <- gltf$getNode(i-1)
    if (!is.null(node$skin) && !is.null(node$mesh)) {
      skin <- gltf$getSkin(node$skin)
      joints <- unlist(skin$joints)
      meshnum <- node$mesh
      mesh <- gltf$getMesh(meshnum)
      for (j in seq_along(mesh$primitives)) {
        prim <- mesh$primitives[[j]]
        for (a in seq_along(prim$attributes)) {
          attr <- unlist(prim$attributes[a])
          if (names(attr) %in% c("JOINTS_0", "WEIGHTS_0")) {
            values <- gltf$readAccessor(attr[1])
            switch (names(attr),
                    JOINTS_0 = jindex <- values,
                    WEIGHTS_0 = weights <- values
            )
          }
        }
        if (length(weights)) {
          tag <- paste0(meshnum, ":", j)
          jindex <- unique(jindex[weights > 0])
          for (ji in jindex) {
            joint <- joints[ji + 1]
            result[[joint+1]] <- c(result[[joint+1]], tag)
          }
        }
      }
    }
  }
  # If an object is affected by a node, it is affected by
  # the parent nodes too.
  addChildren <- function(n) {
    node <- gltf$getNode(n)
    for (child in node$children) {
      addChildren(child)
      result[[n+1]] <<- unique(c(result[[n+1]], result[[child+1]]))
    }
  }

  for (i in seq_len(nnodes)) {
    addChildren(i-1)
  }
  result
}

playgltf <- function(gltf, animation = 0, start = times[1],
                     stop = times[2], times = gltf$timerange(animation), method = c("partialScene", "wholeScene"), ...) {

  if (animation + 1 > gltf$listCount("animations"))
    stop("Animation not found")

  method <- match.arg(method)
  time <- start
  gltf <- gltf$clone()
  s <- as.rglscene(gltf, time = time, clone = FALSE)
  res <- plot3d(s, ...)

  save <- par3d(skipRedraw = TRUE)
  on.exit(par3d(save))

  if (method == "partialScene") {
    # Make a list indexed by the node number (+1) of the
    # objects that it affects.  When it changes, those
    # objects will be deleted and then redrawn.
    affectedObjects <- getAffectedObjects(gltf)
    allNames <- unique(unlist(affectedObjects))
    containingNodes <- vector("list", length(allNames))
    names(containingNodes) <- allNames
    recurse <- function(sub) {
      for (o in sub$objects) {
        obj <- s$objects[[as.character(o)]]
        tag <- obj$material$tag
        if (!is.null(tag))
          containingNodes[[tag]] <<- c(containingNodes[[tag]], sub$id)
      }
      for (s in sub$subscenes)
        recurse(s)
    }
    recurse(s$rootSubscene)
  }

  clockstart <- Sys.time()
  duration <- as.difftime(stop - start, units = "secs")

  repeat {
    time <- Sys.time() - clockstart
    if (time > duration)
      break
    time <- as.numeric(time, units = "secs") + start
    changedNodes <- gltf$settime(time, animation)
    if (length(changedNodes)) {
      if (method == "wholeScene") {
        rgl::clear3d()
        plot3d(gltf, time = time, clone = FALSE, add = TRUE)
      } else if (method == "partialScene") {
        objs <- NULL
        for (n in changedNodes) {
          subname <- paste0("subscene", n)
          par3d(userMatrix = gltf$getTransform(n),
                subscene = res[subname])
          objs <- unique(c(objs, affectedObjects[[n + 1]]))
        }
        for (o in objs) {
          pop3d(tag = o)
          meshnum <- as.numeric(sub(":.*$", "", o))
          mesh <- gltf$getMesh(meshnum)
          primnum <- as.numeric(sub("^.*:", "", o))
          prim <- mesh$primitives[[primnum]]
          for (n1 in containingNodes[[o]]) {
            node <- gltf$getNode(n1)
            skinnum <- node$skin
            newobj <- primToRglobj(prim,
                                   skinnum = skinnum,
                                   gltf,
                                   defaultmaterial = s$material)
            newobj$material$tag <- o
            subname1 <- paste0("subscene", n1)
            useSubscene3d(res[subname1])
            id <- plot3d(newobj, add=TRUE)
          }
        }
      }
      par3d(save)
      par3d(skipRedraw = TRUE)
    }
    Sys.sleep(0.01)
  }
}
