getPrim <- function(gltf, tag) {
  meshnum <- as.numeric(sub(":.*$", "", tag))
  mesh <- gltf$getMesh(meshnum)
  primnum <- as.numeric(sub("^.*:", "", tag))
  prim <- mesh$primitives[[primnum]]
}

getAffectedObjects <- function(gltf, method) {
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
          jindex0 <- unique(jindex[weights > 0])
          if (method == "rigid") {
            wt <- 0*jindex
            for (k in seq_along(jindex0)) {
              wt[k] <- wt[k] + sum(weights[jindex == jindex0[k]])
            }
            prim$usejoint <- jindex0[which.max(wt)]
            mesh$primitives[[j]] <- prim
            jindex0 <- jindex0[prim$usejoint]
          }
          for (ji in jindex0) {
            joint <- joints[ji + 1]
            result[joint+1] <- list(c(result[[joint+1]], tag))
          }
        }
      }
      gltf$setMesh(meshnum, mesh)
    }
  }
  # If an object is affected by a node, it is affected by
  # the parent nodes too.
  addChildren <- function(n) {
    node <- gltf$getNode(n)
    for (child in node$children) {
      addChildren(child)
      result[n+1] <<- list(unique(c(result[[n+1]], result[[child+1]])))
    }
  }

  for (i in seq_len(nnodes)) {
    addChildren(i-1)
  }
  result
}

playgltf <- function(gltf, animation = 0, start = times[1],
                     stop = times[2], times = gltf$timerange(animation),
                     method = c("wholeScene", "partialScene", "rigid"), ...) {
  
  getMatrices <- function(n) {
    if (n != havenode) {
      havenode <<- n
      forward <<- NULL
      backward <<- NULL
      node <- gltf$getNode(n)
      if (!is.null(node$skin)) {
        skin <- gltf$getSkin(node$skin)
        forward <<- skin$forward
        backward <<- gltf$getInverseBindMatrices(skin)
      }
    }
  }
  
  if (animation + 1 > gltf$listCount("animations"))
    stop("Animation not found")

  method <- match.arg(method)
  time <- start
  gltf <- gltf$clone()
  s <- as.rglscene(gltf, time = time, clone = FALSE)
  res <- plot3d(s, ...)

  save <- par3d(skipRedraw = TRUE)
  on.exit(par3d(save))

  if (method %in% c("partialScene", "rigid")) {
    # Make a list indexed by the node number (+1) of the
    # objects that it affects.  When it changes, those
    # objects will be deleted and then redrawn in the
    # partialScene method.  In the rigid method, only
    # the node with the highest total weight will count.
    affectedObjects <- getAffectedObjects(gltf, method)
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

  if (method == "rigid") { 
      # Put each primitive into a separate subscene
      # under its original one.  Set the userMatrix
      # for that subscene to match the majority transformation
      # for its coordinates.
    wrappers <- containingNodes
    havenode <- -1
    for (tag in names(containingNodes)) {
      # if (tag == "0:43")
      #   browser()
      nodes <- containingNodes[[tag]]
      for (i in seq_along(nodes)) {
        subname <- paste0("subscene", nodes[i])
        nodeid <- res[subname]
        wrappers[[tag]][i] <- subid <- newSubscene3d(model="modify",
                                                     viewport = "inherit",
                                                     projection = "inherit",
                                                     parent = nodeid)
        id <- tagged3d(tag, subscene = nodeid)
        getMatrices(nodes[i])
        node <- gltf$getNode(nodes[i])
        if (!is.null(forward)) {
          prim <- getPrim(gltf, tag)
          par3d(userMatrix = forward[,,prim$usejoint + 1] %*% backward[,,prim$usejoint + 1],
                listeners = nodeid,
                subscene = subid)
          pop3d(id = id)
          # cat("tag =", tag, " bbox=", par3d("bbox", subscene=nodeid)[1:2], "\n")
          newobj <- primToRglobj(prim, node$skin,
                             gltf = gltf,
                             defaultmaterial = s$material)
          newobj$material$tag <- tag
          useSubscene3d(subid)
          plot3d(newobj, add=TRUE)
        }
      }
    }
  }

  clockstart <- Sys.time()
  duration <- as.difftime(stop - start, units = "secs")

  repeat {
    time <- Sys.time() - clockstart
    if (time > duration)
      break
    time <- as.numeric(time, units = "secs") + start
    changedNodes <- gltf$settime(time, animation)
    havenode <- -1
    if (length(changedNodes)) {
      if (method == "wholeScene") {
        rgl::clear3d()
        plot3d(gltf, time = time, clone = FALSE, add = TRUE)

      } else if (method %in% c("partialScene", "rigid")) {
        objs <- NULL
        for (n in changedNodes) {
          subname <- paste0("subscene", n)
          par3d(userMatrix = gltf$getTransform(n),
                subscene = res[subname])
          objs <- unique(c(objs, affectedObjects[[n + 1]]))
        }
        for (o in objs) {
          prim <- getPrim(gltf, o)
          # if (o == "0:22")
          #   browser()
          if (method == "partialScene") {
            pop3d(tag = o)
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
          } else if (method == "rigid") {
            getMatrices(containingNodes[[o]][1])
            wrapper <- wrappers[[o]]
            for (i in seq_along(wrapper)) {
              par3d(userMatrix = forward[,,prim$usejoint + 1] %*% backward[,,prim$usejoint + 1], subscene = wrapper[i])
            }
          }
        }
      }
      par3d(save)
      par3d(skipRedraw = TRUE)
    }
    Sys.sleep(0.01)
  }
}
