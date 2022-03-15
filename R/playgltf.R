getPrim <- function(gltf, tag) {
  meshnum <- as.numeric(sub(":.*$", "", tag))
  mesh <- gltf$getMesh(meshnum)
  primnum <- as.numeric(sub("^.*:", "", tag))
  prim <- mesh$primitives[[primnum]]
}

getAffectedObjects <- function(gltf, method) {
  nnodes <- gltf$listCount("nodes")
  result <- vector("list", nnodes)
  if (method != "shader") {
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
            if (method != "shader") {
              tag <- paste0(meshnum, ":", j)
              jindex_vals <- unique(as.numeric(jindex[weights > 0]))

              for (ji in jindex_vals) {
                joint <- joints[ji + 1]
                result[[joint + 1]] <- c(result[[joint + 1]], tag)
              }
              if (method == "rigid") {
                if (is.null(prim$indices))
                  indices <- seq_len(nrow(weights))
                else
                  indices <- gltf$readAccessor(prim$indices) + 1 # R indices start at 1
                if (is.null(mode <- prim$mode))
                  mode <- 4
                primsize <- c("0" = 1, "1" = 2, "4" = 3)[as.character(mode)]
                if (is.na(primsize))
                  base::stop("primitive mode ", mode, " not implemented in rigid method.")

                # weights_expanded <- weights[indices,]
                jindex_expanded <- jindex[indices,]

                # Map jindex values to entries in jindex_vals
                map <- seq_len(max(jindex_vals) + 1)
                map[jindex_vals + 1] <- seq_along(jindex_vals)

                weights_expanded <- matrix(0, ncol = length(jindex_vals), nrow = length(indices))
                for (col in 1:4) {
                  idx <- cbind(seq_along(indices), map[jindex_expanded[, col] + 1])
                  weights_expanded[idx] <- weights_expanded[idx] + weights[indices, col, drop = FALSE]
                }
                if (primsize > 1) {
                  for (gp in seq_len(length(indices)/primsize) - 1) {
                    idx <- primsize * gp + 1:primsize
                    weights_expanded[idx,] <-
                      rep(colMeans(weights_expanded[idx,,drop = FALSE]), each = primsize)
                  }
                }
                unique_weights <- unique(weights_expanded)
                colnames(unique_weights) <- as.character(jindex_vals)
                rownames(unique_weights) <- paste0(tag, ":", 1:nrow(unique_weights))
                indices_split <- list()
                if ((nw <- nrow(unique_weights)) == 1)
                  indices_split <- list(indices)
                else {
                  indices_split <- vector("list", nw)
                  for (gp in 1:nw)
                    indices_split[[gp]] <- indices[apply(weights_expanded, 1, function(row) all(row == unique_weights[gp,]))]
                }
                prim$unique_weights <- unique_weights
                prim$indices_split <- indices_split
                mesh$primitives[[j]] <- prim
              }
            }
          }
        }
        gltf$setMesh(meshnum, mesh)
      }
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

# Remove vertices that are not referenced
cullVertices <- function(obj) {
  if (!is.null(indices <- obj$indices)) {
    used <- unique(indices)
    obj$vertices <- obj$vertices[used,]
    if (!is.null(obj$normals))
      obj$normals <- obj$normals[used,]
    if (!is.null(obj$texcoords))
      obj$texcoords <- obj$texcoords[used,]
    if (!is.null(obj$colors))
      obj$colors <- obj$colors[used,]
    if (length(color <- obj$material$color) > 1)
      obj$material$color <- rep(color, length = length(indices))[used]
    obj$indices <- match(indices, used)
  }
  obj
}

getContainingNodes <- function(s, affectedObjects) {

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
  containingNodes
}

playgltf <- function(gltf, animation = 0, start = times[1],
                     stop = times[2], times = gltf$timerange(animation),
                     method = c("rigid", "wholeScene", "partialScene"),
                     speed = 1, by = NULL, verbose = FALSE, ...) {

  forward <- NULL
  backward <- NULL

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
    base::stop("Animation not found")

  if (verbose)
    cat("Initial plot...\n")

  method <- match.arg(method)
  time <- start
  gltf$closeBuffers()
  gltf <- gltf$clone()
  on.exit(gltf$closeBuffers())

  s <- as.rglscene(gltf, time = time, clone = FALSE)
  res <- plot3d(s, ...)

  save <- par3d(skipRedraw = TRUE)
  on.exit(par3d(save), add = TRUE)

  if (method %in% c("partialScene", "rigid")) {
    if (verbose)
      cat("Preparing skeleton...\n")
    root <- currentSubscene3d()

    # Make a list indexed by the node number (+1) of the
    # objects that it affects.  When it changes, those
    # objects will be deleted and then redrawn in the
    # partialScene method.  In the rigid method, this
    # will be converted to subscenes that need modification

    affectedObjects <- getAffectedObjects(gltf, method)

    containingNodes <- getContainingNodes(s, affectedObjects)
  }

  if (method == "rigid") {
      # Break each primitive into one or more subscenes
      # under its original one.  Set the userMatrix
      # for each subscene to match the average transformation
      # for its coordinates.
    wrappers <- vector("list", length(containingNodes))
    names(wrappers) <- allNames <- names(containingNodes)
    havenode <- -1
    for (tag in allNames) {
      nodes <- containingNodes[[tag]]
      for (i in seq_along(nodes)) {
        subname <- paste0("subscene", nodes[i])
        nodeid <- res[subname]

        id <- tagged3d(tag, subscene = nodeid)
        getMatrices(nodes[i])
        node <- gltf$getNode(nodes[i])
        if (!is.null(forward)) {
          prim <- getPrim(gltf, tag)
          subids <- numeric(length(prim$indices_split))
          weights <- prim$unique_weights
          joints <- as.numeric(colnames(weights))
          newobj <- primToRglobj(prim, node$skin,
                                 gltf = gltf,
                                 defaultmaterial = s$material,
                                 doTransform = FALSE)
          tags <- rownames(weights)
          for (j in seq_along(subids)) {
            subids[j] <- newSubscene3d(model="modify",
                                       viewport = "inherit",
                                       projection = "inherit",
                                       parent = nodeid)
            transform <- weightedTransform(joints, weights[j,],
                                           forward, backward)
            useSubscene3d(subids[j])
            par3d(userMatrix = transform,
                listeners = root)
            newobj$indices <- prim$indices_split[[j]]
            newobj$material$tag <- tags[j]
            newid <- plot3d(cullVertices(newobj), add = TRUE)
          }
          pop3d(id = id)
        }
      }
      wrappers[[tag]] <- subids
    }
  }

  clockstart <- Sys.time()
  duration <- as.difftime(stop - start, units = "secs")
  time <- start

  repeat {
    if (is.null(by))
      time <- (Sys.time() - clockstart)*speed
    else
      time <- time + by

    if (time > duration)
      break

    time <- as.numeric(time, units = "secs") + start

    if (verbose)
      cat("time = ", signif(time, 2), "\n")

    changedNodes <- gltf$settime(time, animation)
    havenode <- -1
    if (length(changedNodes)) {
      if (method == "wholeScene") {
        clear3d(type = "all")
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
          weights <- prim$unique_weights
          joints <- as.numeric(colnames(weights))
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
            subids <- wrappers[[o]]
            for (i in seq_along(subids)) {
              transform <- weightedTransform(joints, weights[i,], forward, backward)
              par3d(userMatrix = transform, subscene = subids[i])
            }
          }
        }
      }
      if (method %in% c("partialScene", "rigid"))
        useSubscene3d(root)
      par3d(save)
      par3d(skipRedraw = TRUE)
    }
    Sys.sleep(0.01)
  }
}
