animationControl <- function(gltf, animation = 0) {
  if (animation + 1 > gltf$listCount("animations"))
    stop("Animation not found")


}

getChangeTimes <- function(joint, gltf, ani) {
  animation <- gltf$getAnimation(ani)
  times <- numeric()
  for (i in seq_along(animation$channels)) {
    node <- animation$channels[[i]]$target$node
    if (node == joint) {
      input <- animation$samplers[[i]]$input
      times <- c(times, gltf$readAccessor(input))
    }
  }
  sort(unique(times))
}

# Start by modifying playgltf to get it to produce
# the rglwidget output we want, with extra info so that
# the animation control can control it

gltfwidget <- function(gltf, animation = 0, start = times[1],
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
    stop("Animation not found")

  if (verbose)
    cat("Initial plot...\n")

  method <- match.arg(method)
  time <- start
  gltf <- gltf$clone()
  s <- as.rglscene(gltf, time = time, clone = FALSE)

  saveopts <- options(rgl.useNULL = TRUE)
  on.exit(options(saveopts))

  res <- plot3d(s, add = TRUE, ...)

  save <- par3d(skipRedraw = TRUE)
  on.exit(par3d(save), add = TRUE)

  if (method != "rigid")
    stop("only rigid method is supported")

  if (verbose)
    cat("Preparing skeleton...\n")
  root <- currentSubscene3d()

  # Make a list indexed by the node number (+1) of the
  # objects that it affects.  When it changes, those
  # objects will be deleted and then redrawn in the
  # partialScene method.  In the rigid method, this
  # will be converted to subscenes that need modification
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
      skinnum <- node$skin
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

  havenode <- -1
  controls <- list()
  for (o in allNames) {
    cat("object ", o, "\n")
    getMatrices(containingNodes[[o]][1])
    prim <- getPrim(gltf, o)
    weights <- prim$unique_weights
    # The colnames are indices into the skin$joints vector
    # First, the index
    joints <- as.numeric(colnames(weights))
    # Second, the node
    skin <- gltf$getSkin(node$skin)
    jointnodes <- skin$joints[joints + 1]
    alltimes <- lapply(jointnodes, getChangeTimes, gltf, animation)
    for (i in seq_along(wrappers[[o]])) {
      keepjoints <- which(weights[i,] > 0)
      times <- sort(unique(unlist(alltimes[keepjoints])))
      nt <- length(times)
      if (nt) {
        transforms <- vector("list", length = nt)
        for (j in seq_len(nt)) {
          gltf$settime(times[j], animation)
          transforms[[j]] <- weightedTransform(joints, weights[i,],
                                         forward, backward)
        }
        fn <- par3dinterp(times, userMatrix = transforms,
                        subscene = wrappers[[o]][i])
        controls <- c(controls, list(par3dinterpControl(fn, from = start, to = stop, steps = 100)))
      }
    }
  }
  widget <- rglwidget(s)
  widget %>% playwidget(controls, start = start, stop = stop,                         interval = (stop - start)/100)
  widget
}
