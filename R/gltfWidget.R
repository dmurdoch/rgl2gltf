animationControl <- function(gltf, ani = 0, value = gltf$timerange(ani)[1], translations) {

  # translations should be a list or dataframe with
  # columns "subscene" and "node" giving the
  # correspondence

  if (ani + 1 > gltf$listCount("animations"))
    stop("Animation not found")

  animation <- gltf$getAnimation(ani)
  buffer <- Buffer$new()
  for (i in seq_along(animation$samplers)) {
    sampler <- animation$samplers[[i]]
    input <- gltf$readAccessor(sampler$input)
    sampler$input <- buffer$addAccessor(input)
    output <- gltf$readAccessor(sampler$output)
    sampler$output <- buffer$addAccessor(t(output))
    animation$samplers[[i]] <- sampler
  }
  for (i in seq_along(animation$channels)) {
    node <- animation$channels[[i]]$target$node
    subscene <- translations$subscene[match(node, translations$node)]
    animation$channels[[i]]$target$node <- subscene
  }
  buffer$closeBuffers()
  dependency <- makeDependency(name = "gltfAnimate",
                               src = "javascript",
                               script = "gltfAnimate.js",
                               package = "rgl2gltf",
                               debugging = TRUE
                               )
  structure(list(type = "rgl2gltfAnimation",
                 value = value,
                 animation = animation,
                 buffer = buffer$as.list(),
                 dependencies = list(dependency)),
            class = "rglControl")
}

weightedControl <- function(subid, nodes, weights, translations,
                        backtransform) {

  dependency <- makeDependency(name = "gltfAnimate",
                               src = "javascript",
                               script = "gltfAnimate.js",
                               package = "rgl2gltf",
                               debugging = TRUE
  )
  nodes <- translations$subscene[match(nodes, translations$node)]
  backtransforms <- list()
  for (i in seq_len(dim(backtransform)[3]))
    backtransforms[[i]] <- as.numeric(backtransform[,,i])
  structure(list(type = "rgl2gltfWeighted",
                 value = 0,
                 subid = unname(subid),
                 nodes = unname(nodes),
                 weights = unname(weights),
                 backtransform = backtransforms,
                 dependencies = list(dependency)),
            class = "rglControl")
}

skeletonControl <- function(subid) {

  dependency <- makeDependency(name = "gltfAnimate",
                               src = "javascript",
                               script = "gltfAnimate.js",
                               package = "rgl2gltf",
                               debugging = TRUE
  )

  structure(list(type = "rgl2gltfSkeleton",
                 value = 0,
                 subid = unname(subid),
                 dependencies = list(dependency)),
            class = "rglControl")
}

shaderControl <- function(id, joints, backtransform) {

  dependency <- makeDependency(name = "gltfAnimate",
                               src = "javascript",
                               script = "gltfAnimate.js",
                               package = "rgl2gltf",
                               debugging = TRUE
  )
  backtransforms <- list()
  for (i in seq_len(dim(backtransform)[3]))
    backtransforms[[i]] <- as.numeric(backtransform[,,i])
  structure(list(type = "rgl2gltfShaderUniforms",
                 value = 0,
                 id = unname(id),
                 joints = unname(joints),
                 backtransform = backtransforms,
                 dependencies = list(dependency)),
            class = "rglControl")
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

gltfWidget <- function(gltf, animation = 0, start = times[1],
                       stop = times[2], times = gltf$timerange(animation),
                       method = c("shader", "rigid"),
                       add = FALSE, close = !add,
                       verbose = FALSE,
                       open3dParams = getr3dDefaults(), ...) {

  backward <- NULL
  havenode <- -1

  getMatrices <- function(n) {
    if (n != havenode) {
      havenode <<- n
      backward <<- NULL
      node <- gltf$getNode(n)
      if (!is.null(node$skin)) {
        skin <- gltf$getSkin(node$skin)
        backward <<- gltf$getInverseBindMatrices(skin)
      }
    }
  }

  if (is.na(animation) || gltf$listCount("animations") == 0) {
    s <- as.rglscene(gltf)
    plot3d(s, useNULL = TRUE, add = add,
           silent = !verbose, open3dParams = open3dParams)
    if (close)
      on.exit(close3d())

    return(rglwidget())
  }

  if (animation + 1 > gltf$listCount("animations"))
    stop("Animation not found")

  if (verbose)
    cat("Initial plot...\n")

  method <- match.arg(method)
  time <- start

  gltf$closeBuffers()
  gltf <- gltf$clone()

  s <- as.rglscene(gltf, time = time, clone = FALSE)

  saveopts <- options(rgl.useNULL = TRUE)
  on.exit(options(saveopts))

  ids <- plot3d(s, useNULL = TRUE, add = add, silent = !verbose, open3dParams = open3dParams)

  subscene <- ids[grepl("subscene", names(ids))]
  node <- as.numeric(sub("subscene", "", names(subscene)))
  names(subscene) <- NULL
  translations <- data.frame(node, subscene)

  toNode <- function(sub) {
    translations$node[match(sub, translations$subscene)]
  }

  toSubscene <- function(node) {
    translations$subscene[match(node, translations$node)]
  }

  if (!(method %in% c("rigid", "shader")))
    stop("only rigid and shader methods are supported")

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

  # Start building the output.
  # First, create a control that will update all the
  # animated nodes.

  controls <- list(animationControl(gltf, ani = animation, value = start, translations = translations))

  # Break each primitive into one or more subscenes
  # under its original one.  Each one will hold a rigid
  # part of the primitive

  skeleton <- -1
  if (method == "shader")
    snew <- scene3d()
  for (tag in names(containingNodes)) {
    nodes <- containingNodes[[tag]]
    for (i in seq_along(nodes)) {
      subname <- paste0("subscene", nodes[i])
      nodeid <- ids[subname]

      id <- tagged3d(tag, subscene = nodeid)
      getMatrices(nodes[i])
      node <- gltf$getNode(nodes[i])
      skinnum <- node$skin
      skin <- gltf$getSkin(skinnum)
      if (is.null(skin$processed)) {
        if (is.null(skin$skeleton))
          skin$skeleton <- s$rootSubscene$id
        else
          skin$skeleton <- toSubscene(skin$skeleton)
        skin$processed <- TRUE
        gltf$setSkin(skinnum, skin)
      }
      if (skin$skeleton != skeleton) {
        skeleton <- skin$skeleton
        controls <- c(controls, list(skeletonControl(skeleton)))
      }
      prim <- getPrim(gltf, tag)
      if (method == "rigid") {
        subids <- numeric(length(prim$indices_split))
        weights <- prim$unique_weights
        joints <- as.numeric(colnames(weights))
        newobj <- primToRglobj(prim, node$skin,
                               gltf = gltf,
                               defaultmaterial = s$material,
                               doTransform = FALSE)
        tags <- rownames(weights)
        joints <- as.numeric(colnames(weights))
        # Second, the node
        skin <- gltf$getSkin(skinnum)
        jointnodes <- unlist(skin$joints[joints + 1])
        for (j in seq_along(subids)) {
          subids[j] <- newSubscene3d(model="modify",
                                     viewport = "inherit",
                                     projection = "inherit",
                                     parent = nodeid)

          newobj$indices <- prim$indices_split[[j]]
          newobj$material$tag <- tags[j]
          newid <- plot3d(cullVertices(newobj), add = TRUE)
          keepjoints <- which(weights[j,] > 0)
          controls <- c(controls, list(weightedControl(subids[j], jointnodes[keepjoints], weights[j, keepjoints], translations, backward[,,joints[keepjoints]+1, drop = FALSE])))
        }
        pop3d(id = id)

      } else {
        joints <- toSubscene(skin$joints)
        newobj <- primToRglobj(prim, node$skin,
                               gltf = gltf,
                               defaultmaterial = s$material,
                               doTransform = FALSE)
        snew$objects[[as.character(id)]] <- merge(snew$objects[[as.character(id)]], newobj)
        snew <- modifyShader(id, snew, joints = joints)
        controls <- c(controls, list(shaderControl(id, joints, backward)))
      }
    }
  }
  gltf$closeBuffers()
  if (method == "rigid")
    snew <- scene3d()

  widget <- rglwidget(snew)

  if (close)
    close3d()

  interval <- 1/20
  widget %>%
    playwidget(controls,
               start = start,
               stop = stop,
               interval = interval,
               step = interval,
               ...)
}

modifyShader <- function(id, scene, shader = getShaders(id, scene)$vertexShader, joints) {
  obj <- scene$objects[[as.character(id)]]
  shader <- unlist(strsplit(shader, "\n"))

  main <- grep("void main(void)", shader, fixed = TRUE)
  position <- grep("gl_Position = prMatrix * vPosition;", shader, fixed = TRUE)
  normal <- grep("vNormal = normMatrix * vec4(-aNorm, dot(aNorm, aPos));", shader, fixed = TRUE)
  stopifnot(length(main) == 1, length(position) == 1, length(normal) == 1)

  # We just keep the rotation part of the
  # skinMatrix.  If it also scales, this is wrong.
  newnormal <- c(
    "    skinMat = mat4(mat3(skinMat));",
    "    vNormal = normMatrix * skinMat * vec4(-aNorm, dot(aNorm, pos.xyz/pos.w));")
  shader <- append(shader[-normal], newnormal, after = normal - 1)

  swiz <- c("x", "y", "z", "w")
  n <- length(joints)
  newpos <- c("    mat4 skinMat = mat4(0);",
              "    for (int i = 0; i < 4; i++) {",
paste(sprintf("      skinMat[i] += aWeight.%s * uJointMat[4*int(aJoint.%s) + i];", swiz, swiz), collapse = "\n"),
              "    }",
              "    vec4 pos = skinMat * vec4(aPos, 1.);",
              "    gl_Position = prMatrix * mvMatrix * pos;")
  shader <- append(shader[-position], newpos, after = position - 1)

  newdecls <- sprintf("  attribute vec4 aJoint;
  attribute vec4 aWeight;
  uniform vec4 uJointMat[%d];", 4*length(joints))
  shader <- append(shader, newdecls, after = main - 1)

  setUserShaders(id, vertexShader = shader,
                 scene = scene,
                 attributes = list(aJoint = obj$joints, aWeight = obj$weights),
                 uniforms = list(uJointMat = matrix(0, 4*length(joints), 4)))

}
