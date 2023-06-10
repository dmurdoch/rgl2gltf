animationDependency <- function() makeDependency(name = "gltfAnimate",
                                                     src = "javascript/gltfAnimate",
                                                     script = "gltfAnimate.js",
                                                     package = "rgl2gltf",
                                                     debugging = TRUE
)

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

  structure(list(type = "rgl2gltfAnimation",
                 value = value,
                 animation = animation,
                 buffer = buffer$as.list(),
                 dependencies = list(animationDependency())),
            class = "rglControl")
}

weightedControl <- function(subid, nodes, weights, translations,
                        backtransform) {

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
                 dependencies = list(animationDependency())),
            class = "rglControl")
}

skeletonControl <- function(subid) {

  structure(list(type = "rgl2gltfSkeleton",
                 value = 0,
                 subid = unname(subid),
                 dependencies = list(animationDependency())),
            class = "rglControl")
}

shaderControl <- function(id, joints, usedjoints, backtransform) {

  backtransforms <- list()
  keep <- usedjoints + 1
  for (i in seq_along(usedjoints))
    backtransforms[[i]] <- as.numeric(backtransform[,,keep[i]])
  structure(list(type = "rgl2gltfShaderUniforms",
                 value = 0,
                 id = unname(id),
                 joints = unname(joints[keep]),
                 backtransform = backtransforms,
                 dependencies = list(animationDependency())),
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

hasPBRparams <- function(gltf) {
  mat <- gltf$getMaterial(0)
  length(mat$pbrMetallicRoughness[c("metallicFactor",
                                    "roughnessFactor",
                                    "metallicRoughnessTexture")]) > 0
}

# Start by modifying playgltf to get it to produce
# the rglwidget output we want, with extra info so that
# the animation control can control it

gltfWidget <- function(gltf, animation = 0, start = times[1],
                       stop = times[2], times = gltf$timerange(animation),
                       method = c("shader", "rigid"),
                       add = FALSE, close = !add,
                       verbose = FALSE,
                       open3dParams = getr3dDefaults(),
                       usePBR = hasPBRparams(gltf),
                       PBRargs = list(), ...) {

  if (!requireNamespace("manipulateWidget", quietly = TRUE))
    stop("gltfWidget requires the manipulateWidget package")

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

  has_animations <- !is.na(animation) && gltf$listCount("animations") != 0

  if (!usePBR && !has_animations) {
    s <- as.rglscene(gltf)
    plot3d(s, useNULL = TRUE, add = add,
           silent = !verbose, open3dParams = open3dParams)
    if (close)
      on.exit(close3d())

    return(rglwidget())
  }

  if (has_animations && animation + 1 > gltf$listCount("animations"))
    stop("Animation not found")

  if (verbose)
    cat("Initial plot...\n")

  method <- match.arg(method)

  if (usePBR && method != "shader" && verbose)
    message("Note:  Physically based rendering (PBR) is only used with method = 'shader'")

  if (method != "shader")
    usePBR <- FALSE

  gltf$closeBuffers()
  gltf <- gltf$clone()
  on.exit(gltf$closeBuffers())

  if (has_animations) {
    time <- start
    s <- as.rglscene(gltf, time = time, clone = FALSE, add = add)
  } else
    s <- as.rglscene(gltf, clone = FALSE, add = add)

  saveopts <- options(rgl.useNULL = TRUE)
  on.exit(options(saveopts), add = TRUE)

  ids <- plot3d(s, useNULL = TRUE, add = FALSE, silent = !verbose, open3dParams = open3dParams)

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

  if (has_animations) {
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
    if (method == "shader") {
      snew <- scene3d()
    }

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
                                 id = id,
                                 doTransform = FALSE)
          newobj <- merge(newobj, snew$objects[[as.character(id)]])
          snew$objects[[as.character(id)]] <- newobj

          # The skeleton probably has far more joints
          # than we need.  Reduce to the minimum.

          obj <- snew$objects[[as.character(id)]]
          weights0 <- as.numeric(obj$weights)
          joints0 <- as.numeric(obj$joints)
          usedjoints <- unique(c(0, joints0[weights0 > 0])) # Always keep joint 0
          # if (obj$material$tag == "0:50")
          # browser()
          obj$joints <- match(obj$joints, usedjoints) - 1
          dim(obj$joints) <- dim(obj$weights)
          snew$objects[[as.character(id)]] <- obj
          n <- length(usedjoints)
          if (usePBR) {
            PBR <- list()
            PBR$defines <- list(HAS_JOINTS = 1,
                                JOINTMATROWS = 4*n)
            PBR$attributes <- list(aJoint = obj$joints, aWeight = obj$weights)
            PBR$uniforms <- list(uJointMat = matrix(0, 4*n, 4))
            obj$PBR <- PBR
            snew$objects[[as.character(id)]] <- obj
          } else {
            shaders <- getShaders(id, snew)
            shaders <- modifyShaders(shaders, "skins",
                                   swiz = c("x", "y", "z", "w"), n = n)
            snew <- setUserShaders(id, scene = snew,
                                 vertexShader = shaders$vertexShader,
                                 attributes = c(list(aJoint = obj$joints, aWeight = obj$weights),
                                                obj$userAttributes),
                                 uniforms = c(list(uJointMat = matrix(0, 4*n, 4)),
                                              obj$userUniforms),
                                 textures = obj$userTextures)
          }
          controls <- c(controls, list(shaderControl(id, joints, usedjoints, backward)))
        }
      }
    }

    gltf$closeBuffers()
    if (method == "rigid")
      snew <- scene3d()
  } else
    snew <- s

  if (usePBR) {
    for (i in seq_along(snew$objects)) {
      obj <- snew$objects[[i]]
      if (!is.null(material <- obj$material) &&
          !is.null(tag <- material$tag) &&
          !is.null(prim <- getPrim(gltf, tag))) {
        obj$material <- mergeMaterial(obj$material, snew$material)
        snew <- do.call("setPBRshaders",
                        c(list(gltf,
                               gltfMat = gltf$getMaterial(prim$material),
                               obj$id,
                               scene = snew),
                          obj$PBR, PBRargs))
      }
    }
  }

  if (close)
    close3d()

  widget <- rglwidget(snew)

  if (has_animations) {
    interval <- 1/20
    widget %>%
      playwidget(controls,
                 start = start,
                 stop = stop,
                 interval = interval,
                 step = interval,
                 ...)
  } else
    widget
}

# These are the shader edits that glTF displays
# need: "skins" applies skins, "normalTextures" applies
# normal textures


shaderChanges <- list(
  skins = list(
    vertexShader = list(
      decls = list(
        old = "void main(void)",
        new = function(n, ...) sprintf("  attribute vec4 aJoint;
  attribute vec4 aWeight;
  uniform vec4 uJointMat[%d];
  void main(void) {", 4*n)),
      pos = list(
        old = "gl_Position = prMatrix * vPosition;",
        new = function(swiz, ...) c("    mat4 skinMat = mat4(0);
    for (int i = 0; i < 4; i++) {",
        paste(sprintf("      skinMat[i] += aWeight.%s * uJointMat[4*int(aJoint.%s) + i];", swiz, swiz), collapse = "\n"),
        "    }
    vec4 pos = skinMat * vec4(aPos, 1.);
    gl_Position = prMatrix * mvMatrix * pos;")
      ),
      normal = list(
        old = "vNormal = normMatrix * vec4(-aNorm, dot(aNorm, aPos));",
        new = "    skinMat = mat4(mat3(skinMat));
    vNormal = normMatrix * skinMat * vec4(-aNorm, dot(aNorm, pos.xyz/pos.w));"
      )
    )
  )
)

# obj <- scene$objects[[as.character(id)]]

# shaders <- getShaders(id, scene)

modifyShaders <- function(shaders, mod, ...) {

  if (is.character(mod))
    mod <- shaderChanges[[mod]]

  for (type in c("vertexShader", "fragmentShader")) {
    if (!is.null(mod[[type]])) {
      shader <- unlist(strsplit(shaders[[type]], "\n"))
      for (part in mod[[type]]) {
        lines <- grep(part$old, shader, fixed = TRUE)
        if (is.function(part$new))
          part$new <- part$new(...)
        for (line in rev(lines))
          shader <- append(shader[-line],
                           part$new, after = line - 1)
      }
      shaders[[type]] <- shader
    }
  }

  shaders
}
