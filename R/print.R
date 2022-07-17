catstring <- function(obj, string) {
  if (length(obj)) {
    obj <- paste(obj, collapse = ", ")
    cat(sprintf(string, obj))
  }
}

catnamed <- function(obj, string) {
  if (length(obj)) {
    cat(string)
    n <- names(obj)
    for (i in seq_along(obj)) {
      catstring(obj[[i]], paste0("          ", n[i], ": %s\n"))
    }
  }
}

catmatrix <- function(obj, string) {
  if (length(obj)) {
    cat(string)
    m <- matrix(as.numeric(obj), 4,4)
    m <- signif(m, 3)
    print(m)
  }
}

other <- c("name", "extensions", "extras")
catother <- function(obj) {
  catstring(obj$name, "    name: %s\n")
  catstring(obj$extensions, "  extensions %s\n")
  showExtras <- getOption("rgl2gltf.showExtras", TRUE)
  if (showExtras)
    catstring(obj$extras,     "  extras %s\n")
}

catmode <- function(mode, string) {
  modes <- c("0 (points)", "1 (lines)", "2 (line_loop)", "3 (line_strip)",
             "4 (triangles)", "5 (triangle_strip)", "6 (triangle_fan)")
  if (length(mode))
    catstring(modes[mode+1], string)
}

catcomptype <- function(ctype, string) {
  types <- c("5120" = "5120 (byte)", "5121" = "5121 (unsigned_byte)",
             "5122" = "5122 (short)", "5123" = "5123 (unsigned_short)",
             "5125" = "5125 (unsigned_int)", "5126" = "5126 (float)")
  if (length(ctype)) {
    stype <- types[as.character(ctype)]
    stype[is.na(stype)] <- paste(ctype[is.na(stype)], "(?)")
    catstring(stype, string)
  }
}

cattarget <- function(obj, string) {
  targets <- c("34962" = "34962 (ARRAY_BUFFER)",
               "34963" = "34963 (ELEMENT_ARRAY_BUFFER)")
  if (length(obj)) {
    starget <- targets[as.character(obj)]
    starget[is.na(starget)] <- paste(obj[is.na(starget)], "(?)")
    catstring(starget, string)
  }
}

catsparse <- function(sparse, string) {
  if (length(sparse)) {
    cat(string)
    catstring(sparse$count,    "      count: %s\n")
    catindices(sparse$indices, "      indices:\n")
    catvalues(sparse$values,   "      values:\n")
    catother(sparse)
  }
}

catprimitives <- function(obj, string) {
  cat(string)
  for (i in seq_along(obj)) {
    prim <- obj[[i]]
    class(prim) <- "gltfPrimitive"
    catstring(i-1,"      primitive %s:\n")
    print(prim)
  }
}

cattextureinfo <- function(obj, string) {
  if (length(obj)) {
    cat(string)
    catstring(obj$index,    "        index: %s\n")
    catstring(obj$texCoord, "        texCoord: %s\n")
    catother(obj)
  }
}

catmetallic <- function(obj, string) {
  if (length(obj)) {
    cat(string)
    catstring(obj$baseColorFactor, "      baseColorFactor: %s\n")
    cattextureinfo(obj$baseColorTexture, "      baseColorTexture:\n")
    catstring(obj$metallicFactor, "      metallicFactor: %s\n")
    catstring(obj$roughnessFactor, "      roughnessFactor: %s\n")
    cattextureinfo(obj$metallicRoughnessTexture, "      metallicRoughnessTexture:\n")
    catother(obj)
  }
}

catpersp <- function(persp, string) {
  if (length(persp)) {
    cat(string)
    catstring(persp$aspectRatio, "      aspectRatio: %s\n")
    catstring(persp$yfov,        "      yfov: %s\n")
    catstring(persp$zfar,        "      zfar: %s\n")
    catstring(persp$znear,       "      znear: %s\n")
    catother(persp)
  }
}

catortho <- function(ortho, string) {
  if (length(ortho)) {
    cat(string)
    catstring(ortho$xmag, "      xmag: %s\n")
    catstring(ortho$ymag, "      ymag: %s\n")
    catstring(ortho$zfar,        "      zfar: %s\n")
    catstring(ortho$znear,       "      znear: %s\n")
    catother(ortho)
  }
}

catnormal <- function(normal, string) {
  if (length(normal)) {
    cat(string)
    catstring(normal$index, "      index: %s\n")
    catstring(normal$texCoord, "      texCoord: %s\n")
    catstring(normal$scale,        "      scale: %s\n")
    catother(normal)
  }
}

catocclusion <- function(occlusion, string) {
  if (length(occlusion)) {
    cat(string)
    catstring(occlusion$index, "      index: %s\n")
    catstring(occlusion$texCoord, "      texCoord: %s\n")
    catstring(occlusion$strength,        "      strength: %s\n")
    catother(occlusion)
  }
}

catchannels <- function(obj, string) {
  cat(string)
  for (i in seq_along(obj)) {
    channel <- obj[[i]]
    class(channel) <- "gltfChannel"
    catstring(i-1,"      channel %s:\n")
    print(channel)
  }
}

catchanneltarget <- function(obj, string) {
  cat(string)
  catstring(obj$node, "          node: %s\n")
  catstring(obj$path, "          path: %s\n")
  catother(obj)
}

catsamplers <- function(obj, string) {
    cat(string)
    for (i in seq_along(obj)) {
      sampler <- obj[[i]]
      class(sampler) <- "gltfAnimationSampler"
      catstring(i-1,"      sampler %s:\n")
      print(sampler)
    }
  }

catindices <- function(obj, string) {
  cat(string)
  catstring(obj$bufferView, "        bufferView: %s\n")
  catstring(obj$byteOffset, "        byteOffset: %s\n")
  catcomptype(obj$componentType, "        componentType: %s\n")
  catstring(obj$extensions, "        extensions %s\n")
  showExtras <- getOption("rgl2gltf.showExtras", TRUE)
  if (showExtras)
    catstring(obj$extras,     "        extras %s\n")
}

catvalues <- function(obj, string) {
  cat(string)
  catstring(obj$bufferView, "        bufferView: %s\n")
  catstring(obj$byteOffset, "        byteOffset: %s\n")
  catstring(obj$extensions, "        extensions %s\n")
  showExtras <- getOption("rgl2gltf.showExtras", TRUE)
  if (showExtras)
    catstring(obj$extras,     "        extras %s\n")
}

print.gltfAccessor <- function(x, ...) {
  catstring(x$bufferView, "    bufferView: %s\n")
  catstring(x$byteOffset, "    byteOffset: %s\n")
  catcomptype(x$componentType, "    componentType: %s\n")
  catstring(x$normalized, "    normalized: %s\n")
  catstring(x$count,      "    count: %s\n")
  catstring(x$type,       "    type: %s\n")
  catstring(x$max,        "    max: %s\n")
  catstring(x$min,        "    min: %s\n")
  catsparse(x$sparse,     "    sparse:\n")
  catother(x)
  catstring(setdiff(names(x), c("bufferView", "byteOffset",
                                  "componentType", "normalized", "count", "type", "max", "min", "sparse", other)),
            "  Other acc fields:  %s.\n")
  invisible(x)
}

print.gltfBufferview <- function(x, ...) {
  catstring(x$buffer, "    buffer: %s\n")
  catstring(x$byteLength,  "    byteLength: %s\n")
  catstring(x$byteOffset,  "    byteOffset: %s\n")
  catstring(x$byteStride,  "    byteStride: %s\n")
  cattarget(x$target,      "    target: %s\n")
  catother(x)
  catstring(setdiff(names(x), c("buffer", "byteLength", "byteOffset", "byteStride", "target", other)),
            "  Other bufferView fields:  %s.\n")
  invisible(x)
}

print.gltfPrimitive <- function(x, ...) {
  catnamed(x$attributes,  "        attributes:\n")
  catstring(x$indices,    "        indices: %s\n")
  catstring(x$material,   "        material: %s\n")
  catmode(x$mode,         "        mode: %s\n")
  catnamed(x$targets,     "        morph targets:\n")
  catother(x)
  catstring(setdiff(names(x), c("attributes", "indices", "material", "mode", "targets", other)),
            "  Other primitive fields:  %s.\n")
  invisible(x)
}

print.gltfNode <- function(x, ...) {
  catstring(x$camera,   "    Camera:   %s\n")
  catstring(x$children, "    Children: %s\n")
  catstring(x$skin,     "    Skin:     %s\n")
  catmatrix(x$matrix,   "    Matrix:\n")
  catstring(x$mesh,     "    Mesh:     %s\n")
  catstring(x$rotation, "    Rotation: %s\n")
  catstring(x$scale,    "    Scale:    %s\n")
  catstring(x$translation, "    Translation: %s\n")
  catstring(x$weights,  "    Weights:  %s\n")
  catother(x)
  catstring(setdiff(names(x), c("camera", "children", "skin",
                                "matrix", "mesh", "rotation",
                                "scale", "translation",
                                "weights", other)),
            "  Other node fields:  %s.\n")
  invisible(x)
}

print.gltfMesh <- function(x, ...) {
  catprimitives(x$primitives, "    primitives:\n")
  catstring(x$weights,  "    weights: %s\n")
  catother(x)
  catstring(setdiff(names(x), c("primitives", "weights", other)),
            "  Other mesh fields:  %s.\n")
  invisible(x)
}

print.gltfScene <- function(x, ...) {
  catstring(x$nodes, "    Nodes: %s\n")
  catother(x)
  catstring(setdiff(names(x), c("nodes", other)),
            "  Other scene fields:  %s.\n")
  invisible(x)
}

print.gltfBuffer <- function(x, ...) {
  catstring(x$uri,        "    uri: %s\n")
  catstring(x$byteLength, "    byteLength: %s\n")
  catother(x)

  catstring(setdiff(names(x), c("byteLength", "uri", other)),
            "  Other buffer fields:  %s.\n")
  invisible(x)
}

print.gltfCamera <- function(x, ...) {
  catstring(x$type, "    type: %s\n")
  catpersp(x$perspective, "    perspective:\n")
  catortho(x$orthographic, "    orthographic:\n")
  invisible(x)
}

print.gltfMaterial <- function(x, ...) {
  catother(x)
  catmetallic(x$pbrMetallicRoughness, "    pbrMetallicRoughness:\n")
  catnormal(x$normalTexture, "    normalTexture:\n")
  catocclusion(x$occlusionTexture, "    occlusionTexture:\n")
  cattextureinfo(x$emissiveTexture, "    emissiveTexture:\n")
  catstring(x$emissiveFactor, "    emissiveFactor: %s\n")
  catstring(x$alphaMode, "    alphaMode: %s\n")
  catstring(x$alphaCutoff, "    alphaCutoff: %s\n")
  catstring(x$doubleSided, "    doubleSided: %s\n")
  catstring(setdiff(names(x), c(other, "pbrMetallicRoughness", "normalTexture", "occlusionTexture",
                                  "emissiveTexture", "emissiveFactor", "alphaMode", "alphaCutoff",
                                  "doubleSided")),
            "  Other material fields:  %s.\n")
  invisible(x)
}

print.gltfTexture <- function(x, ...) {
  catstring(x$sampler, "    sampler: %s\n")
  catstring(x$source,  "    source: %s\n")
  catother(x)
  catstring(setdiff(names(x), c(other, "sampler", "source")),
            "    Other texture fields:  %s.\n")
}

print.gltfImage <- function(x, ...) {
  catstring(x$uri,           "    uri: %s\n")
  catstring(x$mimeType,      "    mimeType: %s\n")
  catstring(x$bufferView,    "    bufferView: %s\n")
  catother(x)
  catstring(setdiff(names(x), c(other, "uri", "mimeType", "bufferView")),
            "    Other image fields:  %s.\n")
}

print.gltfAnimation <- function(x, ...) {
  catchannels(x$channels,    "    channels:\n")
  catsamplers(x$samplers,    "    samplers:\n")
  catother(x)
  catstring(setdiff(names(x), c(other, "channels", "samplers")),
            "    Other animation fields:  %s.\n")
}

print.gltfChannel <- function(x, ...) {
  catstring(x$sampler, "        sampler: %s\n")
  catchanneltarget(x$target,  "        target:\n")
  catother(x)
}

print.gltfAnimationSampler <- function(x, ...) {
  catstring(x$input,         "        input: %s\n")
  catstring(x$interpolation, "        interpolation: %s\n")
  catstring(x$output,        "        output: %s\n")
  catother(x)
}

print.gltfSkin <- function(x, ...) {
  catstring(x$inverseBindMatrices, "        inverseBindMatrices: %s\n")
  catstring(x$skeleton,            "        skeleton: %s\n")
  catstring(x$joints,              "        joints: %s\n")
  catother(x)
}

showtree <- function(x, ...)
  UseMethod("showtree", x)

showtree.gltf <- function(x, ...) {
  gltf <- x
  showNode <- function(n) {
    node <- gltf$getNode(n)
    cat(paste(rep(" ", indent), collapse = ""))
    cat("Node ", n)
    if (!is.null(extras <- node$extras) &&
        !is.null(obj <- extras$RGL_obj))
      cat(" (", obj$type, ")")
    if (!is.null(name <- node$name))
      cat(" ", name)
    if (!is.null(meshnum <- node$mesh)) {
      mesh <- gltf$getMesh(meshnum)
      if (!is.null(mesh$name))
        cat(" (mesh ", mesh$name, ")")
    }
    cat("\n")
    indent <<- indent + 2
    for (i in node$children)
      showNode(i)
    indent <<- indent - 2
  }
  if ((n <- gltf$listCount("nodes")) > 0) {
    isChild <- rep(FALSE, n)
    for (i in seq_len(n)) {
      node <- gltf$getNode(i - 1)
      if (!is.null(children <- unlist(node$children)))
        isChild[children + 1] <- TRUE
    }
    roots <- which(!isChild) - 1
    for (n in roots) {
      indent <- 0
      showNode(n)
    }
  }
}

showtree.rglscene <- function(x, transform = FALSE, ...) {
  s <- x
  showSubscene <- function(sub) {
    indstr <- paste(rep(" ", indent), collapse = "")
    cat(indstr, "Subscene ", sub$id, "\n")
    if (transform) {
      if (!is.null(scale <- sub$par3d$scale) &&
          !all(scale == 1))
        cat(indstr, "scale: ", scale, "\n")
      if (!is.null(mat <- sub$par3d$userMatrix) &&
          !all(mat == diag(4))) {
        cat(indstr, "userMatrix:\n")
        cat(paste(indstr, "  ", capture.output(print(round(mat, 2)))), sep="\n")
      }
    }
    if (length(objects <- sub$objects)) {
      cat(indstr, "  Objects: ")
      for (i in seq_along(objects)) {
        cat(objects[i])
        obj <- s$objects[[as.character(objects[i])]]
        if (!is.null(tag <- obj$material$tag) &&
            nchar(tag))
          cat(" (", tag, ")", sep = "")
        if (i < length(objects))
          cat(", ")
      }
      cat("\n")
    }
    indent <<- indent + 2
    for (i in sub$subscenes)
      showSubscene(i)
    indent <<- indent - 2
  }
  indent <- 0
  showSubscene(s$rootSubscene)
}
