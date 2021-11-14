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

catindices <- catvalues <- function(obj, string) {
  if (!is.null(obj))
    cat(string, "      not implemented\n")
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

Gltf$set("public", "print", function(verbose = FALSE, names = FALSE, showExtras = TRUE, ...) {

  shownames <- function(sec) {
    if (!is.null(private[[sec]]) && sec %in% namefields) {
      section <- private[[sec]]
      for (i in seq_along(section)) {
        if (!is.null(name <- section[[i]]$name))
          cat(sprintf("  %d: %s\n", i - 1, name))
      }
    }
  }

  saveopt <- options(rgl2gltf.showExtras = showExtras)
  on.exit(options(saveopt))

  knowntoplevel <- c("accessors", "asset", "scene", "scenes", "nodes", "buffers", "bufferViews", "meshes", "cameras", "materials", "textures", "images")
  if (!is.logical(verbose)) {
    verbosefields <- match.arg(verbose, knowntoplevel, several.ok = TRUE)
    verbose <- TRUE
  } else {
    if (verbose) verbosefields <- knowntoplevel
    else verbosefields <- NULL
  }

  if (!is.logical(names)) {
    namefields <- match.arg(names, knowntoplevel, several.ok = TRUE)
    names <- TRUE
  } else {
    if (names) namefields <- knowntoplevel
    else namefields <- NULL
  }

  if (!is.null(asset <- private$asset)) {
    cat("asset fields:\n")
    catstring(asset$version, "  GLtf version %s file.\n")
    catstring(asset$generator, "  Generated by %s.\n")
    catstring(asset$copyright, "  Copyright %s\n")
    catstring(asset$minVersion, "  minVersion %s\n")
    catother(asset)

    catstring(setdiff(names(asset), c("version", "generator", "copyright", "minVersion", other)),
              "  Other asset fields:  %s.\n")
  }
  if (length(self$scene))
    defaultScene <- as.numeric(self$scene)
  else
    defaultScene <- -1

  if (length(scenes <- private$scenes)) {
    if ("scenes" %in% verbosefields) {
      cat("Scenes:\n")
      for (i in seq_along(scenes)) {
        catstring(i-1, "  Scene %s:")
        if (defaultScene == i-1)
          cat(" (default)")
        cat("\n")
        scene <- scenes[[i]]
        class(scene) <- "gltfScene"
        print(scene)
      }
    } else {
      cat("Scenes (", length(scenes), ")\n")
      shownames("scenes")
    }
  }
  if (length(nodes <- private$nodes)) {
    if ("nodes" %in% verbosefields) {
      cat("Nodes:\n")
      for (i in seq_along(nodes)) {
        catstring(i-1, "  Node %s:\n")
        node <- nodes[[i]]
        class(node) <- "gltfNode"
        print(node)
      }
    } else {
      cat("Nodes (", length(nodes), ")\n")
      shownames("nodes")
    }
  }

  if (length(buffers <- private$buffers)) {
    if ("buffers" %in% verbosefields) {
      cat("Buffers:\n")
      for (i in seq_along(buffers)) {
        catstring(i-1, "  Buffer %s:\n")
        buffer <- buffers[[i]]
        class(buffer) <- "gltfBuffer"
        print(buffer)
      }
    } else {
      cat("Buffers (", length(buffers), ")\n")
      shownames("buffers")
    }
  }

  if (length(bufferViews <- private$bufferViews)) {
    if ("bufferviews" %in% verbosefields) {
      cat("Buffer views:\n")
      for (i in seq_along(bufferViews)) {
        catstring(i-1, "  Buffer view %s:\n")
        view <- bufferViews[[i]]
        class(view) <- "gltfBufferview"
        print(view)
      }
    } else {
      cat("Bufferviews (", length(bufferViews), ")\n")
      shownames("bufferViews")
    }
  }

  if (length(meshes <- private$meshes)) {
    if ("meshes" %in% verbosefields) {
      cat("Meshes:\n")
      for (i in seq_along(meshes)) {
        catstring(i-1, "  Mesh %s:\n")
        mesh <- meshes[[i]]
        class(mesh) <- "gltfMesh"
        print(mesh)
      }
    } else {
      cat("Meshes (", length(meshes), ")\n")
      shownames("meshes")
    }
  }

  if (length(cameras <- private$cameras)) {
    if ("cameras" %in% verbosefields) {
      cat("Cameras:\n")
      for (i in seq_along(cameras)) {
        catstring(i-1, "  Camera %s:\n")
        camera <- cameras[[i]]
        class(camera) <- "gltfCamera"
        print(camera)
      }
    } else {
      cat("Cameras (", length(cameras), ")\n")
      shownames("cameras")
    }
  }

  if (length(accessors <- private$accessors)) {
    if ("accessors" %in% verbosefields) {
      cat("Accessors:\n")
      for (i in seq_along(accessors)) {
        acc <- accessors[[i]]
        catstring(i-1, "  Accessor %s:\n")
        class(acc) <- "gltfAccessor"
        print(acc)
      }
    } else {
      cat("Accessors (", length(accessors), ")\n")
      shownames("accessors")
    }
  }

  if (length(materials <- private$materials)) {
    if ("materials" %in% verbosefields) {
      cat("Materials:\n")
      for (i in seq_along(materials)) {
        catstring(i-1, "  Material %s:\n")
        mat <- materials[[i]]
        class(mat) <- "gltfMaterial"
        print(mat)
      }
    } else {
      cat("Materials (", length(materials), ")\n")
      shownames("materials")
    }
  }

  if (length(textures <- private$textures)) {
    if ("textures" %in% verbosefields) {
      cat("Textures:\n")
      for (i in seq_along(textures)) {
        catstring(i-1, "  Texture %s:\n")
        texture <- textures[[i]]
        class(texture) <- "gltfTexture"
        print(texture)
      }
    } else {
      cat("Textures (", length(textures), ")\n")
      shownames("textures")
    }
  }

  if (length(images <- private$images)) {
    if ("images" %in% verbosefields) {
      cat("Images:\n")
      for (i in seq_along(images)) {
        catstring(i-1, "  Image %s:\n")
        image <- images[[i]]
        class(image) <- "gltfImage"
        print(image)
      }
    } else {
      cat("Images (", length(images), ")\n")
      shownames("images")
    }
  }

  catstring(setdiff(names(private), knowntoplevel),
            "Other fields:  %s.\n")
  invisible(self)
})

showtree <- function(gltf) {
  showNode <- function(n) {
    node <- nodes[[n+1]]
    cat(paste(rep(" ", indent), collapse = ""))
    cat("Node ", n)
    if (!is.null(extras <- node$extras) &&
        !is.null(obj <- extras$RGL_obj))
      cat(" (", obj$type, ")")
    if (!is.null(name <- node$name))
      cat(" ", name)
    if (!is.null(meshnum <- node$mesh)) {
      mesh <- gltf$meshes[[meshnum + 1]]
      if (!is.null(mesh$name))
        cat(" (mesh ", mesh$name, ")")
    }
    cat("\n")
    indent <<- indent + 2
    for (i in node$children)
      showNode(i)
    indent <<- indent - 2
  }
  if (!is.null(gltf$nodes)) {
    nodes <- gltf$nodes
    isChild <- rep(FALSE, length(nodes))
    for (i in seq_along(nodes)) {
      if (!is.null(children <- unlist(nodes[[i]]$children)))
        isChild[children + 1] <- TRUE
    }
    roots <- which(!isChild) - 1
    for (n in roots) {
      indent <- 0
      showNode(n)
    }
  }
}
