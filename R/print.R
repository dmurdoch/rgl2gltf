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

catmetallic <- function(obj, string) {
  if (length(obj)) {
    cat(string)
    catstring(obj$baseColorFactor, "      baseColorFactor: %s\n")
    cattexture(obj$baseColorTexture, "      baseColorTexture:\n")
    catstring(obj$metallicFactor, "      metallicFactor: %s\n")
    catstring(obj$roughnessFactor, "      roughnessFactor: %s\n")
    cattexture(obj$metallicRoughnessTexture, "      metallicRoughnessTexture:\n")
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



catindices <- catvalues <-
  catnormal <- catocclusion <- cattexture <- function(obj, string) {
    if (!is.null(obj))
      cat(string, "      not implemented\n")
  }

print.gltfAccessor <- function(x, ...) {
  catstring(x$bufferView, "    bufferView: %s\n")
  catstring(x$byteOffset, "    byteOffset: %s\n")
  catcomptype(x$componentType, "    componentType: %s\n")
  catstring(x$normalized, "    normalied: %s\n")
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
  catmatrix(x$matrix, "    Matrix:\n")
  catstring(x$children, "    Children: %s\n")
  catstring(x$mesh, "    Mesh: %s\n")
  catother(x)
  catstring(setdiff(names(x), c("children", "matrix", "mesh", other)),
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
  cattexture(x$emissiveTexture, "    emissiveTexture:\n")
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

print.gltf <- function(x, verbose = FALSE, ...) {
  knowntoplevel <- c("accessors", "asset", "scene", "scenes", "nodes", "buffers", "bufferViews", "meshes", "cameras", "materials")
  if (!is.logical(verbose)) {
    verbosefields <- verbose
    verbose <- TRUE
  } else {
    if (verbose) verbosefields <- knowntoplevel
    else verbosefields <- NULL
  }

  if (!is.null(x$asset)) {
    cat("asset fields:\n")
    catstring(x$asset$version, "  GLtf version %s file.\n")
    catstring(x$asset$generator, "  Generated by %s.\n")
    catstring(x$asset$copyright, "  Copyright %s\n")
    catstring(x$asset$minVersion, "  minVersion %s\n")
    catother(x$asset)

    catstring(setdiff(names(x$asset), c("version", "generator", "copyright", "minVersion", other)),
              "  Other asset fields:  %s.\n")
  }
  if (length(x$scene))
    defaultScene <- as.numeric(x$scene)
  else
    defaultScene <- -1

  if (length(x$scenes)) {
    if ("scenes" %in% verbosefields) {
      cat("Scenes:\n")
      for (i in seq_along(x$scenes)) {
        catstring(i-1, "  Scene %s:")
        if (defaultScene == i-1)
          cat(" (default)")
        cat("\n")
        scene <- x$scenes[[i]]
        class(scene) <- "gltfScene"
        print(scene)
      }
    } else
      cat("Scenes (", length(x$scenes), ")\n")
  }
  if (length(x$nodes)) {
    if ("nodes" %in% verbosefields) {
      cat("Nodes:\n")
      for (i in seq_along(x$nodes)) {
        catstring(i-1, "  Node %s:\n")
        node <- x$nodes[[i]]
        class(node) <- "gltfNode"
        print(node)
      }
    } else
      cat("Nodes (", length(x$nodes), ")\n")
  }

  if (length(x$buffers)) {
    if ("buffers" %in% verbosefields) {
      cat("Buffers:\n")
      for (i in seq_along(x$buffers)) {
        catstring(i-1, "  Buffer %s:\n")
        buffer <- x$buffers[[i]]
        class(buffer) <- "gltfBuffer"
        print(buffer)
      }
    } else
      cat("Buffers (", length(x$buffers), ")\n")
  }

  if (length(x$bufferViews)) {
    if ("bufferviews" %in% verbosefields) {
      cat("Buffer views:\n")
      for (i in seq_along(x$bufferViews)) {
        catstring(i-1, "  Buffer view %s:\n")
        view <- x$bufferViews[[i]]
        class(view) <- "gltfBufferview"
        print(view)
      }
    } else
      cat("Bufferviews (", length(x$bufferviews), ")\n")
  }

  if (length(x$meshes)) {
    if ("meshes" %in% verbosefields) {
      cat("Meshes:\n")
      for (i in seq_along(x$meshes)) {
        catstring(i-1, "  Mesh %s:\n")
        mesh <- x$meshes[[i]]
        class(mesh) <- "gltfMesh"
        print(mesh)
      }
    } else
      cat("Meshes (", length(x$meshes), ")\n")
  }

  if (length(x$cameras)) {
    if ("cameras" %in% verbosefields) {
      cat("Cameras:\n")
      for (i in seq_along(x$cameras)) {
        catstring(i-1, "  Camera %s:\n")
        camera <- x$cameras[[i]]
        class(camera) <- "gltfCamera"
        print(camera)
      }
    } else
      cat("Cameras (", length(x$cameras), ")\n")
  }

  if (length(x$accessors)) {
    if ("accessors" %in% verbosefields) {
      cat("Accessors:\n")
      for (i in seq_along(x$accessors)) {
        acc <- x$accessors[[i]]
        catstring(i-1, "  Accessor %s:\n")
        class(acc) <- "gltfAccessor"
        print(acc)
      }
    } else
      cat("Accessors (", length(x$accessors), ")\n")
  }

  if (length(x$materials)) {
    if ("materials" %in% verbosefields) {
      cat("Materials:\n")
      for (i in seq_along(x$materials)) {
        catstring(i-1, "  Material %s:\n")
        mat <- x$materials[[i]]
        class(mat) <- "gltfMaterial"
        print(mat)
      }
    } else
      cat("Materials (", length(x$materials), ")\n")
  }

  catstring(setdiff(names(x), knowntoplevel),
            "Other fields:  %s.\n")
  invisible(x)
}

