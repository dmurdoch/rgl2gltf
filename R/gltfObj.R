# glTF constants

targetArray <- 34962
targetElementArray <- 34963

#' @title R6 Class for glTF file objects
#'
#' @description
#' The glTF file spec is described here: \url{https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html}.  This
#' object encapsulates most of the data from those files.

Gltf <- R6Class("gltf",
  inherit = Buffer,
  public = list(
    #' @field scene The default scene number
    scene = NULL,

    #' @param json
    #'   List read from glTF file.
    #' @param defaultbin
    #'   Optional external binary file.
    initialize = function(json = NULL, defaultbin = NULL) {
      super$initialize(json, defaultbin)
      json$buffers <- json$bufferViews <- json$accessors <- NULL
      for (n in ls(private))
        if (!is.null(json[[n]]))
          private[[n]] <- json[[n]]
      self$scene <- json$scene
    },


    #' @description
    #' Write values to accessor, including `min` and `max`.
    #'
    #' The GLTF standard requires `min` and `max` values in
    #' accessors, whereas other uses of buffers may not.
    #' This function stores in the usual way using the
    #' [`Buffer$addAccessor()`][Buffer] method, and then adds
    #' `min` and `max` values.
    #'
    #' The standard also doesn't support signed 4 byte integers
    #' or double precision values, so we test for those here.
    #'
    #' @param values Values to write.
    #' @param target Optional target use for values.
    #' @param useDouble Whether to write doubles or singles.
    #'
    #' @return New accessor number.
    #'
    addAccessor = function(values, target = NULL, useDouble = FALSE) {
      acc <- super$addAccessor(values, target, useDouble)
      accessor <- self$getAccessor(acc)
      if (accessor$componentType %in% c(typeSignedInt, typeDouble))
        stop("Type is not supported in glTF")
      values <- self$readAccessor(acc)
      if (any(!is.finite(values)))
        stop("Only finite values are supported in glTF")
      if (is.matrix(values)) {
        accessor$max <- I(apply(values, 2, max))
        accessor$min <- I(apply(values, 2, min))
      } else {
        accessor$max <- I(max(values))
        accessor$min <- I(min(values))
      }
      self$setAccessor(acc, accessor)
      acc
    },

    #' @description Get scene object.
    #' @param sc Scene number.
    #' @return Scene object, documented here:
    #' \url{https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#reference-scene}.
    getScene = function(sc)
      structure(private$scenes[[sc + 1]], class = "gltfScene"),

    #' @description Update scene record.
    #' @param sc Which scene to update.
    #' @param scene New scene record.
    setScene = function(sc, scene)
      private$scenes[[sc + 1]] <- unclass(scene),

    #' @description Add a scene object.
    #' @return Scene number.
    addScene = function() {
      scene <- list()
      private$scenes <- c(private$scenes, list(scene))
      self$scene <- length(private$scenes) - 1
      self$scene
    },

    #' @description Add node to scene.
    #' @param scene Scene number to modify.
    #' @param node Node number(s) to add.
    addToScene = function(scene, node) {
      sceneobj <- self$getScene(scene)
      sceneobj$nodes <- I(c(sceneobj$nodes, node))
      self$setScene(scene, sceneobj)
    },

    #' @description Get default scene, creating it if necessary.
    #' @return Scene number.
    defaultScene = function() {
      if (is.null(self$scene))
        self$addScene()
      self$scene
    },

    #' @description Get node object.
    #' @param n Node number.
    #' @return Node object, documented here:
    #' \url{https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#reference-node}.
    getNode = function(n)
      structure(private$nodes[[n + 1]], class = "gltfNode"),

    #' @description Set node object.
    #' @param n Node number.
    #' @param node New node object.
    setNode = function(n, node)
      private$nodes[[n + 1]] <- unclass(node),

    #' @description Add a node object.
    #' @param mesh A mesh number.
    #' @param matrix A matrix transformation for the node.
    #' @param extras A list of extras, typically `rgl` objects.
    #' @return Node number.
    addNode = function(mesh = NULL, matrix = NULL, extras = NULL) {
      node <- list()
      node$mesh <- mesh
      if (!is.null(matrix) && !all(matrix == diag(4)))
        node$matrix <- as.numeric(matrix)
      if (!is.null(extras))
        node$extras <- extras
      private$nodes <- c(private$nodes, list(node))
      length(private$nodes) - 1
    },

    #' @description Add node as child of another.
    #' @param parent Node number to modify.
    #' @param node Node number(s) to add as children.
    addChild = function(parent, node) {
      parentobj <- self$getNode(parent)
      parentobj$children <- I(c(parentobj$children, node))
      self$setNode(parent, parentobj)
    },

    #' @description Get camera object.
    #' @param cam Camera number.
    #' @return Camera object, documented here:
    #' \url{https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#reference-camera}.
    getCamera = function(cam)
      structure(private$cameras[[cam + 1]], class = "gltfCamera"),

    #' @description Get top-level extras list.
    #' @return Extras list, including rgl objects.
    getExtras = function()
      private$extras,

    #' @description Set extras list.
    #' @param extras New extras list.
    setExtras = function(extras)
      private$extras <- unclass(extras),

    #' @description Get mesh object.
    #' @param m Mesh number.
    #' @return Mesh object, documented here:
    #' \url{https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#reference-mesh}.
    getMesh = function(m)
      structure(private$meshes[[m + 1]], class = "gltfMesh"),

    #' @description Add a mesh object.
    #' @param primitives A list of primitive objects.
    #' @return Mesh number.
    addMesh = function(primitives) {
      if (length(primitives)) {
        mesh <- list(primitives = primitives)
        private$meshes <- c(private$meshes, list(mesh))
        length(private$meshes) - 1
      }
    },

    #' @description Get material object.
    #' @param m Material number.
    #' @return Material object, documented here:
    #' \url{https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#reference-material}.
    getMaterial = function(m)
      structure(private$materials[[m + 1]], class = "gltfMaterial"),

    #' @description Get texture object.
    #' @param tex Texture number.
    #' @return Texture object, documented here:
    #' \url{https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#reference-texture}.
    getTexture = function(tex)
      structure(private$textures[[tex + 1]], class = "gltfTexture"),

    #' @description Get image object.
    #' @param im Image number.
    #' @return Image object, documented here:
    #' \url{https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#reference-image}.
    getImage = function(im)
      structure(private$images[[im + 1]], class = "gltfImage"),

    #' @description Construct and possibly add material.
    #'
    #' This will return an existing material if possible.
    #' @param mat An `rgl` material record.
    #' @param defaultMaterial Default material properties.
    #' @return Material number.
    addMaterial = function(mat, defaultMaterial = list()) {
      newmat <- defaultMaterial
      newmat[names(mat)] <- mat
      mat <- newmat
      material <- list()

      pbrMetallicRoughness <- list()
      col <- c(1,1,1,1)
      if (!is.multicolored(mat)) {
        if (!is.null(mat$color))
          col[1:3] <- col2rgb(mat$color)/255
        if (!is.null(mat$alpha))
          col[4] <- mat$alpha
        if (any(col != 1))
          pbrMetallicRoughness$baseColorFactor <- col
      }
      if (!is.null(mat$emission))
        material$emissiveFactor <- c(col2rgb(mat$emission)/255)
      if (!is.null(mat$texture))
        pbrMetallicRoughness$baseColorTexture <- self$addTexture(mat)
      if (length(pbrMetallicRoughness))
        material$pbrMetallicRoughness <- pbrMetallicRoughness

      # Some properties have already been handled
      mat$color <- mat$alpha <- mat$emission <- mat$texture <- NULL
      # Include the rest as an extension
      material$extras <- list(RGL_material_properties = mat)
      self$getMaterialNumber(material)
    },

    #' @description Add a texture.
    #' @param mat An `rgl` material record.
    #' @return Texture number.
    addTexture = function(mat) {
      texture <- list()
      texture$source <- self$addImage(mat)
      texture$sampler <- self$addSampler(mat)
      texture$name <- basename(mat$texture)
      private$textures <- c(private$textures, list(texture))
      length(private$textures) - 1
    },

    #' @description Add an image for a texture.
    #' @param mat An `rgl` material record.
    #' @return Image number.
    addImage = function(mat) {
      image <- list()
      bytes <- readBin(mat$texture, "raw", file.size(mat$texture))
      image$bufferView <- self$addBufferView(bytes, typeUnsignedInt, size = 1)
      image$mimeType <- "image/png"
      image$name <- basename(mat$texture)
      private$images <- c(private$images, list(image))
      length(private$images) - 1
    },

    #' @description Add a sampler.
    #' @param mat An `rgl` material record.
    #' @return Sampler number.
    addSampler = function(mat) {
      sampler <- list()
      sampler$magFilter <- getFilter(mat$texmagfilter)
      sampler$minFilter <- getFilter(mat$texminfilter)
      if (length(sampler)) {
        private$samplers <- c(private$samplers, list(sampler))
        length(private$samplers) - 1
      }
    },

    #' @description Add or return a material.
    #' @param material A glTF material record.
    #' @return Material number.
    getMaterialNumber = function(material) {
      materials <- private$materials
      for (i in seq_along(materials))
        if (identical(materials[[i]], material)) {
          return(i - 1)
        }

      private$materials <- c(private$materials, list(material))
      length(private$materials) - 1
    },

    #' @description Write data.
    #' @param coords Data to write, or `NULL`.
    #' @param target Optional target use for data.
    #' @return Accessor number, or `NULL`.
    writeVectors = function(coords, target = NULL) {
      if (!is.null(coords)) {
        self$addAccessor(coords, target = target)
      } else
        NULL
    },

    #' @description Create a primitive record.
    #' @param inds Indices of vertices.
    #' @param mode Mode of primitive.
    #' @param attributes Primitive attributes.
    #' @param matnum Material number.
    #' @return Primitive record, documented here:
    #' \url{https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#reference-primitive}.
    makePrimitive = function(inds, mode = NULL, attributes = NULL, matnum = NULL) {
      indices <- as.integer(inds)
      if (length(indices)) {
        indices <- self$addAccessor(indices - 1L, targetElementArray)
        primitive <- list(attributes = attributes,
                          material = matnum,
                          mode = mode,
                          indices = indices
                         )
        list(primitive)
      }
    },

    #' @description Get asset list.
    #' @return Asset object, documented here:
    #' \url{https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#reference-asset}.
    getAsset = function()
      structure(private$asset, class = "gltfAsset"),

    #' @description Set asset list.
    #' @param version Version number of glTF format.
    #' @param generator Identifier of code generating it.
    setAsset = function(version, generator)
      private$asset <- list(version = version, generator = generator),

    #' @description Get local transform.
    #' @param node Node object.
    #' @param parentTransform Matrix transform of parent object.
    #' @return 4x4 matrix of local transform.
    getTransform = function(node, parentTransform) {
      if (!is.null(node$matrix)) {
        transform <- matrix(unlist(node$matrix), 4, 4)
      } else {
        transform <- diag(4)
        if (!is.null(node$scale)) {
          scale <- unlist(node$scale)
          transform <- t(scaleMatrix(scale[1], scale[2], scale[3])) %*% transform
        }
        if (!is.null(node$rotation)) {
          rot <- unlist(node$rotation)
          transform <- t(rotationMatrix(rot[4], rot[1], rot[2], rot[3])) %*% transform
        }
        if (!is.null(node$translation)) {
          trans <- unlist(node$translation)
          transform <- t(translationMatrix(trans[1], trans[2], trans[3])) %*% transform
        }
      }
      parentTransform %*% transform
    },

    #' @description Reconstruct `rgl` material.
    #' @param n Material number.
    #' @return `rgl` material record.
    getRglMaterial = function(n) {
      if (is.null(n))
        result <- list()
      else {
        material <- self$getMaterial(n)
        result <- list(color = "white", alpha = 1)
        if (!is.null(pbrm <- material$pbrMetallicRoughness)) {
          if (!is.null(col <- unlist(pbrm$baseColorFactor))) {
            result$color <- rgb(col[1], col[2], col[3])
            result$alpha <- col[4]
          }
          if (!is.null(texnum <- unlist(pbrm$baseColorTexture))) {
            texturefile <- extractTexture(self, texnum,
                                          verbose = FALSE)
            mime <- attr(texturefile, "mimeType")
            if (!is.null(mime) && mime != "image/png")
              warning(sprintf("MIME type %s not supported as texture in rgl (texture %d).", mime, texnum))
            attributes(texturefile) <- NULL
            result$texture <- texturefile
            texture <- self$getTexture(texnum)
            result$gltftexCoord <- texture$texCoord
          }
        }
        if (!is.null(col <- unlist(material$emissiveFactor)))
          result$emission <- rgb(col[1], col[2], col[3])
        else
          result$emission <- "black"

        if (!is.null(ext <- material$extras)
            && !is.null(props <- ext$RGL_material_properties)) {
          result[names(props)] <- props
        } else
          result$specular <- "gray10"
      }
      result
    },

    #' @description Print `gltf` objects with various levels of detail.
    #' @param verbose Logical indicator of verbose printing, or
    #' character vector of components to print verbosely.
    #' @param names Print names for components.
    #' @param showExtras Logical:  show extra fields?
    #' @param ... Passed `...` .
    #' @examples
    #' samples <- "https://raw.githubusercontent.com/KhronosGroup/glTF-Sample-Models/master/2.0"
    #' gltf <- readGLB(paste0(samples, "/2CylinderEngine/glTF-Binary/2CylinderEngine.glb?raw=true"))
    #' gltf$print(names = "meshes")
    print = function(verbose = FALSE, names = FALSE, showExtras = TRUE, ...) {

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

      others <- setdiff(names(private), "finalize")
      others <- Filter(function(n) length(private[[n]]) > 0, others)

      catstring(setdiff(others, knowntoplevel),
                "Other fields:  %s.\n")
      invisible(self)
    },

    #' @description Get number of items in private list.
    #' @param list Name of list to get.
    listCount = function(list) {
      length(private[[list]])
    }

  ),

  private = list(
    asset = list(),
    cameras = list(),
    extras = list(),
    extensions = list(),
    images = list(),
    meshes = list(),
    materials = list(),
    nodes = list(),
    samplers = list(),
    scenes = list(),
    textures = list()
  )
)
