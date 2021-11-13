# glTF constants

targetArray <- 34962
targetElementArray <- 34963

Gltf <- R6Class("gltf",
  inherit = Buffer,
  public = list(
    scene = NULL,

    initialize = function(json = NULL, defaultbin = NULL) {
      super$initialize(json, defaultbin)
      json$buffers <- json$bufferViews <- json$accessors <- NULL
      for (n in ls(private))
        if (!is.null(json[[n]]))
          private[[n]] <- json[[n]]
      self$scene <- json$scene
    },

    listCount = function(list) {
      length(private[[list]])
    },

    getScene = function(sc)
      structure(private$scenes[[sc + 1]], class = "gltfScene"),

    getNode = function(n)
      structure(private$nodes[[n + 1]], class = "gltfNode"),

    getCamera = function(cam)
      structure(private$cameras[[cam + 1]], class = "gltfCamera"),

    getExtras = function()
      private$extras,

    getMesh = function(m)
      structure(private$meshes[[m + 1]], class = "gltfMesh"),

    getMaterial = function(m)
      structure(private$materials[[m + 1]], class = "gltfMaterial"),

    getTexture = function(tex)
      structure(private$textures[[tex + 1]], class = "gltfTexture"),

    getImage = function(im)
      structure(private$images[[im + 1]], class = "gltfImage"),

    writeVectors = function(coords, target = NULL) {
      if (!is.null(coords)) {
        self$addAccessor(coords, target = target)
      } else
        NULL
    },

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
        pbrMetallicRoughness$baseColorTexture <- addTexture(mat)
      if (length(pbrMetallicRoughness))
        material$pbrMetallicRoughness <- pbrMetallicRoughness

      # Some properties have already been handled
      mat$color <- mat$alpha <- mat$emission <- mat$texture <- NULL
      # Include the rest as an extension
      material$extras <- list(RGL_material_properties = mat)
      self$getMaterialNumber(material)
    },

    # The material is in glTF format; have
    # we recorded it already?
    getMaterialNumber = function(material) {
      materials <- private$materials
      for (i in seq_along(materials))
        if (identical(materials[[i]], material)) {
          return(i - 1)
        }

      private$materials <- c(private$materials, list(material))
      length(private$materials) - 1
    },


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

    addMesh = function(primitives) {
      if (length(primitives)) {
        mesh <- list(primitives = primitives)
        private$meshes <- c(private$meshes, list(mesh))
        length(private$meshes) - 1
      }
    },

    addNode = function(mesh = NULL, matrix = NULL, extras = NULL) {
      node <- list()
      node$mesh <- mesh
      if (!is.null(matrix))
        node$matrix <- as.numeric(matrix)
      if (!is.null(extras))
        node$extras <- extras
      private$nodes <- c(private$nodes, list(node))
      length(private$nodes) - 1
    },

    addScene = function() {
      scene <- list()
      private$scenes <- c(private$scenes, list(scene))
      self$scene <- length(private$scenes) - 1
      self$scene
    },

    defaultScene = function() {
      if (is.null(self$scene))
        self$addScene()
      self$scene
    },

    addToScene = function(scene, node) {
      sceneobj <- self$getScene(scene)
      sceneobj$nodes <- I(c(sceneobj$nodes, node))
      self$setScene(scene, sceneobj)
    },

    addChild = function(parent, node) {
      parentobj <- self$getNode(parent)
      parentobj$children <- I(c(parentobj$children, node))
      self$setNode(parent, parentobj)
    },

    setScene = function(sc, scene)
      private$scenes[[sc + 1]] <- scene,

    setAsset = function(version, generator)
      private$asset <- list(version = version, generator = generator)
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
    scenes = list(),
    textures = list()
  )
)
