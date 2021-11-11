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
      structure(private$images[[im + 1]], class = "gltfImage")

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
