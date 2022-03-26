setPBRshaders <- function(gltf, prim,
                          id,
                          scene = scene3d(minimal),
                          minimal = TRUE) {
  defines <- list()
  uniforms <- list()
  attributes <- list()
  textures <- list()

  obj <- scene$objects[[as.character(id)]]
  obj$material$lit <- TRUE
  mat <- obj$material
  if (!is.null(obj$texcoords))
    defines[["HAS_UV"]] <- 1
  if (!is.null(obj$normals))
    defines[["HAS_NORMALS"]] <- 1
  if (!is.null(obj$material$texture))
    defines[["HAS_BASECOLORMAP"]] <- 1
  if (!is.null(obj$material$normalTexture)) {
    textures[["normalTexture"]] <- obj$material$normalTexture
    if (is.null(obj$tangents)) {
      obj <- getTangents(obj)
    }
    attributes[["aTangent"]] <- obj$tangents

    defines[["HAS_NORMALMAP"]] <- 1
    defines[["HAS_TANGENTS"]] <- 1

    uniforms[["u_NormalScale"]] <- 1
  }

  uniforms[["u_ScaleDiffBaseMR"]] <- c(0,0,0,0)
  uniforms[["u_ScaleFGDSpec"]]    <- c(0,0,0,0)

  uniforms[["u_LightColor"]] <- c(1,1,1)

  gltfMat <- gltf$getMaterial(prim$material)
  rv <- c(1, 1)
  if (!is.null(mr <- gltfMat$pbrMetallicRoughness)) {
    if (!is.null(mf <- mr$metallicFactor))
      rv[1] <- mf
    if (!is.null(rf <- mr$roughnessFactor))
      rv[2] <- rf
    if (!is.null(mt <- mr$metallicRoughnessTexture)) {
      textures[["u_MetallicRoughnessSampler"]] <- extractTexture(gltf, mt$index)
      defines[["HAS_METALROUGHNESSMAP"]] <- 1
    }
  }
  uniforms[["u_MetallicRoughnessValues"]] <- rv
  uniforms[["u_ScaleIBLAmbient"]] <- c(1,1)

  if (!is.null(ot <- gltfMat$occlusionTexture)) {
    textures[["u_OcclusionSampler"]] <- extractTexture(gltf, ot$index)
    strength <- ot$strength
    if (is.null(strength)) strength <- 1
    uniforms[["u_OcclusionStrength"]] <- strength;
    defines[["HAS_OCCLUSIONMAP"]] <- 1
  }

  if (length(defines))
    defines <- paste("#define", names(defines), unlist(defines))
  else
    defines <- character()

  vshader <- c(defines, readLines(system.file("shaders/pbr-vert.glsl", package = "rgl2gltf")))
  fshader <- c(defines, readLines(system.file("shaders/pbr-frag.glsl", package = "rgl2gltf")))

  scene$objects[[as.character(id)]] <- obj

  # defines[["MANUAL_SRGB"]] <- 1

  setUserShaders(id,
                 vertexShader = vshader,
                 fragmentShader = fshader,
                 attributes = attributes,
                 uniforms = uniforms,
                 textures = textures,
                 scene = scene)
}
