setPBRshaders <- function(gltf, gltfMat,
                          id,
                          scene = scene3d(minimal = TRUE),
                          useIBL = TRUE,
                          brdfLUT = system.file("textures/brdfLUT.png", package = "rgl2gltf"),
                          IBLspecular = system.file("textures/refmap.png", package = "rgl"),
                          IBLdiffuse = system.file("textures/refmapblur.jpeg", package = "rgl2gltf"),
                          debugBaseColor = 0,
                          debugMetallic = 0,
                          debugRoughness = 0,
                          debugSpecularReflection = 0,
                          debugGeometricOcclusion = 0,
                          debugMicrofacetDistribution = 0,
                          debugSpecContrib = 0,
                          debugDiffuseContrib = 0,
                          debugIBLDiffuse = 1,
                          debugIBLSpecular = 1,
                          defines = list(),
                          uniforms = list(),
                          attributes = list(),
                          textures = list()) {

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

  uniforms[["u_ScaleDiffBaseMR"]] <- c(debugDiffuseContrib,
                                       debugBaseColor,
                                       debugMetallic,
                                       debugRoughness)
  uniforms[["u_ScaleFGDSpec"]]    <- c(debugSpecularReflection,
                                       debugGeometricOcclusion,
                                       debugMicrofacetDistribution,
                                       debugSpecContrib)

  uniforms[["u_LightColor"]] <- c(1,1,1)

  rv <- c(1, 1)
  if (!is.null(mr <- gltfMat$pbrMetallicRoughness)) {
    if (!is.null(mf <- mr$metallicFactor))
      rv[1] <- mf
    if (!is.null(rf <- mr$roughnessFactor))
      rv[2] <- rf
    if (!is.null(mt <- mr$metallicRoughnessTexture)) {
      textures[["u_MetallicRoughnessSampler"]] <-
        extractTexture(gltf, mt$index, verbose = FALSE)
      defines[["HAS_METALROUGHNESSMAP"]] <- 1
    }
  }
  uniforms[["u_MetallicRoughnessValues"]] <- rv

  if (!is.null(ot <- gltfMat$occlusionTexture)) {
    textures[["u_OcclusionSampler"]] <-
      extractTexture(gltf, ot$index, verbose = FALSE)
    strength <- ot$strength
    if (is.null(strength)) strength <- 1
    uniforms[["u_OcclusionStrength"]] <- strength;
    defines[["HAS_OCCLUSIONMAP"]] <- 1
  }

  if (!is.null(em <- gltfMat$emissiveTexture)) {
    textures[["u_EmissiveSampler"]] <-
      extractTexture(gltf, em$index, verbose = FALSE)
    factor <- unlist(gltfMat$emissiveFactor)
    if (is.null(factor))
      factor <- c(0, 0, 0)
    uniforms[["u_EmissiveFactor"]] <- factor
    defines[["HAS_EMISSIVEMAP"]] <- 1
  }

  if (useIBL) {
    textures[["u_DiffuseEnvSampler"]] <- IBLdiffuse
    textures[["u_SpecularEnvSampler"]] <- IBLspecular
    textures[["u_brdfLUT"]] <- brdfLUT
    defines[["USE_IBL"]] <- 1
    uniforms[["u_ScaleIBLAmbient"]] <- c(debugIBLDiffuse,
                                         debugIBLSpecular)
  }
  defines[["MANUAL_SRGB"]] <- 1

  if (is.null(alphaMode <- gltfMat$alphaMode))
    alphaMode <- "OPAQUE"
  if (is.null(alphaCutoff <- gltfMat$alphaCutoff))
    alphaCutoff <- 0.5

  if (alphaMode != "MASK")
    alphaCutoff <- -1

  uniforms[["u_alphaCutoff"]] <- alphaCutoff

  if (length(defines))
    defines <- paste("#define", names(defines), unlist(defines))
  else
    defines <- character()

  vshader <- c(defines, readLines(system.file("shaders/pbr-vert.glsl", package = "rgl2gltf")))
  fshader <- c(defines, readLines(system.file("shaders/pbr-frag.glsl", package = "rgl2gltf")))

  scene$objects[[as.character(id)]] <- obj

  setUserShaders(id,
                 vertexShader = vshader,
                 fragmentShader = fshader,
                 attributes = attributes,
                 uniforms = uniforms,
                 textures = textures,
                 scene = scene)
}
