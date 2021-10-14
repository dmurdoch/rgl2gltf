readBuffer <- function(buf, gltf) {
  buffer <- gltf$buffers[[buf+1]]
  bufferdata <- buffer$bufferdata
  if (is.null(bufferdata)) {
    if (!is.null(buffer$uri))
      bufferdata <- file(buffer$uri, "rb")
    else if (!is.null(defaultbin <- attr(gltf, "defaultbin")))
      bufferdata <- rawConnection(defaultbin)
    else
      stop("buffer data not found for buffer ", buf)
    gltf$buffers[[buf+1]]$bufferdata <- bufferdata
  }
  list(bufferdata, gltf)
}

readBufferview <- function(bufv, gltf) {
  bufferview <- gltf$bufferViews[[bufv+1]]
  class(bufferview) <- "gltfBufferview"
  results <- readBuffer(bufferview$buffer, gltf)
  bufferview$bufferdata <- results[[1]]
  list(bufferview, results[[2]])
}

# Usually this function will be called just to clean
# up the connections, but maybe some caller will want to
# keep working with the gltf object
closeBuffers <- function(gltf) {
  for (i in seq_along(gltf$buffers))
    if (!is.null(con <- gltf$buffers[[i]]$bufferdata)) {
      close(con)
      gltf$buffers[[i]]$bufferdata <- NULL
    }
  gltf
}
