
extractTexture <- function(gltf, index = 0, outfile = tempfile(), verbose = TRUE, closeConnections = TRUE) {
  if (closeConnections)
    on.exit(closeBuffers(gltf))

  texture <- gltf$textures[[index + 1]]
  if (is.null(texture))
    stop("No such texture.")
  class(texture) <- "gltfTexture"
  if (!is.null(texture$source)) {
    image <- gltf$images[[texture$source + 1]]
    mime <- image$mimeType
    class(image) <- "gltfImage"
    if (!is.null(image$bufferView)) {
      readlist <- readBufferview(image$bufferView, gltf)
      gltf <- readlist[[2]]
      view <- readlist[[1]]
      if (is.null(offset <- view$byteOffset))
        offset <- 0
      seek(view$bufferdata, offset)
      data <- readBin(view$bufferdata, "raw", view$byteLength)
      if (!nchar(file_ext(outfile)) && !is.null(mime))
        outfile <- paste0(outfile, ".", basename(mime))
      writeBin(data, outfile)
      if (verbose)
        cat("Extracted ", mime, " file ", outfile)
      if (!closeConnections)
        attr(outfile, "gltf") <- gltf
      attr(outfile, "mimeType") <- mime
      invisible(outfile)
    }
  }
}
