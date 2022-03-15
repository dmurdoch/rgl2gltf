
extractTexture <- function(gltf, index = 0, outfile = tempfile(), verbose = TRUE) {

  texture <- gltf$getTexture(index)
  if (is.null(texture))
    stop("No such texture.")
  if (!is.null(texture$source)) {
    image <- gltf$getImage(texture$source)
    mime <- image$mimeType
    if (!is.null(image$bufferView)) {
      view <- gltf$getBufferview(image$bufferView)
      con <- gltf$openBufferview(image$bufferView)
      data <- readBin(con, "raw", view$byteLength)
      if (!nchar(file_ext(outfile)) && !is.null(mime))
        outfile <- paste0(outfile, ".", basename(mime))
      writeBin(data, outfile)
      if (verbose)
        cat("Extracted ", mime, " file ", outfile)
      if (mime != "image/png")
        warning(sprintf("MIME type %s not supported as texture in rgl (texture %d).", mime, index))
      invisible(outfile)
    }
  }
}
