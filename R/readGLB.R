readGLB <- function(con, verbose = FALSE, ...) {
  # Convert two 16 bit uints to a 32 bit value
  uint <- function(x) x[1] + x[2]*2^16

  readChunk <- function() {
    header <- readBin(con, "integer", 4, size = 2, signed = FALSE)
    if (length(header) == 4) {
      chunkLength <- uint(header[1:2])
      chunkType <- uint(header[3:4])
      chunkData <- readBin(con, "raw", chunkLength)
      list(Type = chunkType, Data = chunkData)
    }
  }

  if (is.character(con)) {
    con <- file(con, "rb")
    on.exit(close(con))
  }
  header <- readBin(con, "integer", 6, size = 2, signed = FALSE)
  magic <- uint(header[1:2])
  if (magic != 0x46546C67)
    stop("Header magic value not found.")
  version <- uint(header[3:4])
  filesize <- uint(header[5:6])
  if (verbose) {
    cat("GLB format version ", version, "\n")
    cat("File size ", filesize, "\n")
  }
  json <- readChunk()
  if (json$Type != 0x4E4F534AL)
    stop("First chunk is not JSON")
  if (verbose)
    cat("JSON chunk of ", length(json$Data), " bytes.\n")
  bin <- readChunk()
  if (bin$Type != 0x004E4942L)
    stop("Second chunk is not BIN")
  if (verbose)
    cat("BIN chunk of ", length(bin$Data), " bytes.\n")
  chunks <- list()
  repeat {
    chunk <- readChunk()
    if (is.null(chunk)) break
    chunks[[length(chunks) + 1]] <- chunk
    if (verbose)
      cat("Chunk of type ", chunk$Type, " of ", length(chunk$Data), " bytes.\n")
  }
  jsonfile <- tempfile(fileext=".gltf")
  writeBin(json$Data, jsonfile)
  on.exit(unlink(jsonfile), add = TRUE)

  readglTF(jsonfile, defaultbin = bin$Data)
}
