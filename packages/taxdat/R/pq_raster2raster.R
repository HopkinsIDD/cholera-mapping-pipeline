## // Basic Type definitions
## // byte : 1 byte
## // uint16 : 16 bit unsigned integer (2 bytes)
## // uint32 : 32 bit unsigned integer (4 bytes)
## // float64 : double precision floating point number (8 bytes)
##
##  +------------------------------------------------------------+
##  | RASTER                                                     |
##  +---------------+-------------+------------------------------+
##  | - name -      |  - type -   | - meaning -                  |
##  +---------------+-------------+------------------------------+
##  | endiannes     | byte        | 1:ndr/little endian          |
##  |               |             | 0:xdr/big endian             |
##  +---------------+-------------+------------------------------+
##  | version       | uint16      | format version (0 for this   |
##  |               |             | structure)                   |
##  +---------------+-------------+------------------------------+
##  | nBands        | uint16      | Number of bands              |
##  +---------------+-------------+------------------------------+
##  | scaleX        | float64     | pixel width                  |
##  |               |             | in geographical units        |
##  +---------------+-------------+------------------------------+
##  | scaleY        | float64     | pixel height                 |
##  |               |             | in geographical units        |
##  +---------------+-------------+------------------------------+
##  | ipX           | float64     | X ordinate of upper-left     |
##  |               |             | pixel's upper-left corner    |
##  |               |             | in geographical units        |
##  +---------------+-------------+------------------------------+
##  | ipY           | float64     | Y ordinate of upper-left     |
##  |               |             | pixel's upper-left corner    |
##  |               |             | in geographical units        |
##  +---------------+-------------+------------------------------+
##  | skewX         | float64     | rotation about Y-axis        |
##  +---------------+-------------+------------------------------+
##  | skewY         | float64     | rotation about X-axis        |
##  +---------------+-------------+------------------------------+
##  | srid          | int32       | Spatial reference id         |
##  +---------------+-------------+------------------------------+
##  | width         | uint16      | number of pixel columns      |
##  +---------------+-------------+------------------------------+
##  | height        | uint16      | number of pixel rows         |
##  +---------------+-------------+------------------------------+
##  | bands[nBands] | RASTERBAND  | Bands data                   |
##  +---------------+-------------+------------------------------+

get_endian_string <- function(x) {
  return(ifelse(x, "little", "big"))
}

get_size <- function(x) {
  return(as.numeric(gsub("[^1234567890]", "", x)) / 8)
}

get_mode <- function(x) {
  mode_changer <- c(
    "float" = list(double()),
    "uint" = list(integer()),
    "int" = list(integer())
  )
  return(mode_changer[gsub("[1234567890]", "", x)])
}

get_signed <- function(x) {
  signed_changer <- c(
    "float" = TRUE,
    "uint" = FALSE,
    "int" = TRUE
  )
  return(signed_changer[gsub("[1234567890]", "", x)])
}

#' @export
#' @name as.raster.pq_raster
#' @title as.raster.pq_raster
#' @param x A wkb (as hex pq_raster string) representation of a raster dumped from postgresql
#' @description Convert a raster from the default postgres output to an R raster object
#' @return A raster::raster
as.raster.pq_raster <- function(x) {
  npairs <- nchar(x) / 2
  raw_pq_rast <- as.raw(paste0("0x", substring(x, first = seq_len(npairs) * 2 - 1, last = seq_len(npairs) * 2)))
  ## Header is 12 fields : 1 byte field, 4 unit16 fields, 1 int32 field, 6 float64 fields
  ## Header is 61 bytes : 1 + 4 * 2 + 1 * 4 + 6 * 8
  ## Each element is 2 hex digits. each hex digit is 1 byte
  header_types <- c(
    endiannes = "uint8",
    version = "uint16",
    nBands = "uint16",
    scaleX = "float64",
    scaleY = "float64",
    ipX = "float64",
    ipY = "float64",
    skewX = "float64",
    skewY = "float64",
    srid = "uint32",
    width = "uint16",
    height = "uint16"
  )

  pixel_types <- list(
    "bool1",
    "uint2",
    "uint4",
    "int8",
    "uint8",
    "int16",
    "uint16",
    "int32",
    "uint32",
    NA,
    "float32",
    "float64"
  )


  header <- parse_raw(x = raw_pq_rast, spec = header_types)
  header$endiannes <- get_endian_string(header$endiannes)
  raw_pq_rast_bands <- raw_pq_rast[-(1:61)]
  for (band_idx in 1:header$nBands) {
    first_byte <- rawToBits(raw_pq_rast_bands[1])
    isOffline <- as.logical(first_byte[8])
    hasNodataValue <- as.logical(first_byte[7])
    # first_byte[1:4] <- first_byte[5:8]
    first_byte[5:8] <- rawToBits(raw(1))[1]
    pixtype <- pixel_types[[as.integer(packBits(first_byte)) + 1]]
    pix_size <- get_size(pixtype)
    nodata_value <- readBin(con = raw_pq_rast_bands[1 + seq_len(pix_size)], what = get_mode(pixtype)[[1]], size = pix_size, signed = get_signed(pixtype), endian = header$endiannes)

    ## Parse data
    if (isOffline) {
      stop("This code is not yet written")
      ## pix_size + 2 : band number
      ## pix_size + 3 + : null terminated file name string for file to read
    } else {
      return(raster::raster(
        nrows = header$width, ncols = header$height, xmn = header$ipX, ymx = header$ipY, xmx = header$ipX + header$width * header$scaleX, ymn = header$ipY + header$height * header$scaleY,
        vals = readBin(
          con = raw_pq_rast_bands[pix_size + 1 + seq_len(header$width * header$height * pix_size)],
          what = get_mode(pixtype)[[1]],
          size = pix_size,
          signed = get_signed(pixtype),
          n = header$width * header$height,
          endian = header$endiannes
        )
      ))
    }
  }
}

parse_raw <- function(x, spec) {
  sizes <- get_size(spec)
  modes <- get_mode(spec)
  signs <- get_signed(spec)
  info <- data.frame(
    name = names(spec),
    type = spec,
    size = sizes,
    signed = signs
  ) %>%
    dplyr::mutate(
      mode = modes,
      start = cumsum(c(0, size[-length(size)])) + 1,
      end = cumsum(size),
      raw_value = mapply(start, end, FUN = function(s, e) {
        x[s:e]
      }, SIMPLIFY = FALSE),
      value = as.numeric(rep(NA, times = length(size)))
    )
  is_little_endian <- as.logical(info$raw_value[info$name == "endiannes"][[1]])
  endiannes <- get_endian_string(is_little_endian)

  rc <- list()
  for (row_idx in seq_len(nrow(info))) {
    rc[[info$name[[row_idx]]]] <- readBin(con = info$raw_value[[row_idx]], what = info$mode[[row_idx]], size = info$size[[row_idx]], signed = info$signed[[row_idx]], endian = endiannes)
  }
  return(rc)
}
