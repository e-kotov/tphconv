#' Convert Transport Opportunities CSV to a Raster
#'
#' Reads a gzipped CSV of public transport accessibility data, reprojects point coordinates,
#' and rasterizes the data onto a regular grid. Optionally writes the raster to disk.
#'
#' @param input_file Character. Path to the input CSV (gzipped) containing at least
#'   columns 'lon', 'lat', and one numeric data column.
#' @param out_column_name Character or NULL. Name to assign to the data column in the output raster.
#'   If NULL (default), the original column name from the CSV is used.
#' @param crs_src Integer or character. Source CRS (EPSG code or WKT); default is 4326. Normally, you should not change this, unless you know the data is in a different CRS.
#' @param crs_dst Integer or character. Destination CRS (EPSG code or WKT); default is 3035. Normally, you should not change this. The function relies on the fact that the source of the Transport Poverty Hub input data is the standardized GISCO grid, so internally it offests the coordinates by 500 m to align with the grid. If you change the CRS, you may need to adjust this offset.
#' @param x_offset Numeric. Offset applied to the projected coordinates to align with the GISCO (or some other) grid; default is 500 m. Normally, you should not change this, unless you know the data is in a different CRS.
#' @param y_offset Numeric. Offset applied to the projected coordinates to align with the GISCO (or some other) grid; default is 500 m. Normally, you should not change this, unless you know the data is in a different CRS.
#' @param resolution_m Numeric. Cell size in units of the destination CRS; default is 1000 (1 km).
#' @param out_raster_file Character or NULL. Output filename for the raster. If provided, the raster is
#'   written to this file (creating directories as needed) and the filename is returned.
#'   If NULL (default), the SpatRaster is returned directly.
#' @return If \code{file} is NULL, a \code{terra::SpatRaster} object with rasterized values.
#'   If \code{file} is provided, a character string of the file path is returned.
#' @export
#' @examples
#' tph_file <- system.file(
#'  "extdata",
#'  "ver1_0_LU_1km_pt_ppl_within_10-20_min.csv.gz",
#'  package = "tphconv"
#' )
#' # Convert a CSV file to a raster
#' r <- tph_to_raster(
#'   input_file = tph_file)
#' library(terra)
#' plot(r)
#'
#' # Convert a CSV file to a raster and write it to disk
#' tmp_file <- tempfile(fileext = ".tiff")
#' out_raster_file <- tph_to_raster(
#'   input_file = tph_file,
#'   out_raster_file = tmp_file
#' )
#' #' # Check the output file
#' library(terra)
#' r <- rast(out_raster_file)
#' plot(r)
#' unlink(tmp_file)
#'
tph_to_raster <- function(
  input_file,
  out_column_name = NULL,
  crs_src = 4326,
  crs_dst = 3035,
  x_offset = -500,
  y_offset = -500,
  resolution_m = 1000,
  out_raster_file = NULL
) {
  # Read the CSV
  x <- readr::read_csv(input_file, show_col_types = FALSE)

  # Identify the single data column (excluding lon/lat)
  data_col <- setdiff(names(x), c("lon", "lat"))
  if (length(data_col) != 1) {
    stop(
      "Expected exactly one data column besides 'lon' and 'lat'; found: ",
      paste(data_col, collapse = ", ")
    )
  }

  # Rename column if requested
  if (!is.null(out_column_name)) {
    x[[out_column_name]] <- x[[data_col]]
    field_name <- out_column_name
    x[[data_col]] <- NULL
  } else {
    field_name <- data_col
  }

  sanitize_names <- function(x) {
    # 1. Remove just the brackets (but leave a space in their place)
    x <- gsub("[\\(\\)\\[\\]]", " ", x)
    # 2. Collapse any run of spaces into a single underscore
    x <- gsub(" +", "_", x)
    # 3. Remove any non‐alphanumeric/underscore (just in case)
    x <- gsub("[^A-Za-z0-9_]", "", x)
    # 4. Collapse multiple underscores, trim leading/trailing
    x <- gsub("_+", "_", x)
    x <- gsub("^_|_$", "", x)
    # 5. Lowercase
    tolower(x)
  }
  names(x) <- sanitize_names(names(x))
  field_name <- sanitize_names(field_name)

  # Get WKT strings for source and destination CRS
  wkt_src <- sf::st_crs(crs_src)$wkt
  wkt_dst <- sf::st_crs(crs_dst)$wkt

  # Project coordinates
  coords_src <- as.matrix(x[, c("lon", "lat")])
  coords_dst <- sf::sf_project(from = wkt_src, to = wkt_dst, pts = coords_src)
  x$x <- coords_dst[, 1] - x_offset
  x$y <- coords_dst[, 2] - y_offset
  x$lon <- NULL
  x$lat <- NULL

  # Create an empty raster template
  data_extent <- c(
    min(x$x),
    max(x$x),
    min(x$y),
    max(x$y)
  )
  raster_template <- terra::rast(
    extent = data_extent,
    resolution = resolution_m,
    crs = wkt_dst
  )

  # Rasterize the points
  x_spatvec <- terra::vect(
    x,
    geom = c("x", "y"),
    crs = wkt_dst
  )

  out_raster <- terra::rasterize(
    x = x_spatvec,
    y = raster_template,
    field = field_name
  )
  names(out_raster) <- field_name
  # Set NA flag
  terra::NAflag(out_raster) <- -1

  # If file specified, write to disk and return filename
  if (!is.null(out_raster_file)) {
    out_dir <- fs::path_dir(out_raster_file)
    if (!fs::dir_exists(out_dir)) {
      fs::dir_create(out_dir, recurse = TRUE)
    }
    terra::writeRaster(out_raster, filename = out_raster_file, overwrite = TRUE)
    return(out_raster_file)
  }

  # Otherwise return the raster
  return(out_raster)
}
