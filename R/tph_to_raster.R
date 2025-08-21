#' Convert Transport Opportunities CSV to a Raster
#'
#' Reads a CSV file (optionally gzipped) of public transport accessibility data,
#' reprojects point coordinates, and rasterizes the data onto a regular grid.
#' Optionally, it can add a second raster layer containing INSPIRE-compliant
#' grid cell IDs.
#'
#' @param input_file Character. Path to the input CSV (gzipped) containing at least
#'   columns 'lon', 'lat', and one numeric data column.
#' @param out_column_name Character or NULL. Name to assign to the data column in the output raster.
#'   If NULL (default), the original column name from the CSV is used.
#' @param crs_src Integer or character. Source CRS (EPSG code or WKT); default is 4326.
#' @param crs_dst Integer or character. Destination CRS (EPSG code or WKT); default is 3035.
#' @param x_offset Numeric. An offset subtracted from the projected x-coordinate.
#'   This is used to convert the source point's reference (e.g., a lower-left
#'   corner) to the cell's theoretical centroid. For centroid-based data like
#'   GISCO, the default of 0 is correct.
#' @param y_offset Numeric. An offset subtracted from the projected y-coordinate,
#'   similar to `x_offset`.
#' @param resolution_m Numeric. Cell size in units of the destination CRS; default is 1000 (1 km).
#' @param out_raster_file Character or NULL. Output filename for the raster. If provided, the raster is
#'   written to this file (creating directories as needed) and the filename is returned.
#'   If NULL (default), the SpatRaster is returned directly.
#' @param add_id Logical. If `TRUE` (the default), a second layer named "id" is
#'   added to the raster. This is a factor layer where the labels correspond to
#'   the INSPIRE-compliant grid cell ID for each cell.
#' @return If `out_raster_file` is `NULL`, a `terra::SpatRaster` object. This will be a
#'   multi-layer raster if `add_id = TRUE`. If `out_raster_file` is provided, a character
#'   string of the file path is returned.
#' @export
#' @examples
#' tph_file <- system.file(
#'  "extdata",
#'  "ver1_0_LU_1km_pt_ppl_within_10-20_min.csv.gz",
#'  package = "tphconv"
#' )
#' library(terra)
#'
#' # Convert a CSV to a raster with the ID layer (default)
#' r_with_id <- tph_to_raster(input_file = tph_file)
#' plot(r_with_id)
#'
tph_to_raster <- function(
  input_file,
  out_column_name = NULL,
  crs_src = 4326,
  crs_dst = 3035,
  x_offset = 0,
  y_offset = 0,
  resolution_m = 1000,
  out_raster_file = NULL,
  add_id = TRUE
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

  names(x) <- sanitize_names(names(x))
  field_name <- sanitize_names(field_name)

  # Get WKT strings for source and destination CRS
  wkt_src <- sf::st_crs(crs_src)$wkt
  wkt_dst <- sf::st_crs(crs_dst)$wkt

  # Project coordinates
  coords_src <- as.matrix(x[, c("lon", "lat")])
  coords_dst <- sf::sf_project(from = wkt_src, to = wkt_dst, pts = coords_src)

  # Apply user-defined offsets to get the theoretical centroids
  x_centroids <- coords_dst[, 1] - x_offset
  y_centroids <- coords_dst[, 2] - y_offset

  resolution_m <- as.numeric(resolution_m)
  x_ll <- floor(x_centroids / resolution_m) * resolution_m
  y_ll <- floor(y_centroids / resolution_m) * resolution_m
  x$x <- x_ll + (resolution_m / 2)
  x$y <- y_ll + (resolution_m / 2)

  x$lon <- NULL
  x$lat <- NULL

  xmin <- min(x_ll)
  ymin <- min(y_ll)
  # The max extent is the corner of the *last* cell
  xmax <- max(x_ll) + resolution_m
  ymax <- max(y_ll) + resolution_m
  data_extent <- c(xmin, xmax, ymin, ymax)

  raster_template <- terra::rast(
    extent = data_extent,
    resolution = resolution_m,
    crs = wkt_dst
  )

  # Rasterize the points
  x_spatvec <- terra::vect(x, geom = c("x", "y"), crs = wkt_dst)
  out_raster <- terra::rasterize(
    x = x_spatvec,
    y = raster_template,
    field = field_name
  )
  names(out_raster) <- field_name
  terra::NAflag(out_raster) <- -1

  # Add ID layer if requested
  if (add_id) {
    ids <- tph_generage_grid_id(
      x = out_raster,
      cellsize = resolution_m,
      crs = crs_dst,
      raster_mask_na = TRUE
    )
    id_levels <- unique(ids)
    id_numeric <- as.numeric(factor(ids, levels = id_levels))
    id_raster <- terra::rast(out_raster, nlyrs = 1, vals = NA)
    names(id_raster) <- "id"
    id_raster[as.numeric(names(ids))] <- id_numeric
    levels(id_raster) <- data.frame(
      id = seq_along(id_levels),
      gisco_id = id_levels
    )
    out_raster <- c(out_raster, id_raster)
  }

  # If file specified, write to disk and return filename
  if (!is.null(out_raster_file)) {
    out_dir <- fs::path_dir(out_raster_file)
    if (!fs::dir_exists(out_dir)) {
      fs::dir_create(out_dir, recurse = TRUE)
    }
    terra::writeRaster(out_raster, filename = out_raster_file, overwrite = TRUE)
    return(out_raster_file)
  }

  return(out_raster)
}
