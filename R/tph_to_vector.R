#' Convert Transport Opportunities CSV to a Spatial Vector Object (Direct Method)
#'
#' Reads a CSV file of public transport accessibility data and converts it
#' into a spatial vector format (`sf` or `terra::SpatVector`). This function
#' builds polygon geometries directly from the point coordinates in a vectorized
#' manner using the `sfheaders` package, which can be faster for sparse datasets
#' than methods requiring an intermediate raster.
#'
#' @inheritParams tph_to_raster
#' @param out_vector_file Character or `NULL`. If a file path is provided (e.g.,
#'   `"data/luxembourg.gpkg"`), the output is written to that file using
#'   `sf::st_write`, and the file path is returned. If `NULL` (the default),
#'   the function returns an in-memory spatial object.
#' @param return_as Character. Specifies the class of the returned object when
#'   `out_vector_file` is `NULL`. Can be either `"sf"` (the default) or
#'   `"SpatVector"`. This argument is ignored if `out_vector_file` is not `NULL`.
#' @param add_id Logical. If `TRUE` (the default), an INSPIRE-compliant grid
#'   cell ID is generated and added as a column named `"id"`. Set to `FALSE`
#'   to omit this step.
#'
#' @return
#' Depending on the arguments:
#' - If `out_vector_file` is `NULL`, returns an `sf` object (default) or a `SpatVector`
#'   object (if `return_as = "SpatVector"`) with polygon geometries.
#' - If `out_vector_file` is a character string, the path to the created file is returned.
#' @export
#' @examples
#' # This function requires the 'sfheaders' package
#' if (requireNamespace("sfheaders", quietly = TRUE)) {
#'   tph_file <- system.file(
#'     "extdata",
#'     "ver1_0_LU_1km_pt_ppl_within_10-20_min.csv.gz",
#'     package = "tphconv"
#'   )
#'
#'   # Convert a CSV file to an sf object using the direct method
#'   sf_obj_direct <- tph_to_vector(input_file = tph_file)
#'   plot(sf_obj_direct)
#' }
tph_to_vector <- function(
  input_file,
  out_column_name = NULL,
  crs_src = 4326,
  crs_dst = 3035,
  x_offset = -500,
  y_offset = -500,
  resolution_m = 1000,
  out_vector_file = NULL,
  return_as = c("sf", "SpatVector"),
  add_id = TRUE
) {
  # Check for required package
  if (!requireNamespace("sfheaders", quietly = TRUE)) {
    stop(
      "Package 'sfheaders' is required. Please install it with `install.packages('sfheaders')`."
    )
  }

  return_as <- match.arg(return_as)
  df <- readr::read_csv(input_file, show_col_types = FALSE)
  data_col <- setdiff(names(df), c("lon", "lat"))
  if (length(data_col) != 1) {
    stop("Expected exactly one data column besides 'lon' and 'lat'.")
  }
  if (!is.null(out_column_name)) {
    names(df)[names(df) == data_col] <- out_column_name
    field_name <- sanitize_names(out_column_name)
  } else {
    field_name <- sanitize_names(data_col)
  }
  names(df) <- sanitize_names(names(df))

  wkt_src <- sf::st_crs(crs_src)$wkt
  wkt_dst <- sf::st_crs(crs_dst)$wkt
  coords_dst <- sf::sf_project(
    from = wkt_src,
    to = wkt_dst,
    pts = as.matrix(df[, c("lon", "lat")])
  )

  # --- FIX: REMOVED INCORRECT OFFSET APPLICATION ---
  # The projected coordinates are the true centroids. Do not shift them.
  x_proj <- coords_dst[, 1]
  y_proj <- coords_dst[, 2]

  # Vectorized calculation of polygon corners from centroids
  half_res <- resolution_m / 2
  xmin <- x_proj - half_res
  ymin <- y_proj - half_res
  xmax <- x_proj + half_res
  ymax <- y_proj + half_res

  n_polygons <- nrow(df)
  df_vertices <- data.frame(
    id = rep(seq_len(n_polygons), each = 5),
    x = as.vector(rbind(xmin, xmax, xmax, xmin, xmin)),
    y = as.vector(rbind(ymin, ymin, ymax, ymax, ymin))
  )

  sf_geoms <- sfheaders::sf_polygon(
    obj = df_vertices,
    x = "x",
    y = "y",
    polygon_id = "id"
  )

  sf_gridded <- sf::st_sf(
    df[, field_name, drop = FALSE],
    geometry = sf::st_geometry(sf_geoms),
    crs = wkt_dst
  )

  if (add_id) {
    sf_gridded$id <- tph_generage_grid_id(
      x = sf_gridded,
      cellsize = resolution_m,
      crs = crs_dst
    )
  }

  if (!is.null(out_vector_file)) {
    out_dir <- fs::path_dir(out_vector_file)
    if (!fs::dir_exists(out_dir)) {
      fs::dir_create(out_dir, recurse = TRUE)
    }
    sf::st_write(sf_gridded, dsn = out_vector_file, delete_dsn = TRUE)
    return(out_vector_file)
  } else {
    if (return_as == "SpatVector") {
      return(terra::vect(sf_gridded))
    } else {
      return(sf_gridded)
    }
  }
}
