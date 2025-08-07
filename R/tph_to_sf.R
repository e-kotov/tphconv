#' Convert Transport Opportunities CSV to an sf Object
#'
#' Reads a gzipped CSV of public transport accessibility data, reprojects point coordinates,
#' and creates a regular grid of polygons with the data aggregated to the grid cells.
#' An INSPIRE-compliant grid cell ID is also generated for each cell.
#'
#' @param input_file Character. Path to the input CSV (gzipped) containing at least
#'   columns 'lon', 'lat', and one numeric data column.
#' @param out_column_name Character or NULL. Name to assign to the data column in the output.
#'   If NULL (default), the original column name from the CSV is used.
#' @param crs_src Integer or character. Source CRS (EPSG code or WKT); default is 4326.
#' @param crs_dst Integer or character. Destination CRS (EPSG code or WKT); default is 3035.
#' @param resolution_m Numeric. Cell size in units of the destination CRS; default is 1000 (1 km).
#' @param out_sf_file Character or NULL. Output filename for the sf object. If provided, the sf object is
#'   written to this file (creating directories as needed) and the filename is returned.
#'   If NULL (default), the sf object is returned directly. The file extension determines the driver used by `sf::st_write`.
#' @return If `out_sf_file` is NULL, an `sf` object with polygon geometries.
#'   If `out_sf_file` is provided, a character string of the file path is returned.
#' @export
#' @examples
#' tph_file <- system.file(
#'  "extdata",
#'  "ver1_0_LU_1km_pt_ppl_within_10-20_min.csv.gz",
#'  package = "tphconv"
#' )
#' # Convert a CSV file to an sf object
#' sf_obj <- tph_to_sf(
#'   input_file = tph_file
#' )
#' if (require("sf")) {
#'   plot(sf_obj)
#' }
#'
#' # Convert a CSV file to a GeoPackage
#' tmp_file <- tempfile(fileext = ".gpkg")
#' out_sf_file <- tph_to_sf(
#'   input_file = tph_file,
#'   out_sf_file = tmp_file
#' )
#' # Check the output file
#' if (require("sf")) {
#'   sf_obj_read <- sf::st_read(out_sf_file)
#'   plot(sf_obj_read)
#'   unlink(tmp_file)
#' }
tph_to_sf <- function(
  input_file,
  out_column_name = NULL,
  crs_src = 4326,
  crs_dst = 3035,
  resolution_m = 1000,
  out_sf_file = NULL
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
    x <- gsub("[\(\)\[\]]", " ", x)
    x <- gsub(" +", "_", x)
    x <- gsub("[^A-Za-z0-9_]", "", x)
    x <- gsub("_+$", "", x)
    x <- gsub("^_|", "", x)
    tolower(x)
  }
  names(x) <- sanitize_names(names(x))
  field_name <- sanitize_names(field_name)

  # Create sf object from points
  sf_pts <- sf::st_as_sf(x, coords = c("lon", "lat"), crs = crs_src)
  sf_pts <- sf::st_transform(sf_pts, crs = crs_dst)

  # Create grid
  grid <- sf::st_make_grid(sf_pts, cellsize = resolution_m)
  grid <- sf::st_as_sf(grid)

  # Spatially join points to grid
  # The result will have the same number of rows as the grid
  # and for each grid cell, a list of intersecting points
  sf_gridded <- sf::st_join(grid, sf_pts, join = sf::st_intersects)

  # Aggregate data in grid cells (e.g., take the mean if multiple points fall in a cell)
  # For now, we assume one point per cell as per the data source
  # If there are cells with no points, they will have NA
  sf_gridded <- sf_gridded[!is.na(sf_gridded[[field_name]]), ]


  # Generate INSPIRE ID
  inspire_id <- function(x, crs, cellsize) {
    crs_name <- sf::st_crs(crs)$name
    # A very basic way to get the CRS code from the name
    crs_code <- gsub("[^0-9]", "", crs_name)

    prefix <- glue::glue("CRS{crs_code}RES{cellsize}m")
    coords <- sf::st_coordinates(sf::st_centroid(x))
    y <- glue::glue("N{coords[,'Y']}")
    x <- glue::glue("E{coords[,'X']}")
    id <- glue::glue("{prefix}{y}{x}")
    return(id)
  }

  sf_gridded$inspire_id <- inspire_id(sf_gridded, crs_dst, resolution_m)

  # If file specified, write to disk and return filename
  if (!is.null(out_sf_file)) {
    out_dir <- fs::path_dir(out_sf_file)
    if (!fs::dir_exists(out_dir)) {
      fs::dir_create(out_dir, recurse = TRUE)
    }
    sf::st_write(sf_gridded, dsn = out_sf_file, append = FALSE)
    return(out_sf_file)
  }

  # Otherwise return the sf object
  return(sf_gridded)
}
