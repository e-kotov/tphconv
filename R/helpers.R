#' Helper functions for the package
#' @keywords internal
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
