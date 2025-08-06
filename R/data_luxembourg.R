#' Example accessibility CSV for Luxembourg
#'
#' A small gzipped CSV with access to opportunities measured as the
#' number of people reachable within 10–20 minutes on public transport
#' on a 1 km grid in Luxembourg.
#'
#' @name data_luxembourg
#' @docType data
#' @aliases ver1_0_LU_1km_pt_ppl_within_10-20_min.csv.gz
#'
#' @format A gzipped CSV file with 1 600 rows and 3 columns:
#' \describe{
#'   \item{lat}{*dbl*; latitude (decimal degrees)}
#'   \item{lon}{*dbl*; longitude (decimal degrees)}
#'   \item{opportunities (people)}{*dbl*; number of people reachable in 10–20 min on public transport}
#' }
#'
#' @section File location:
#' The raw CSV is installed at:
#' ```r
#' system.file(
#'   "extdata",
#'   "ver1_0_LU_1km_pt_ppl_within_10-20_min.csv.gz",
#'   package = "tphconv"
#' )
#' ```
#'
#' @examples
#' f <- system.file(
#'   "extdata",
#'   "ver1_0_LU_1km_pt_ppl_within_10-20_min.csv.gz",
#'   package = "tphconv"
#' )
#' library(readr)
#' acc <- read_csv(f)
#' head(acc)
#'
#' @source Transport Poverty Hub © European Union, 1995–2025.
#'   The Commission’s reuse policy is implemented by the
#'   [Commission Decision of 12 December 2011 on the reuse of Commission documents](
#'     https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:32011D0833
#'   ). Unless otherwise indicated, content owned by the EU on this website
#'   is licensed under the [Creative Commons Attribution 4.0 International (CC BY 4.0) licence](
#'     https://creativecommons.org/licenses/by/4.0/
#'   ). Reuse is allowed, provided appropriate credit is given and changes are indicated.
#'
#' More information is available at:
#' - [Recommendations for Member States to help tackle transport poverty and promote fair, sustainable mobility](https://urban-mobility-observatory.transport.ec.europa.eu/news-events/news/recommendations-member-states-help-tackle-transport-poverty-and-promote-fair-sustainable-mobility-2025-05-22_en)
#' - [Transport Poverty Hub (requires login with a free EU account)](https://jeodpp.jrc.ec.europa.eu/eu/vaas/voila/render/mobitrans/transportpoverty/TransportPovertyHub.ipynb)
#'
#' @keywords internal datasets
NULL
