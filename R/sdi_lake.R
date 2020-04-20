#' Indice de complexité des rives -SDI-
#'
#' @param sppolygon un polygone spatialisé -objet sf- masse d'eau
#'
#' @return indice de complexité des rives
#' @export
#'
#' @examples
#' library(lakemetrics)
sdi_lake <- function(sppolygon) {
  if (! class(sppolygon)[1] %in% c("sfc_POLYGON","sf") ) {
    stop("la masse eau n'est pas un objet sf")
  }
  if (!st_geometry_type(sppolygon, by_geometry = TRUE) %>%
      as.character() %in% c("MULTIPOLYGON", "POLYGON")) {
    stop("la masse eau n'est pas de la classe MULTIPOLYGON ou POLYGON")
  }
  SDI <- shoreline_lake(sppolygon) / (2 * sqrt(pi * area_lake(sppolygon))) %>% as.numeric()
  return(SDI)
}
