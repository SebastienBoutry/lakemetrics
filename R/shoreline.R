#' Périmètre de la masse d'eau
#'
#' @param sppolygon un objet sf correspondant un polygone de la masse d'eau
#'
#' @return distance du périmètre en m -numeric-
#' @export
#' @examples
#' library(lakemetrics)
#' data(lac)
#' shoreline_lake(lac)
#'
#' @importFrom magrittr %>%
#' @importFrom sf st_cast st_length
shoreline_lake <- function(sppolygon) {
    if (! class(sppolygon)[1] %in% c("sfc_POLYGON","sf") ) {
      stop("la masse eau n'est pas un objet sf")
    }
  if (!sf::st_geometry_type(sppolygon, by_geometry = TRUE) %>%
      as.character() %in% c("MULTIPOLYGON", "POLYGON")) {
    stop("la masse eau n'est pas de la classe MULTIPOLYGON ou POLYGON")
  }
  shoreline_return <- sppolygon %>%
    sf::st_cast("MULTILINESTRING") %>%
    sf::st_length()
  return(shoreline_return)
}
