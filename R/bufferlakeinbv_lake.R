#' Création d'une zone tampon autour de la masse d'eau en prenant en compte le découpage du Bassin versant.
#'
#' @param sppolygon un objet sf correspondant un polygone de la masse d'eau
#' @param bv polygone du bassin versant de la masse d'eau -un objet sf-
#' @param distance distance de la zone tampon valeur valeur numérique en m
#'
#' @return  un polygone -objet sf- de la zone tampon
#' @export
#'
#' @examples
#' library(lakemetrics)
#' data(lac)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom sf st_difference st_buffer st_geometry st_intersection st_make_valid
lakebufferinbv_lake <- function(sppolygon, bv, distance) {
  if (! class(sppolygon)[1] %in% c("sfc_POLYGON","sf") ) {
    stop("la masse eau n'est pas un objet sf")
  }
  if (!sf::st_geometry_type(sppolygon, by_geometry = TRUE) %>%
      as.character() %in% c("MULTIPOLYGON", "POLYGON")) {
    stop("la masse eau n'est pas de la classe MULTIPOLYGON ou POLYGON")
  }
  buffer_output <- sf::st_make_valid(
    sf::st_difference(
    sppolygon %>%
      sf::st_cast("MULTILINESTRING") %>%
      sf::st_buffer(dist = distance) %>%
      sf::st_geometry(),
    sppolygon %>%
      sf::st_geometry()
  )) %>%
    sf::st_intersection(
      sf::st_make_valid(bv %>%
      sf::st_geometry())
      )
  return(buffer_output)
}
