#' Classe dans l'ordre décroissant les lacs d'un même ensemble d'une masse d'eau selon la surface en eau
#'
#' @param sppolygon un objet sf correspondant un polygone de la masse d'eau
#'
#' @return le même polygone spatialisé avec deux variables en plus la surface en eau -area- et une variable ordinale qui classe dans l'ordre décroissant les lacs selon leurs surface.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate arrange desc
#' @importFrom sf st_geometry_type st_geometry st_cast st_sf st_area
class_lakes <- function(sppolygon) {
  if (! class(sppolygon)[1] %in% c("sfc_POLYGON","sf") ) {
    stop("la masse eau n'est pas un objet sf")
  }
  if (!sf::st_geometry_type(sppolygon, by_geometry = TRUE) %>%
    as.character() %in% c("MULTIPOLYGON", "POLYGON")) {
    stop("la masse eau n'est pas de la classe MULTIPOLYGON ou POLYGON")
  }
  sp_polygon_return <- sppolygon %>%
    sf::st_geometry() %>%
    sf::st_cast("MULTIPOLYGON") %>%
    sf::st_cast("POLYGON") %>%
    sf::st_sf() %>%
    dplyr::mutate(id = 1:length(geometry)) %>%
    dplyr::mutate(area = sf::st_area(geometry)) %>%
    dplyr::arrange(dplyr::desc(area)) %>%
    dplyr::mutate(selection = 1:length(area))
  return(sp_polygon_return)
}
