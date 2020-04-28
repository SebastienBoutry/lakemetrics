#' Surface en eau d'un lac
#'
#' @param sppolygon un polygone spatialisé -objet sf- masse d'eau
#'
#' @return valeur de la surface en eau en m²
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom sf st_geometry_type st_area
area_lake <- function(sppolygon) {
  if (! class(sppolygon)[1] %in% c("sfc_POLYGON","sf") ) {
    stop("la masse eau n'est pas un objet sf")
  }
  if (!(sf::st_geometry_type(sppolygon, by_geometry = TRUE)  %>%
        as.character() %in% c("MULTIPOLYGON","POLYGON"))) {
    stop("la masse eau n'est pas de la classe MULTIPOLYGON ou POLYGON")
  }
  area_return <- sppolygon %>%
    sf::st_area()
  return(area_return)
}




