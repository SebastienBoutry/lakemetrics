#' Largeur moyenne calculée selon l'aire et la longueur maximale de la masse d'eau
#'
#' @param sppolygon un polygone spatialisé -objet sf- masse d'eau
#'
#' @return Valeur de la largeur moyenne de la masse d'eau selon la formule
#' @export
#'
#' @examples
#' library(lakemetrics)
#'
#' @importFrom magrittr %>%
#' @importFrom sf st_length
meanwidthcalc_lake <- function(sppolygon) {
  if (! class(sppolygon)[1] %in% c("sfc_POLYGON","sf") ) {
    stop("la masse eau n'est pas un objet sf")
  }
  if (!sf::st_geometry_type(sppolygon, by_geometry = TRUE) %>%
      as.character() %in% c("MULTIPOLYGON", "POLYGON")) {
    stop("la masse eau n'est pas de la classe MULTIPOLYGON ou POLYGON")
  }
  meanwidthcalc_return <- area_lake(sppolygon) / (linemax_lake(sppolygon) %>% sf::st_length())
  return(meanwidthcalc_return)
}
