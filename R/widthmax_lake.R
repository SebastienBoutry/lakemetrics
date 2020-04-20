#' Largeur maximale de la masse d'eau selon la ligne de base -longueur maximale-
#'
#' @param sppolygon un polygone spatialisé -objet sf- masse d'eau
#' @param distance intervalle entre deux points le long de la longueuer maximale de la masse d'eau -distance en m-
#' @param linemax  longueur la plus grande sur la masse d'eau -objet sf-
#'
#' @return
#' @export
#'
#' @examples
#' library(lakemetrics)
#' @importFrom dplyr mutate filter
#' @importFrom sf st_length
widthmax_lake <- function(sppolygon,linemax,distance) {
  if (class(sppolygon)[1] %in% c("sfc_POLYGON","sf") ) {
    stop("la masse eau n'est pas un objet sf")
  }
  if (!st_geometry_type(sppolygon, by_geometry = TRUE) %>%
      as.character() %in% c("MULTIPOLYGON", "POLYGON")) {
    stop("la masse eau n'est pas de la classe MULTIPOLYGON ou POLYGON")
  }
  perpendiculars <- allwidths_lake(sppolygon,distance)  %>%
    mutate(largeur = st_length(geometry)) %>%
    mutate(selection = ifelse(largeur == max(largeur), "max", "no"))
  return(perpendiculars %>%
           filter(selection == "max"))
}
