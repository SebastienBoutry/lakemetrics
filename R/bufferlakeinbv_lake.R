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
  if (!st_geometry_type(sppolygon, by_geometry = TRUE) %>%
      as.character() %in% c("MULTIPOLYGON", "POLYGON")) {
    stop("la masse eau n'est pas de la classe MULTIPOLYGON ou POLYGON")
  }
  buffer_output <- st_make_valid(
    st_difference(
    sppolygon %>%
      st_cast("MULTILINESTRING") %>%
      st_buffer(dist = distance) %>%
      st_geometry(),
    sppolygon %>%
      st_geometry()
  )) %>%
    st_intersection(
      st_make_valid(bv %>%
      st_geometry())
      )
  return(buffer_output)
}
