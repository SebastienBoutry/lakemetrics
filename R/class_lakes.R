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
  if (!st_geometry_type(sppolygon, by_geometry = TRUE) %>%
    as.character() %in% c("MULTIPOLYGON", "POLYGON")) {
    stop("la masse eau n'est pas de la classe MULTIPOLYGON ou POLYGON")
  }
  sp_polygon_return <- sppolygon %>%
    st_geometry() %>%
    st_cast("MULTIPOLYGON") %>%
    st_cast("POLYGON") %>%
    st_sf() %>%
    mutate(id = 1:length(geometry)) %>%
    mutate(area = st_area(geometry)) %>%
    arrange(desc(area)) %>%
    mutate(selection = 1:length(area))
  return(sp_polygon_return)
}
