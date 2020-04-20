#' Longueur la plus grande sur la masse d'eau
#'
#' @param sppolygon un objet sf correspondant au polygone de la masse d'eau
#'
#' @return la longueur la plus grande sur la masse d'eau -objet sf-
#' @export
#'
#' @examples
#' library(lakemetrics)
#' data(lac)
#' linemax_lake(lac)
#'
#' @importFrom data.table rbindlist
#' @importFrom dplyr mutate group_by
#' @importFrom purrr map2
#' @importFrom sf st_geometry_type st_cast st_geometry st_sf
#' @importFrom tidyr nest
linemax_lake <- function(sppolygon) {
  if (! class(sppolygon)[1] %in% c("sfc_POLYGON","sf") ) {
    stop("la masse eau n'est pas un objet sf")
  }
  if (!st_geometry_type(sppolygon, by_geometry = TRUE) %>%
      as.character() %in% c("MULTIPOLYGON", "POLYGON")) {
    stop("la masse eau n'est pas de la classe MULTIPOLYGON ou POLYGON")
  }
  ##
  lengthmax_return <- NULL
  nest_point_longueur <- NULL
  ##
  points_lake <- sppolygon %>%
    st_cast("MULTILINESTRING") %>%
    st_geometry() %>%
    st_cast("POINT") %>%
    st_sf()
  ##
  nest_point_longueur <- points_lake %>%
    mutate(id = 1:length(geometry)) %>%
    group_by(id) %>%
    nest() %>%
    mutate(longueur = map2(data, points_lake, linemaxpoint_lake))
  ##
  linemaxpoints <- rbindlist(nest_point_longueur$longueur)
  ##
  lengthmax_return <- lengthmax_allpoints_lake(linemaxpoints)
  return(lengthmax_return[1, ])
}
