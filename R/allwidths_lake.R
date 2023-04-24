#' Toutes les droites orthogonales à la ligne de base -longueur maximale- équidistant de la distance en m
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
#' data(lac)
#' linemax <- linemax_lake(sppolygon=lac)
#' allwidths_lake(sppolygon=lac, linemax=linemax, distance=100)
#'
#' @importFrom magrittr %>%
#' @importFrom data.table rbindlist
#' @importFrom dplyr filter group_by mutate
#' @importFrom purrr pmap
#' @importFrom sf st_geometry st_bbox st_sf st_intersection
#' @importFrom tidyr nest
allwidths_lake <- function(sppolygon,linemax, distance) {
  if (! class(sppolygon)[1] %in% c("sfc_POLYGON","sf") ) {
    stop("la masse eau n'est pas un objet sf")
  }
  if (!sf::st_geometry_type(sppolygon, by_geometry = TRUE) %>%
      as.character() %in% c("MULTIPOLYGON", "POLYGON")) {
    stop("la masse eau n'est pas de la classe MULTIPOLYGON ou POLYGON")
  }
  ##
  # linemax <- linemax_lake(sppolygon)
  plan_eau <- sppolygon
  plan_eau_select <- class_lakes(sppolygon) %>%
    dplyr::filter(selection == 1)
  # for a point in the base line
  tibble_perpendiculars <- points_linemax(linemax, distance)

  # perpendiculars_tibble <- tibble_perpendiculars  %>%
  #   as_tibble() %>%
  #   rowwise() %>%
  #   # group_by(id) %>%
  #   # nest() %>%
  #   mutate(data_line_perpendicular = map2(X,Y,
  #     lineperdendicular))
  # data_line_perpendicular <- perpendiculars_tibble$data_line_perpendicular
  data_line_perpendicular <- list()
  for (i in 1:nrow(tibble_perpendiculars)) {
    data_line_perpendicular[[i]] <- lineperdendicular(sppolygon,
                                                      linemax,
                                                      tibble_perpendiculars$X[i],
                                                      tibble_perpendiculars$Y[i])
  }
  plan_eau_select <- plan_eau_select %>%
    sf::st_geometry() |>
    sf::st_transform(2154)

  perpendiculars <- bind_rows(data_line_perpendicular) %>%
    dplyr::mutate(id = 1:length(geometry)) %>%
    sf::st_sf() %>%
    sf::st_geometry() %>%
    sf::st_transform(2154) |>
    sf::st_intersection(plan_eau_select) %>%
    sf::st_sf()
  return(perpendiculars)
}
