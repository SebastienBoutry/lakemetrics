#' Longueur maximale pour un point spatialisé de la masse d'eau
#'
#' @param point_sf un objet sf correspondant à un point spatialisé de la masse d'eau
#' @param points_lake un objet sf -mulitpoint- correspondant à tout les points définissant  la masse d'eau
#'
#' @return
#' @export
#'
#' @examples
#' library(lakemetrics)
#'
#' @importFrom  magrittr %>%
#' @importFrom dplyr mutate filter rename
#' @importFrom sf st_nearest_points st_sf st_length
linemaxpoint_lake <- function(point_sf, points_lake) {
  # if (class(points_lake)[1] != "sf") {
  #   stop("points_lake n'est pas un objet sf")
  # }
  # if (!st_geometry_type(points_lake, by_geometry = TRUE) %>%
  #     as.character() %in% c("MULTIPOINT")) {
  #   stop("points_lake n'est pas de la classe MULTIPOINT")
  # }
  ##
  # if (class(point_sf)[1] != "sf") {
  #   stop("point_sf n'est pas un objet sf")
  # }
  # if (!st_geometry_type(point_sf, by_geometry = TRUE) %>%
  #     as.character() %in% c("POINT")) {
  #   stop("point_sf n'est pas de la classe POINT")
  # }
  ##
  sf::st_nearest_points(
    point_sf,
    points_lake
  ) %>%
    sf::st_sf() %>%
    dplyr::mutate(longueur = sf::st_length(geometry)) %>%
    dplyr::filter(longueur == max(longueur)) %>%
    dplyr::rename("geom_point" = "geometry")
}
