#' Comparaison des longueurs pour chaque pont afin de déterminer la longueur maximale de la masse d'eau
#'
#' @param linemaxpoints un objet sf -linestring- ensemble de longueur maximale pour chaque points de la masse d'eau
#'
#' @return la longueur maximale de la masse d'eau en obejt sf -linestring-
#' @export
#'
#' @examples
#' library(lakemetrics)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter distinct
#' @importFrom sf st_sf
lengthmax_allpoints_lake <- function(linemaxpoints) {
    # if (class(point_sf)[1] != "sf") {
    #   stop("point_sf n'est pas un objet sf")
    # }
    # if (!st_geometry_type(point_sf, by_geometry = TRUE) %>%
    #     as.character() %in% c("POINT")) {
    #   stop("la donnée d'entrée point_sf n'est pas de la classe POINT")
    # }
  ##
  linemax_return <- linemaxpoints %>%
    dplyr::filter(longueur == max(longueur)) %>%
    sf::st_sf() %>%
    dplyr::distinct()
  return(linemax_return)
}
