#' Ligne perpendiculaire selon un point sur l
#'
#' @param sppolygon un polygone spatialisé -objet sf- masse d'eau
#' @param linemax objet sf correspondant à la longueur maximale sur la masse d'eau -linestring-
#' @param coord coordonnées du point d'intersection sur la longueur maximale de la masse d'eau
#'
#' @return
#' @export
#'
#' @examples
#' library(lakemetrics)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr tibble mutate group_by summarize as_tibble
#' @importFrom sf st_as_sf st_cast
lineperdendicular <- function(sppolygon,linemax,X,Y) {
  if (! class(sppolygon)[1] %in% c("sfc_POLYGON","sf") ) {
    stop("la masse eau n'est pas un objet sf")
  }
  if (!sf::st_geometry_type(sppolygon, by_geometry = TRUE) %>%
      as.character() %in% c("MULTIPOLYGON", "POLYGON")) {
    stop("la masse eau n'est pas de la classe MULTIPOLYGON ou POLYGON")
  }
  # coord <- coord %>%
  #   purrr::pmap(~c(...))
  ##
  vec <- sppolygon %>%
    sf::st_geometry() %>%
    sf::st_bbox()
  x1 <- vec["xmin"]
  x2 <- vec["xmax"]
  ##
  data_coord <- linemax %>%
    sf::st_geometry() %>%
    sf::st_cast("POINT") %>%
    sf::st_coordinates() %>%
    dplyr::as_data_frame() %>%
    dplyr::arrange(dplyr::desc(X))
  ## coefficients
  a <- stats::coef(stats::lm(Y ~ X, data = data_coord))["X"]
  a0 <- as.vector(stats::coef(stats::lm(Y ~ X, data = data_coord))["(Intercept)"])
  b <- as.vector(-1 / a)
  # b0 <- coord$Y - (b * coord$X)
  b0 <- Y - (b * X)
  ##
  perpendicular_return <- dplyr::tibble(X = seq(
    from = x1,
    to = x2,
    length.out = 100
  )) %>%
    dplyr::mutate(Y = X * b + b0) %>%
    dplyr::as_tibble() %>%
    sf::st_as_sf(coords = c("X", "Y"), crs = 2154) %>%
    dplyr::mutate(id = 1) %>%
    dplyr::group_by(id) %>%
    dplyr::summarize() %>%
    sf::st_cast("LINESTRING")
  return(perpendicular_return)
}

