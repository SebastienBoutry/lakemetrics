#' Répartition des points équidistants le long de longueur maximale de la masse d'eau
#'
#' @param linemax objet sf correspondant à la longueur maximale sur la masse d'eau -linestring-
#' @param distance intervalle entre deux points le long de la longueuer maximale de la masse d'eau -distance en m-
#'
#' @return
#' @export
#'
#' @examples
#' library(lakemetrics)
#' @importFrom dplyr as_data_frame arrange desc mutate
#' @importFrom sf st_geometry st_cast st_coordinates st_segmentize
#' @importFrom stats coef lm
#' @importFrom units set_units
points_linemax <- function(linemax, distance) {
  ##
  # data_coord <- linemax %>%
  #   st_geometry() %>%
  #   st_cast("POINT") %>%
  #   st_coordinates() %>%
  #   as_data_frame() %>%
  #   arrange(desc(X))
  # ## coefficients
  # a <- coef(lm(Y ~ X, data = data_coord))["X"]
  # a0 <- as.vector(coef(lm(Y ~ X, data = data_coord))["(Intercept)"])
  # b <- as.vector(-1 / a)
  # b0 <- data_coord$Y - (b * data_coord$X)
  ##
  tibble_perpendiculars <- st_segmentize(
    linemax %>%
      st_geometry(),
    set_units(distance, m)
  ) %>%
    st_cast("POINT") %>%
    st_coordinates() %>%
    as_data_frame() %>%
    mutate(id = 1:length(X))
    # mutate(
    #   b = b,
    #   b0 = Y - (b * X)
    # )
  return(tibble_perpendiculars)
}
