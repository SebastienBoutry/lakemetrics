#' Différence de profondeur entre les pts_bathy et le modèle
#'
#' @param depth_raster raster XY value : profondeur
#' @param pts_bathy une collection de points -objet sf- de dimension XYZ  ou Z : profondeur
#'
#' @return
#' @export
#'
#' @examples
#' library(lakemetrics)
#'
#' @importFrom magrittr %>%
#' @importFrom raster extract
diffrasterpts <- function(depth_raster, pts_bathy) {
  if (!class(pts_bathy)[1] %in% c("sf")) {
    stop("l'objet pts_bathy n'est pas un objet sf")
  }
  if (sf::st_coordinates(pts_bathy) %>% ncol() != 3) {
    stop("l'objet pts_bathy n'est pas un objet à trois dimensions XYZ")
  }
  ##
  pts_bathy_modify <- pts_bathy %>%
    cbind(., sf::st_coordinates(.)) %>%
    dplyr::select(-X, -Y) %>%
    dplyr::rename(depth = Z) %>%
    sf::st_zm()
  ##
  raster::extract(depth_raster, # raster layer
    pts_bathy_modify, # SPDF with centroids for buffer
    buffer = 15, # buffer size, units depend on CRS
    fun = mean, # what to value to extract
    df = TRUE
  ) %>% # return a dataframe? %>%
    dplyr::mutate(depth = pts_bathy_modify %>%
      sf::st_drop_geometry() %>%
      dplyr::pull(depth)) %>%
    dplyr::mutate(diff = abs(depth - value)) %>%
    dplyr::pull(diff) %>%
    sum(na.rm = TRUE) / nrow(pts_bathy)
}
