#' Title
#'
#' @param grid
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom magrittr %>%
#' @importFrom raster rasterFromXYZ crs
rasterizeptsbathy <- function(grid){
  grid  %>%
    select(X, Y, value) %>%
    raster::rasterFromXYZ(crs = raster::crs("+init=epsg:2154"))
}
