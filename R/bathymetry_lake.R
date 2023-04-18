#' Bathymmétrie de la masse d'eau au format raster
#'
#' @param sppolygon un objet sf correspondant un polygone de la masse d'eau
#' @param pts_bathy une collection de points -objet sf- de dimension XYZ  ou Z : profondeur
#' @param cellsize taille du pixel par défaut 10 m
#'
#' @return Bathymmétrie de la masse d'eau au format raster ; plusieurs algorithmes pour la création de la bathymétrie sont testés.
#' \describe{
#'   \item{IDW} {Inverse Distance Weighting a pour principe que chaque point spatialisé de profondeur est un point relié autres points environnants, le poids de relation entre les points varie selon la distance entre les points (relation forte entre deux points proche et relation faible pour deux points distants).}
#'   \item{TPRS} {Thin Plate Regression Spline}
#'   \item{} {Soap Film Smooth}
#' }
#'
#' Le choix se fait juste par l'écart le plus petit entre les pts_bathy et le modèle -en valeur absolue-. Pour cela on utilise la fonction diffrasterpts.
#'
#' @export
#'
#' @examples
#' library(lakemetrics)
#' bathymetry_lake(lac,bathy)
#'
#' @references https://fishandwhistle.net/post/2019/bathymetry-lake-volume-estimation-using-r/
#' @references https://fromthebottomoftheheap.net/2016/03/27/soap-film-smoothers/
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select rename mutate distinct_all filter pull group_by bind_rows
#' @importFrom gstat gstat
#' @importFrom mgcv gam
#' @importFrom purrr map
#' @importFrom sf st_geometry_type st_coordinates st_zm st_cast st_as_sf st_make_grid st_contains st_intersects st_buffer st_drop_geometry
#' @importFrom stats predict
#' @importFrom tidyr pivot_longer nest
bathymetry_lake<- function(sppolygon,pts_bathy,cellsize=10){
  if (! class(sppolygon)[1] %in% c("sfc_POLYGON","sf","sfc_MULTIPOLYGON") ) {
    stop("la masse eau n'est pas un objet sf")
  }
  if (!sf::st_geometry_type(sppolygon, by_geometry = TRUE) %>%
      as.character() %in% c("MULTIPOLYGON", "POLYGON")) {
    stop("la masse eau n'a pas d du type geom MULTIPOLYGON ou POLYGON")
  }
  if (! class(pts_bathy)[1] %in% c("sf") ) {
    stop("l'objet pts_bathy n'est pas un objet sf")
  }
  if (sf::st_coordinates(pts_bathy) %>% ncol()!=3) {
    stop("l'objet pts_bathy n'est pas un objet à trois dimensions XYZ")
  }
  ##
  ##
  pts_bathy_modify <- pts_bathy %>%
    cbind(.,sf::st_coordinates(.)) %>%
    dplyr::select(-X,-Y) %>%
    dplyr::rename("depth"="Z") %>%
    dplyr::mutate(source="measured") %>%
    sf::st_zm()
  ##
  ##
  boundary_points <- sf::st_cast(sppolygon, "POINT") %>%
    sf::st_as_sf() %>%
    dplyr::rename("geometry"="x") %>%
    dplyr::mutate(source= "boundary",depth = 0) %>%
    dplyr::select(source,depth,geometry)
  ##
  depths <- dplyr::bind_rows(boundary_points,  pts_bathy_modify) %>%
    dplyr::select(source,depth,geometry) %>%
    cbind(., sf::st_coordinates(.)) %>%
    dplyr::distinct_all()
  ##
  grid <- sf::st_make_grid(sppolygon,
                       cellsize = c(cellsize, cellsize),
                       what = "centers") %>%
    sf::st_as_sf() %>%
    st_intersection(sppolygon) %>%
    # dplyr::filter(sf::st_contains(sppolygon,
    #                               .,
    #                               sparse = FALSE)) %>%
    cbind(., sf::st_coordinates(.)) %>%
    dplyr::rename("geometry"="x")
  ##

  # Inverse Distance Weighting (IDW)
  fit_gstat <- gstat::gstat(
    formula = depth ~ 1,
    data = as(depths, "Spatial"),
    nmax = 10, nmin = 3,
    set = list(idp = 0.5)
  )
  grid$IDW <- stats::predict(fit_gstat, newdata = as(grid, "Spatial")) %>%
    sf::st_as_sf() %>%
    dplyr::pull(1)
  # Thin Plate Regression Spline (TPRS)
  fit_gam_reml <- mgcv::gam(depth ~ s(X, Y, k = 60), data = depths, method = "REML")
  grid$TPRS <- stats::predict(fit_gam_reml, newdata = grid, type = "response")
  ## Soap Film Smooth
  boundary_coords <- sf::st_coordinates(sppolygon)
  gam_bound  <- list(
    list(
      X = boundary_coords[-1, "X"],
      Y = boundary_coords[-1, "Y"],
      f = rep(0, nrow(boundary_coords))
    )
  )

  knot_points <- sf::st_make_grid(
    sppolygon,
    n = c(10, 10),
    what = "centers"
  ) %>%
    sf::st_as_sf() %>%
    st_intersection(sppolygon) %>%
    # dplyr::filter(sf::st_contains(sppolygon, x, sparse = FALSE)) %>%
    st_difference(sppolygon %>% sf::st_cast("LINESTRING") %>% sf::st_buffer(10)) %>%
    # dplyr::filter(
    #   !sf::st_intersects(
    #     sppolygon %>% sf::st_cast("LINESTRING") %>% sf::st_buffer(10),
    #     x,
    #     sparse = FALSE
    #   )
    # ) %>%
    cbind(., sf::st_coordinates(.))

  fit_gam_soap <- mgcv::gam(
    depth ~ s(X, Y, bs = "so", xt = list(bnd = gam_bound)),
    data = depths %>%
      dplyr::filter(source == "measured") %>%
      st_intersection(sppolygon),
      # dplyr::filter(sf::st_contains(sppolygon, geometry, sparse = FALSE)),
    method = "REML",
    knots = knot_points
  )
  grid$GAM_Soap <- stats::predict(fit_gam_soap, newdata = grid, type = "response")
  ##
  ##
  grid_nested <- grid %>%
    sf::st_drop_geometry() %>%
    tidyr::pivot_longer(c("IDW","TPRS","GAM_Soap")) %>%
    dplyr::group_by(name) %>%
    tidyr::nest() %>%
    dplyr::mutate(depth_raster_output=purrr::map(data,rasterizeptsbathy))
  diff <- NULL
  for(i in grid_nested$name){
    depth_raster <- grid_nested %>% dplyr::filter(name==i) %>% dplyr::pull(depth_raster_output)
    diff <- c(diff,diffrasterpts(depth_raster[[1]] , pts_bathy))
  }
  names(diff) <- grid_nested$name
  best_select <- which.min(diff)
  ##
  best_grid <- grid_nested$data[[3]] %>%
    sf::st_as_sf(coords=c("X","Y"),crs=2154) %>%
    cbind(., sf::st_coordinates(.))
  return(best_grid)
}
