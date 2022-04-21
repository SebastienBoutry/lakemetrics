#' Transects et la ligne de base  conformément au protocole d'échantillonnage
#'
#' @param sppolygon un polygone spatialisé -objet sf- masse d'eau
#' @param linemax la longueur la plus grande sur la masse d'eau -objet sf-
#'
#' @return ligne de base et les transects perpendiculaires -objet sf-
#' @export
#'
#' @examples
#' library(lakemetrics)
#' library(ggplot2)
#' data(lac)
#' baseline <- linemax_lake(lac)
#' transects <- transects_ibml(lac,baseline)
#' transects
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate rename filter select
#' @importFrom sf st_geometry_type st_length
transects_ibml<- function(sppolygon,linemax){
  if (! class(sppolygon)[1] %in% c("sfc_POLYGON","sf") ) {
    stop("la masse eau n'est pas un objet sf")
  }
  if (!sf::st_geometry_type(sppolygon, by_geometry = TRUE) %>%
      as.character() %in% c("MULTIPOLYGON", "POLYGON")) {
    stop("la masse eau n'est pas de la classe MULTIPOLYGON ou POLYGON")
  }
  if(is.na(ntb_ibml(sppolygon))){
    distance <-50
  }else{
    distance <- (linemax %>% sf::st_length()/(ntb_ibml(sppolygon))) %>% as.numeric()
  }
  grid_output <- allwidths_lake(sppolygon,linemax,distance) %>%
    dplyr::mutate(classe="transects") %>%
    rbind(linemax %>%
            dplyr::select(-longueur) %>%
            dplyr::rename("geometry"="geom_point") %>%
            dplyr::mutate(classe="ligne de base")) %>%
    dplyr::mutate(longueur=sf::st_length(geometry) %>% as.numeric()) %>%
    dplyr::filter(longueur>0) %>%
    dplyr::select(-longueur)
  return(grid_output)
}
