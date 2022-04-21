#' Profondeur d’influence des vagues (Depth of Wave Base)
#'
#' @param area aire de la masse d'eau en km²
#'
#' @return Cette valeur correspond à la profondeur maximale d’influence de l’énergie des vagues sur les sédiments de fond pour une surface de plan d’eau (Hakanson 2005). Ce paramètre doit servir à mieux décrire une partie du phénomène de remise en suspension des sédiments qui peut avoir des effets sur la physico-chimie du plan d’eau, la chaine trophique ou encore la répartition de la faune  et/ou la flore. Exprimée en mètre (m)
#' @export
#' @references Hakanson 2005
#'
#' @examples
#' library(lakemetrics)
#' depthwavebase_lake(area=50000)
#'
depthwavebase_lake <- function(area){
  if (! class(area) %in% c("numeric") ) {
    stop("l'aire de la masse d'eau n'est pas au bon format")
  }
  dwb <- (45.7*sqrt(area))/(21.4+sqrt(area))
  return(dwb)
}
