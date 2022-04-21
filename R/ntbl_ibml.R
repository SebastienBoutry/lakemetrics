#' Nombre de transects à retenir selon la nome XP T90-328
#'
#' @param sppolygon un objet sf correspondant un polygone de la masse d'eau
#'
#' @return le nombre de transects à tracer le long de la ligne de base -valeur numérique-
#' @export
#'
#' @examples
#' library(lakemetrics)
#' ntb_ibml(lac)
#'
#' @importFrom magrittr %>%
#' @importFrom udunits2 ud.convert
#' @importFrom units set_units
ntb_ibml <- function(sppolygon){
  ##
  nbr_transect_base <- tibble::tibble(Smini=units::set_units(c(0.001,0.05,0.40,0.8,1.6,3.2,6.4,12.8,25.6,51.20),km^2),
                              Smax=units::set_units(c(0.39,0.39,0.79,1.59,3.19,6.39,12.79,25.59,51.19,102.39),km^2),
                              NTBM=c(1,1:9))
  ##
  area_ouput <- units::set_units(udunits2::ud.convert(area_lake(sppolygon) %>%
                                                        as.numeric(),
                                                      "m^2", "km^2"),
                                 "km^2")
  sit <- which((area_ouput > nbr_transect_base$Smini) +  (area_ouput <= nbr_transect_base$Smax)==2)
  ntb_output <- nbr_transect_base$NTBM[sit]+((area_ouput-nbr_transect_base$Smini[sit])/nbr_transect_base$Smini[sit]) %>% as.numeric()
  coeff <- sdi_lake(sppolygon) %>% as.numeric()
  ntbTRUE_output <- coeff*ntb_output
  ##
  return(ntbTRUE_output)
}
