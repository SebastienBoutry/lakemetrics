utils::globalVariables(c("area","geometry"))
usethis::use_package("magrittr")
usethis::use_package("sf")
usethis::use_package("dplyr")
usethis::use_package("base")
usethis::use_package("data.table")
usethis::use_package("purrr")
usethis::use_package("tidyr")
usethis::use_package("units")
usethis::use_package("udunits2")
usethis::use_package("gstat")
usethis::use_package("mgcv")
usethis::use_package("raster")
utils::globalVariables(c("area","geometry"))
lac <- read_sf("/home/sebastien.boutry/Documents/Stage/Sorties/Chauvet_lake.shp") %>% st_geometry()
bv <- read_sf("/home/sebastien.boutry/Documents/Stage/Sorties/BV_Chauvet_lake.shp") %>% st_geometry()
bathy <- bathy_LDC63
usethis::use_data(bathy)
# use_data(lac)
devtools::document() # mettre à jour le namespace


nbr_transect_base <- tibble(Smini=units::set_units(c(0.05,0.40,0.8,1.6,3.2,6.4,12.8,25.6,51.20),km^2),
                            Smax=units::set_units(c(0.39,0.79,1.59,3.19,6.39,12.79,25.59,51.19,102.39),km^2),
                            NTBM=1:9)
use_data(nbr_transect_base)


  # cd monpackage
  # git add .
  # git commit -m "nouveau commit"
  # git push -u origin master
