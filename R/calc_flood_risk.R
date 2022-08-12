#' Calculate flood risks using different metrics.
#' @param admin object of class sf.
#' @param pop object of class raster.
#' @param flood object of class raster.
#' @param depth numeric; depth water depth used as a threshold to define flood.
#' @param resample resample flood to match the resolution of pop; default FALSE. If set TRUE, this can take a lot of time depending on the size and resolution of pop.
#' @export
#' @return calc_flood_risk returns a data.frame object with row ID of admin (id), population (pop), population exposed to flood risks (flood_pop), and percent of people exposed to flood risks (flood_pop_s).
#' @examples
#'\dontrun{
#' admin <- st_read(paste0("data/moz_adm_20190607b_shp/moz_admbnda_adm1_ine_20190607.shp"))
#' pop <- raster(paste0("data/MOZ_population_v1_1_gridded.tif"))
#' flood <- raster::raster(paste0("data/fluvial_undefended/FU_1in20.tif"))
#' flood_risk <- calc_flood_risk(admin, pop, flood, 0.1)#this will return the number and percent of people exposed to a flood depth of 10cm for a return period of 20 years.
#'}
#' @import sf sp dplyr exactextractr
#' @importFrom raster raster resample crop mask

calc_flood_risk <- function(admin, pop, flood, depth, resample = FALSE) {
#resample <- FALSE
#depth <- 0.1
#
#
# %>%
#raster::calc(function(x) ifelse(x>=999 | x < depth, 0, 1))
admin$id <- seq(1:dim(admin)[1])
if(identical(resample,TRUE)){
flood <- resample(flood, pop, method="ngb")
}
flood_pop <- flood*pop

admin <- admin %>%
  mutate(pop = exact_extract(pop, ., fun = "sum"),
         flood_pop = exact_extract(flood_pop, ., fun = "sum"),
         flood_pop_s = (flood_pop/pop*100))

admin %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  dplyr::select(id,pop,flood_pop,flood_pop_s)
}
