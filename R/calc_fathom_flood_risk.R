#' Calculate flood risks using different metrics.
#' @param admin object of class sf.
#' @param pop object of class raster.
#' @param flood object of class raster.
#' @param depth numeric; depth water depth used as a threshold to define flood.
#' @param resample resample flood to match the resolution of pop; default FALSE. If set TRUE, this can take a lot of time depending on the size and resolution of pop.
#' @export
#' @return `calc_flood_risk2()` returns a data.frame object with row ID of admin (id), population (pop), population exposed to flood risks (flood_pop), and percent of people exposed to flood risks (flood_pop_s).
#' @examples
#'\dontrun{
#' flood <- raster::raster(paste0("inst/extdata/fluvial_undefended/FU_1in20.tif"))
#' flood_risk <- calc_flood_risk(sf, pop, flood, 0.1)
#'}
#' @import sf dplyr
#' @importFrom raster raster crop mask
#' @importFrom exactextractr exact_extract

calc_fathom_flood_risk <- function(admin, pop, flood, depth, resample = FALSE) {
flood <- flood %>% raster::calc(function(x) ifelse(x>=999 | x < depth, 0, 1))
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
