#' Calculate flood risks using different metrics based on two different flood raster layers.
#' @param admin object of class sf.
#' @param pop object of class raster.
#' @param flood object of class raster; flood raster with an expected depth of water for a certain return period.
#' @param flood2 object of class raster; the second flood raster with an expected depth of water for a certain return period.
#' @param depth numeric; depth water depth used as a threshold to define flood.
#' @param resample resample flood to match the resolution of pop; default FALSE. If set TRUE, this can take a lot of time depending on the size and resolution of pop.
#' @export
#' @return `calc_flood_risk2()` returns a data.frame object with row ID of admin (id), population (pop), population exposed to flood risks (flood_pop), and percent of people exposed to flood risks (flood_pop_s).
#' @examples
#'\dontrun{
#' admin <- st_read(paste0("data/moz_adm_20190607b_shp/moz_admbnda_adm1_ine_20190607.shp"))
#' pop <- raster(paste0("data/MOZ_population_v1_1_gridded.tif"))
#' flood <- raster::raster(paste0("data/fluvial_undefended/FU_1in20.tif"))
#' flood2 <- raster::raster(paste0("data/pluvial/P_1in20.tif"))
#' flood_risk <- calc_flood_risk2(admin, pop, flood, flood2, 0.1)
#'}
#' @import sf dplyr
#' @importFrom raster raster crop mask
#' @importFrom exactextractr exact_extract

calc_fathom_flood_risk2 <- function(admin, pop, flood, flood2, depth, resample = FALSE) {
  flood <- flood %>% raster::calc(function(x) ifelse(x>=999 | x < depth, 0, 1))
  flood2 <- flood2 %>% raster::calc(function(x) ifelse(x>=999 | x < depth, 0, 1))
  flood <- flood + flood2
  flood <- flood %>% raster::calc(function(x) ifelse(x>=1, 1, 0))
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
