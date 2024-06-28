# library (qs)
# library (terra)
# library (tidyterra)
#
# rmodel <- terra::rast(xmin = -180,xmax=180,ymin=-90,ymax=90,res=0.5)
# recoregions <- rast ('~/RS/Data/ecoregions/ecoreg2017_ecoid_1km.tif')
# recoregions <- terra::project (recoregions,rmodel,method='near')
#
# sp_cells <- qread ('/Users/pepserradiaz/RS/Projects/exposure2023/presenceAbsFuncs/sp_cellids/Tantilla_shawi.qs')
# buffdist= 500000
# habitat_mask <- NULL
# habitat_mask <- rnaturalearth::ne_download(type='land',category = 'physical', scale=10,returnclass = 'sf')
# habitat_mask <- terra::rasterize(vect(habitat_mask),rmodel,touches=T)
# plot (habitat_mask)
# method='buffer'
# method='adjacent'
# metjod='ecoregions'
# buf_dist

### OBTAIN ABSENCES FROM A RASTER MODEL AND SP IDS
#' @title Obtain absence from species id and a raster model
#' @param sp_cells data.frame or alike with the column cell specifying cell ids.
#' @param weights_column character. optional. column with the name specifying the weights in the presences
#' @param rmodel spatRaster of reference for the species cellid
#' @param ras_ecoregions spatRaster where each number is an ecoregion ID
#' @param buffdist numeric. Typically m or the unit of the projection of the raster. Defaut to 500 km
#' @param habitat_mask spatRaster where NA values indicate out of habitat (e.g. sea, land, etc.)
#' @param partial_absences logical. Default to T. Should partial absences be included (e.g where species presences with weights <1 are used)
#' @param include_presences logical. Should presencers be incorporated in th output? Default T
#' @author Josep M Serra Diaz
#' @keywords internal
#' @export
#' @examples
#'

cheap_absences <- function (sp_cells,rmodel,weights_column=NULL,
                            ras_ecoregions=NULL,buffdist=500000,habitat_mask=NULL,
                           partial_absences=T,include_presences=T,
                           method='adjacent'){
  
  cid <- sp_cells$cell
  rsp = rast (rmodel)
  values(rsp) <- 1:terra::ncell(rsp)
  rsp <- terra::subst(rsp,from=cid,
                      to=rep(1,length(cid)),
                      others=NA)

  # METHODS ====
  #method adjacent
  if (method=='adjacent'){
    abs <- terra::adjacent(rsp,cells=cid,directions='16',include=F) |>
      as.numeric() |>
      unique()
    abs <- abs [!abs%in%cid]
  }
  if (method=='buffer'){
    #method buffer
    rsp_bufdist <- terra::buffer(rsp,width=buffdist) *1
    rsp_bufdist <- terra::subst (rsp_bufdist,from=1,to=1,others=NA)
    abs <- tidyterra::as_tibble(rsp_bufdist,na.rm=T,cells=T,xy=T) |>
      dplyr::select(!cell %in% cid)
  }
  if (method=='ecoregions'){
    #method ecoregions
    if (!terra::compareGeom(rmodel,recoregions)) stop('not the same geometry for raster of ecoregions and raster model')
    sp_ecoreg <- terra::extract(x = recoregions,y = cid) |> dplyr::pull(1)
    rsp_bufdist <- terra::buffer(rsp,width=1000000) *1
    rsp_bufdist <- terra::subst (rsp_bufdist,from=1,to=1,others=NA)
    recoreg_buf <- recoregions * rsp_bufdist
    target_cid <- recoreg_buf |> tidyterra::as_tibble(cells=T,na.rm=T)
    names(target_cid) <- c('cell','ecoid')
    target_cid_specoreg <- target_cid |>
      filter(ecoid %in% sp_ecoreg) |>
      pull(cell)
    adjacentEcoreg_cid <- terra::adjacent (recoreg_buf,
                                           target_cid_specoreg,
                                           direction='queen',
                                           include=T) |> as.numeric() |> unique()
    abs <- adjacentEcoreg_cid[!adjacentEcoreg_cid %in% cid]
  }

  # output absences
  out_abs <- dplyr::tibble(cell=abs,n=1)

  # INCLUDE PARTIAL WEIGHTS? ====
  if (partial_absences){
    #include partial weights
    partial_weights <- 1 - sp_cells |> pull(4)
    partial_abs <- dplyr::tibble(cell=cid,n=partial_weights)
    out_abs <- dplyr::bind_rows(partial_abs,out_abs)
  }
  # INCLUDE PRESENCES? ====
  if (include_presences){
    sp_cells$pa <- 1
    out_abs$pa <- 0
    out_abs <- dplyr::bind_rows(sp_cells,out_abs) |>
      dplyr::select(cell,n,pa)
  }
  # MASK BY HABITAT? ====
  if (!is.null(habitat_mask)) {
    na_vals <- terra::extract(habitat_mask,out_abs$cell) |>
      is.na() |> as.vector()
    out_abs <- out_abs[!na_vals,]
  }
  # RETURN ====
  return(out_pa)
}

