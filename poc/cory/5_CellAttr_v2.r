# 
# store species attributes from each data source and a cumulative one with all attributes. 
#=====================================================
# get ecoregion associations of each pixel: eco, realm, biome, protected
e=st_read('/Users/ctg/Dropbox/Projects/2023_Exposure/Ecoregions2017/Ecoregions2017.shp')
# get key metadata like names that we don't want to carryeverywher
e.d=e %>% st_drop_geometry
saveRDS(e.d,file='/Users/ctg/Dropbox/Projects/2023_Exposure/Ecoregions2017/ecoregionMetadata.rds')

# # I think these need to be facors to rasterize right. NOPE, they shift values when you do that.
# e=e %>% mutate(REALM1=as.factor(as.numeric(as.factor(REALM)))) %>% dplyr::select(-SHAPE_LENG,-SHAPE_AREA,-OBJECTID) %>% mutate(BIOME_NUM=as.factor(BIOME_NUM), ECO_ID=as.factor(ECO_ID), NNH=as.factor(NNH))

# land=!is.na(rast('/Users/ctg/Dropbox/Projects/2023_Exposure/Summaries/amphibians_t2m-q099_99p.tif')[[1]]) 
# values(land)[values(land)==0]=NA
template=stack('/Users/ctg/Documents/SDMs/Exposure_2023/climDeparture/departure_tp_ref_1940-2022_proj_2023.tif')[[6]]>0
values(template)[!is.na(values(template))]=0

eco=e %>% dplyr::select(ECO_ID,NNH,BIOME_NUM) # maybe only need eco_ID
e.r=st_rasterize(eco,template=st_as_stars(template),options = "ALL_TOUCHED=TRUE") %>% rast
names(e.r)=names(eco)[1:3]
#e.r1a=mask(e.r1,land)
terra::writeRaster(e.r,file='/Users/ctg/Dropbox/Projects/2023_Exposure/Ecoregions2017/Ecoregions2017_v2.tif',overwrite=T)

plot(e.r['BIOME_NUM']) # 13 
plot(e.r['ECO_ID']) # 800
plot(e.r['NNH']) # protected status
plot(e.r['REALM1']) # protected status

# not used in v2
# # # # need df to merge metadata with raster codes (uniqe vals from e)
# # # ecoAtt=e %>% dplyr::select(ECO_NAME,ECO_ID,BIOME_NAME,BIOME_NUM,REALM,REALM1,NNH,NNH_NAME) %>% st_drop_geometry %>% unique
# # # # apparently NNH is a single classification for NNH.
# # # saveRDS(ecoAtt,file=paste0(cellAttDir,'/EcoAttr.rds'))
# # # ecoAtt=readRDS(paste0(cellAttDir,'/EcoAttr.rds'))
# # # 
# # # #update all cell attributes (these aren't actually cells yet, just ecoregions)
# # # d.cell=ecoAtt
# # # saveRDS(d.cell,file=paste0(cellAttDir,'/allCellAttr.rds'))
# # # 
# # # # ==================================================
# # # # NOTE. haven't used cells yet.
# # # # get cell by eco table
# # # 
# # # co=data.frame(xyFromCell(e.r1,1:ncell(e.r1)),cellID=1:ncell(e.r1))
# # # keep=!values(e.r1)[,1]==0
# # # co=co[keep,]
# # # 
# # # cellXEco=data.frame(co,values(e.r1)[keep,]) %>% mutate(BIOME_NUM=as.factor(BIOME_NUM), ECO_ID=as.factor(ECO_ID), NNH=as.factor(NNH),REALM1=as.factor(REALM1))
# # # saveRDS(cellXEco,file=paste0(cellAttDir,'/cellXEco.rds'))
# # # 
# # # d.cell2=d.cell %>% full_join(cellXEco,by=c('BIOME_NUM','NNH','ECO_ID','REALM1'))
# # # saveRDS(d.cell2,file=paste0(cellAttDir,'/allCellAttr.rds'))
# # # 
# # # 
# # # # ===================================================
# # # # NOTE. haven't used cells yet. i think this needs to be merged with sp and summarize num of cells in the range in the country. 
# # # # country by cell table
# # # # not fixed cuz i don't have a plan to use it
# # # gadm=st_read('/Users/ctg/Documents/SDMs/Exposure_2023/gadm_410.gpkg')
# # # # 
# # # # # maybe get % of each cell that a country has. this means a cell can occur multiple times
# # # # # maybe also get majority rule for some applications
# # # # 
# # # # gadm.r=rast(st_rasterize(gadm,st_as_stars(land)))
# # # # g=data.frame(values(gadm.r),cellID=1:ncell(land)) %>% left_join(gadm%>% st_drop_geometry,by='UID')
# # # # keep=!values(gadm.r)[,1]==0
# # # # g1=g[keep,]
# # # # 
# # # # saveRDS(g1,file=paste0(cellAttDir,'/cellXGADM.rds'))
# # # # 
# # # # d.cell3=d.cell2 %>% full_join(g1,by=c('cellID'))
# # # # saveRDS(d.cell3,file=paste0(cellAttDir,'/allCellAttr.rds'))
# # # 
# # # 
# # # # need mega table of species by cell. perform subset operations and then  left join to smaller tables like speceis name and ecoregion
# # # 




