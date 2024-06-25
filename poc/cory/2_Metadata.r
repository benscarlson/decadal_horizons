# make all the stuff about cells and species that don't change. only join with modeling outputs outside this file. 6_GroupByStats joins with exposure

# NEED TO THINK THROUGH WHETHER TO TOSS THINGS OR TRACK ISSUES SO WE DONT HAVE TO DO THIS EVERY TIME WE UPDATE EXPSOURE. AND WHAT TO DO WITH MISSING DATA? INTERPOLATE? in V2, i think i've done this

sp.meta=readRDS(paste0(dataDir,'/db3/metadata/spInfo.rds')) %>% mutate(group=str_replace_all(group,'mammals_terrestrial_only','mammals'))
spCodes=readRDS('/Users/ctg/Documents/SDMs/Exposure_2023/exposure_2023_sensitivity/spCodes.rds')

spMeta=sp.meta%>% full_join(spCodes,by=c('spName'='sp_name')) 

spMeta %>% filter(is.na(spcd)) %>% data.frame
qsave(spMeta %>% filter(is.na(spcd)) %>% data.frame,paste0(metaDir,'/spAttributes_broke.qs')) # PEP FIXED THESE, if rerun V2, add them

qsave(spMeta,paste0(metaDir,'/spAttributes_tmp.qs'))

#+++++++++++++++++++++++++++++++++++++++++++++++
# IUCN Status
f=list.files('/Users/ctg/Dropbox/Projects/2023_Exposure/IUCN',pattern='assess',full.names=T,recursive=T)

# read the whole mess of files
d1=lapply(f,function(x){
	data.frame(read.csv(x),group=basename(dirname(x)))
}) %>% bind_rows  %>% mutate(group=replace(group,group=='Birds1','Birds')) %>% mutate(group=replace(group,group=='Birds2','Birds'))
# relevant subset
d.iucn = d1 %>% mutate(sp=sub('[[:space:]]','_',scientificName)) %>% dplyr::select(assessmentId:criteriaVersion,populationTrend,realm,sp,group) %>% dplyr::select(-scientificName)

saveRDS(d.iucn,file=paste0(spAttDir,'/iucnStatus.rds'))
d.iucn=readRDS(paste0(spAttDir,'/iucnStatus.rds'))
#update all species attributes

# save it all. 
spMeta=qread(paste0(metaDir,'/spAttributes_tmp.qs')) %>% full_join((d.iucn %>% dplyr::select(-group)),by=c('spName'='sp'))

qsave(spMeta,paste0(metaDir,'/spAttributes_tmp.qs'))

#saveRDS(d.sp,file=paste0(spAttDir,'/allSpAttr.rds'))

#+++++++++++++++++++++++++++++++++++++++++++++++
# species by ecoregion  
	# loop over sp ranges and make df of species, ecoregion. this can then be joined with the metadata for the other fields like protection and biome
	# could get speceis by cell and ecoregion by cell and join by cellid, then do unique on species and ecoregion. maybe when bored. 
e.r1=stack('/Users/ctg/Dropbox/Projects/2023_Exposure/Ecoregions2017/Ecoregions2017_v2.tif')
# i think the raster got fucked up with factors

# the old way used the coords, but better to use the same files with cellid's tha i use for exposer
##sp.coords.f=list.files('/Users/ctg/Documents/SDMs/Exposure_2023/errors_v1/spCoords',full.names=T) # this will include some sp that we don't have exposure models for. DOES THIS INCLUDE THE ISLAND CELLS?
ff=list.files('/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_expProj',full.names=T)
out=mclapply(ff,function(x){ # x=ff[1] # 5 min
	print(x)
	s=readRDS(x) %>% pull(cellid) %>% unique
	# don't make these fucking factors!
	fuck=raster::extract(e.r1,s) %>% data.frame %>% tibble %>% unique %>% drop_na %>%  mutate(sp=basename(file_path_sans_ext(x)))  
},mc.cores=11) %>% bind_rows

#FINISH JOINING, UPDATE ATTR and group stats, FIX MONITORING
spXEco=out %>% filter(!NNH==0) 
# NNH=0 and ECO=0 are rock/ice, so toss
# somewhere I lost the region names
#e=st_read('/Users/ctg/Dropbox/Projects/2023_Exposure/Ecoregions2017/Ecoregions2017.shp') %>% dplyr::select(-COLOR,-COLOR_BIO,-COLOR_NNH,-LICENSE,-SHAPE_LENG,-SHAPE_AREA) %>% st_drop_geometry
e.d=readRDS(paste0(computer,'/Dropbox/Projects/2023_Exposure/Ecoregions2017/ecoregionMetadata.rds')) %>% dplyr::select(- (COLOR:LICENSE))

spXEco1= spXEco %>% left_join(e.d,b=c('BIOME_NUM','NNH','ECO_ID')) 
# no  NAs!
spXEco1 %>% filter(is.na(ECO_NAME))

saveRDS(spXEco1,file=paste0(spAttDir,'/SpeciesXEcoregion.rds'))
spXEco=readRDS(paste0(spAttDir,'/SpeciesXEcoregion.rds'))

spXEco %>% filter(ECO_ID==1) # this looks good; all the same regions
#update all species attributes
spMeta=qread(paste0(metaDir,'/spAttributes_tmp.qs')) %>% full_join(spXEco,by=c('spName'='sp'))
#d.sp2=d.iucn %>% full_join(spXEco,by='sp')
# let's just keep these, because no assessment info is important too
spXEco$sp[which(!(spXEco$sp %in% d.iucn$sp))]
# these are species that do not have assessment info. 
(fuck=spMeta[!complete.cases(spMeta),])
fuck[which(fuck$sp %in% d.iucn$sp),] # have assessments but no models to intersect with ecoregions. that's fine, we'll toss these later
fuck$sp[which(fuck$sp %in% spXEco$sp)] # yes, mabye because I read in sp from Exposure_2023/errors_v1/spCoords instead on successful models, but no assessments
# keep everything and just join by what we need
qsave(spMeta,paste0(metaDir,'/spAttributes_tmp.qs'))


#+++++++++++++++++++++++++++++++++++++++++++++++
# taxonomic group
tax.f=list.files('/Users/ctg/Dropbox/Projects/2023_Exposure/IUCN',pattern='taxonomy',full.names=T,recursive=T)

d.tax=lapply(tax.f,function(x){
	data.frame(read.csv(x),group=basename(dirname(x)))
}) %>% bind_rows  %>% mutate(group=replace(group,group=='Birds1','Birds')) %>% mutate(group=replace(group,group=='Birds2','Birds')) %>% dplyr::select(-(infraType:taxonomicNotes)) %>% mutate(sp=sub('[[:space:]]','_',scientificName)) %>% dplyr::select(-scientificName,-group)

saveRDS(d.tax,file=paste0(spAttDir,'/SpeciesXTaxonomy.rds'))
d.tax=readRDS(paste0(spAttDir,'/SpeciesXTaxonomy.rds'))

d.sp3[is.na(d.sp3$orderName),]
d.iucn %>% filter(sp=='Anadia_antioquensis') 
d.sp2 %>% filter(sp=='Anadia_antioquensis') 
spXEco %>% filter(sp=='Anadia_antioquensis') 
# seems like some misalignment among iucn products

#update all species attributes
spMeta=qread(paste0(metaDir,'/spAttributes_tmp.qs'))  %>% full_join(d.tax,by=c('spName'='sp','internalTaxonId'))
# # (fuck=d.sp3[!complete.cases(d.sp3),]) # good. 
# # head(fuck[which(fuck$sp %in% d.tax$sp),]) 
# # 
qsave(spMeta,paste0(metaDir,'/spAttributes_tmp2.qs'))
# # d.sp3 %>% filter(ECO_ID==0) # still good
# # which(d.sp3$sp %in% missing.spAtt)
#gott find where these speceis were lost
# half have taxnomica group
d.tax$sp[which(d.tax$sp %in% missing.spAtt)]

#+++++++++++++++++++++++++++++++++++++++++++++++
# range size 
# best to use the same files as for exposure
ff=list.files('/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_expProj',full.names=T)
rangeSize=mclapply(ff,function(x){ # x=ff[1]
	print(x)
	tibble(rangeSize=readRDS(x) %>% pull(cellid) %>% unique %>% length,spName=basename(file_path_sans_ext(x)))
},mc.cores=11) %>% bind_rows

qsave(rangeSize,paste0(metaDir,'/rangeSize.qs'))

spMeta=qread(paste0(metaDir,'/spAttributes_tmp2.qs'))  %>% full_join(rangeSize,by=c('spName'))
qsave(spMeta,paste0(metaDir,'/spAttributes_v2.qs'))


#=============================================
#=============================================
#=============================================
# quantile rules
#=============================================
#=============================================
#=============================================



#=============================================
#=============================================
#=============================================
# cells
#=============================================
#=============================================
#=============================================
