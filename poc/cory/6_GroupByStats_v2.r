# here i autogenerate group stats and see where the most exposure is.
# this is where the main table used for stats is made: paste0('/Users/ctg/Dropbox/Projects/2023_Exposure/ExposureXIUCNXECO_',scen,'.rds')

# redo with tables. just need 2023 exposure. adapt from fig 1
# get exposure
ff=list.files('/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_stats_v2',full.names=T)
d11=mclapply(ff,function(x){ # x=ff[12] # 10s
	print(x)
	a=readRDS(x) %>% filter(year.q == .99 & !isRounded & cell.q==.99 & quantileType==7) %>% dplyr::select(year.q:quantileIndex,X2023) %>% mutate(spName=basename(file_path_sans_ext(x)))
},mc.cores=11) %>% bind_rows %>% dplyr::select(-year.cell.q,-expEvents)


spMeta=qread(paste0(metaDir,'/spAttributes_v2.qs'))
st = d11 %>% full_join(spMeta,by=c('spName','rangeSize') ) %>% mutate(per.expYear=X2023/rangeSize)

# Explore --------------------------------------------------
# there are multiple NAs, with different interpretations
#1. missing redlist info. IUCN didn't provide these. they seem to be missing taxonomy, but for some reason had maps.
  head(st[is.na(st$redlistCategory),])
#2. Missing ecoregion info. I think these fell off the land mask used to rasterize the ecoregions. leave them in for summaries that dont invovle ecoregion
  head(st[is.na(st$ECO_ID),])
#3. missing taxonomic info. IUCN has maps for these but didn't include them in assessements or their taxonomy
	head(st[is.na(st$orderName),])
# looks like 1 and 3 completely overlap, so let's toss these as not part of the iucn product. fiture out why they had maps some day	
	head(spAtt[!is.na(spAtt$orderName) & is.na(spAtt$redlistCategory),])

st=st[!is.na(st$orderName),] # only 10ish
# 	3. cell attribute table
head(cellAtt) # NOT USED YET

table(d2$redlistCategory)

# see if any interesting classifications
print(d2 %>% filter(per.expYear>=.25) %>% group_by(taxon) %>% count(redlistCategory) %>% arrange(desc(n)),n=100)

print(d2 %>% filter(per.expYear>=.25 & per.expYear<.5) %>% group_by(taxon) %>% count(redlistCategory) %>% arrange(desc(n)),n=100)

# clean up
d2=st %>% mutate(redlistCategory=gsub('[[:space:]]','_',redlistCategory))
# dont make them factors because it shifts the numercial values compared to the metadata
# mutate(REALM=as.factor(REALM1),BIOME_NUM=as.factor(BIOME_NUM), ECO_ID=as.factor(ECO_ID), NNH=as.factor(NNH)) %>% 
 # these aren't land
d2 %>% filter(ECO_ID==1)  %>% pull(REALM) %>% unique
# some don't have an ecoregion. i susepect these are from small islands, ebcause Accipeter butleri in there. keep and just exclude from ecoregion stats; 
d2 %>% filter(is.na(ECO_ID)) %>%  data.frame %>% head

saveRDS(d2,file=paste0('/Users/ctg/Dropbox/Projects/2023_Exposure/ExposureXIUCNXECO_q99_p99_v2.rds'))

d2=readRDS(paste0('/Users/ctg/Dropbox/Projects/2023_Exposure/ExposureXIUCNXECO_q99_p99_v2.rds'))


#========================================================
# OLD TESTS. to delete when tables checked
# ecoregion names with species
	# d2 is finally clean an we only get NAs for cases with ecoregions where species fell in water
# # d2 %>% filter(per.expYear>=.25) %>% group_by(taxon,realm) %>% count(redlistCategory) %>% arrange(desc(n)) %>% print(n=100) 
# # 
# # d2 %>% filter(per.expYear>=.25) %>% group_by(familyName,realm) %>% count(redlistCategory) %>% arrange(desc(n)) %>% print(n=100)
# # 
# # d2 %>% filter(per.expYear>=.25) %>% group_by(taxon,ECO_ID) %>% count(redlistCategory) %>% arrange(desc(n)) %>% print(n=100)
# # 
# # d2 %>% filter(per.expYear>=.25) %>% group_by(taxon,ECO_NAME) %>% count(redlistCategory) %>% arrange(desc(n)) %>% print(n=100)
# # 
# # d2 %>% filter(per.expYear>=.25) %>% group_by(orderName,ECO_ID) %>% count(redlistCategory) %>% filter(redlistCategory=='Least_Concern' | redlistCategory=='Data_Deficient' )%>% arrange(desc(n)) %>% print(n=100)
# # 
# # d2 %>% dplyr::select(ECO_ID,ECO_NAME) %>% unique %>% arrange(desc(ECO_ID))


#========================================================
#========================================================
#========================================================
# not redone for v2 because it wasn't promising.

# # # # see how many 1998 exposed species are IUCN listed
# # # 
# # # # 	1. species exposure table 	but each year needs its own col. should make it take year and threshold. I guess these should be represented as a year and thresh col, and we filter by these before making a summary
# # # 
# # # 	# start with just 1 exposure scenario. add other scenarios and vars
# # # years1=1990:2023
# # # 	# this is what should exist. but allow missing
# # # vars1=list.files('/Users/ctg/Documents/SDMs/Exposure_2023/climThresholds')
# # # 	# nah, issues. list.files(paste0(sumDir,'/2003'),pattern='stats.rds')
# # # 	# loop over both and read in and add cols for vars and years. 
# # # exp.df=mclapply(seq_along(years1),function(y){
# # # 	out1=lapply(seq_along(vars1),function(v){
# # # 			# making file first allows for missing file
# # # 		pat=paste0(vars1[v], '.*_stats.rds')
# # # 		f1=list.files(paste0(sumDir,'/',years1[y]), pattern=pat,full.names=T) 
# # # 		if(length(f1)==0) {
# # # 			print(paste0('no files for: ',pat))
# # # 			return()
# # # 		} else { print(paste0(length(f1),' files for: ',pat))}
# # # 		st=f1 %>% lapply(readRDS) %>% bind_rows %>% mutate(var=vars1[v],year=years1[y]) %>% mutate(per.expYear=exposedYear/range.size)
# # # 		row.names(st)=NULL
# # # 		st
# # # 	})
# # # 	out2=out1 %>% bind_rows()
# # # },mc.cores=4)	%>% bind_rows()
# # # str(exp.df)
# # # 
# # # # the NAs occur because i got the species list from spInfo instaed of exposure map files, so i can just toss them
# # # exp.df=exp.df[ which(complete.cases(exp.df)), ]
# # # 
# # # #-----------------------------
# # # # join IUCN with exposure (just experimenting)
# # # 	# get duplicated species rows for each ecoregion a species occurs in
# # # # spAtt has different rows for multiple ecoregions, so use this
# # # d.iucn=readRDS(paste0(spAttDir,'/iucnStatus.rds')) %>% mutate(redlistCategory=gsub('[[:space:]]','_',redlistCategory))
# # # d2=d.iucn %>% full_join(exp.df,by="sp") %>% dplyr::select(-group)
# # # # # some NA snuck back in because exposure was calculated on species without assessments
# # # d2=d2[!is.na(d2$redlistCategory),] 
# # # 
# # # #-----------------------------
# # # # ready to check 98 exposure.
# # # 	# note that I used 20% exposed so tinker with that
# # # d3=d2 %>% filter(year==1998 & per.expYear>.2 & var=='t2m-q099_99p') 	
# # # nrow(d3)
# # # # 687 rows; check that this is hte same as thie histogram
# # # (d4 = d3 %>% filter(yearPublished>1998 & !redlistCategory=='Least_Concern' & !redlistCategory=="Data_Deficient"))
# # # 
# # # (d5 = d3 %>% filter(yearPublished>1998  & redlistCategory=="Data_Deficient"))
# # # (d6 = d3 %>% filter(yearPublished>1998 & redlistCategory=='Least_Concern'))
# # # nrow(d3) # total
# # # nrow(d4) # were listed
# # # nrow(d5) # no data
# # # nrow(d6) # no prob
# # # 
# # # (nrow(d6)/(nrow(d3)-nrow(d5))) #% we're wrong
# # # nrow(d5) * (1- (nrow(d6)/(nrow(d3)-nrow(d5))) )# another 57 speceis should be listed
# # # 
# # # #do the same for 2010 and see if different sp
# # # d3=d2 %>% filter(year==2010 & var=='t2m-q099_99p')#& per.expYear>.2 & var=='t2m-q099_99p') 	
# # # # 687 rows; check that this is hte same as thie histogram
# # # (d4 = d3 %>% filter(yearPublished>2010 & !redlistCategory=='Least_Concern' & !redlistCategory=="Data_Deficient"))
# # # 
# # # (d5 = d3 %>% filter(yearPublished>2010  & redlistCategory=="Data_Deficient"))
# # # (d6 = d3 %>% filter(yearPublished>2010 & redlistCategory=='Least_Concern'))
# # # nrow(d3) # total
# # # nrow(d4) # were listed
# # # nrow(d5) # no data
# # # nrow(d6) # no prob
# # # 
# # # (nrow(d6)/(nrow(d3)-nrow(d5))) #% we're wrong
# # # nrow(d5) * (1- (nrow(d6)/(nrow(d3)-nrow(d5))) )# another 57 speceis should 
# # # 
# # # # how to test whether this is more than expected by chance































