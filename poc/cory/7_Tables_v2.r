#Tables
#d2=readRDS('/Users/ctg/Dropbox/Projects/2023_Exposure/ExposureXIUCNXECO_t2m-q099_99p.rds')
d2=readRDS(paste0('/Users/ctg/Dropbox/Projects/2023_Exposure/ExposureXIUCNXECO_q99_p99_v2.rds'))
#==============================================================

 # table 1: order x ecoregion x iucn not listed. get top 5 in each REALM, so 25 rows
 (d5=d2 %>% filter(per.expYear>=.2 & !REALM=='Antarctica') %>% group_by(ECO_NAME) %>% count(orderName) %>% arrange(desc(n)) %>% rename(Ecoregion=ECO_NAME,Order=orderName,NSpecies=n))
 # put realm back in
 head(cellAtt)
 tmp=cellAtt %>% dplyr::select(ECO_NAME,REALM) %>% rename(Ecoregion=ECO_NAME,Realm=REALM) %>% unique
	rownames(tmp)=NULL

	# just species IUCN mising
 (d6=d2 %>% filter(per.expYear>=.2) %>% group_by(ECO_NAME,orderName) %>% count(redlistCategory) %>% filter(redlistCategory=='Least_Concern' | redlistCategory=='Data_Deficient') %>%  arrange(desc(n)) %>% rename(Ecoregion=ECO_NAME,Order=orderName,NSpecies=n) %>% count %>% rename(Nnotlisted=n))

d7=d5 %>% left_join(d6,by=c('Ecoregion','Order')) 
d7= d7 %>% left_join(tmp,by='Ecoregion',relationship='many-to-many') 
row.names(d7)=NULL 
saveRDS(d7,'/Users/ctg/Dropbox/Projects/2023_Exposure/TableS1df_v2.rds')
 xtable( d7 %>% group_by(Realm) %>% slice_max(order_by=NSpecies,n=4))
 
#==============================================================
 # table 2: large range species with lots of exposure: good candiates for mointoring
 quantile(d2$range.size,.5,na.rm=T)
(d8=d2 %>% dplyr::select(rangeSize,per.expYear,spName,orderName,redlistCategory) %>% unique %>% filter(per.expYear>=.5) %>% arrange(desc(rangeSize))) %>% mutate(PercentExposed=per.expYear)#, fuck it... spName=sub('_','[[:space:]]',spName))
row.names(d8)=NULL
 head(d8)
 d8$per.expYear=100* d8$per.expYear
names(d8)=c('Range\nSize','% Exposed','Species','Order','Category')
saveRDS(d8,'/Users/ctg/Dropbox/Projects/2023_Exposure/TableS2df_v2.rds')

xtable(d8[1:40,],digits=c(1,0,0,0,0,0))

#==============================================================
