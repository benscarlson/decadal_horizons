# This is the same as V1 because it doesn't depend on exposre
monDir=sumDirMonitor
spimeta=readRDS('/Users/ctg/Documents/SDMs/Exposure_2023/errors_v1/misc/spInfo.rds') %>% mutate(group=str_replace_all(group,'mammals_terrestrial_only','mammals'))
spi=tibble(spName=sub('_t2m-q1_100p.tif','',list.files('/Users/ctg/Documents/SDMs/Exposure_2023/expProj/2023/climThresholds/t2m-q1_100p'),fixed=T))
spi=spi %>% left_join(spimeta,by='spName')

template=as.numeric(rast('/Users/ctg/Documents/SDMs/Exposure_2023/climDeparture/departure_tp_ref_1940-2022_proj_2023.tif')[[6]]>0)
values(template)[!is.na(values(template))]=0

mclapply(unique(spi$group),function(x){ #x='reptiles'
	print(x)
	print('===================================================')
	sp=spi %>% filter(group==x) %>% dplyr::select(spName) %>% unlist
	mon.count1=mon.count2=mon.count3=	mon.sp1=	mon.sp2=	mon.sp3= expertRich=template
	#df=data.frame(sp=sp,exposedYear=NA,range.size=NA,taxon=x)
	for(y in sp){ #y=sp[1]
		print(y)
		r.sp=try(abs(rast(paste0( '/Users/ctg/Documents/SDMs/Exposure_2023/spRangeSampling/',y,'.tif'))))
		if(class(r.sp)=='try-error') {
			print(paste0(y, ' couldnt read raster'))
			next
		}
		if(any(is.nan(minmax(r.sp)))) {
			print(paste0(y, ' raster is NaN'))
			next 
		}
		if(minmax(r.sp)[2]<1) next # no observations
		mon.count1=terra::app(c(mon.count1,r.sp[[1]]),sum,na.rm=T)
		mon.count2=terra::app(c(mon.count2,r.sp[[2]]),sum,na.rm=T)
		mon.count3=terra::app(c(mon.count3,r.sp[[3]]),sum,na.rm=T)
		r=try(as.numeric(r.sp>0)) # get species level
		mon.sp1=terra::app(c(mon.sp1,r[[1]]),sum,na.rm=T)
		mon.sp2=terra::app(c(mon.sp2,r[[2]]),sum,na.rm=T)
		mon.sp3=terra::app(c(mon.sp3,r[[3]]),sum,na.rm=T)
		expert=as.numeric(!is.na(r.sp[[1]]))
		expertRich=terra::app(c(expertRich,expert),sum,na.rm=T)
		#df[df$sp==y,'range.size']=sum(values(r),na.rm=T) # store range size
		#r=as.numeric(r.sp>=1) # any exposure (legacy from months)
		#exposed=app(c(exposed,r),sum,na.rm=T)
		#df[df$sp==y,'exposedYear']=sum(values(r),na.rm=T) # # cells exposed
	}
	out=c(mon.count1,mon.count2,mon.count3, mon.sp1,mon.sp2,mon.sp3,expertRich)
	names(out)=c(paste0('count.',names(r.sp)),paste0('sp.',names(r.sp)), 'expertRich')
	terra::writeRaster(out,file=paste0(monDir,'/',x,'.tif'), overwrite=T)
	'ignore error'
	#saveRDS(df, file=paste0(sumDir,'/',x,'_',z,'_stats.rds'))
},mc.cores=4)

