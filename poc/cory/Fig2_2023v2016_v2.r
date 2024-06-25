# Motivation: WHY IS A TINY INCREASE IN TEMP IN 2023 A DOUBLING OF EXPOSURE COMPARED TO 2016? DO WE NEED TO LOOK AT THERMAL MARGINS?

#==============================================
#==============================================
#==============================================
#==============================================
## restart anaylsis here

## out1=readRDS('/Users/ctg/Dropbox/Projects/2023_Exposure/SpeciesXCellXExposure_1990-2023_t2m-q099_99p_v2.rds')

# read in cell level exposure and fill in unexposed cells
ff=list.files('/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_marg_v2',full.names=T)

d2=mclapply(ff,function(x){ # x=ff[12] # 5 min
	print(x)
	onlyExposed=readRDS(x) %>% filter(year.q == .99 & !isRounded & cell.q==.99 & quantileType==7) 
	thresh=unique(onlyExposed$thresh.val)
	onlyExposed= onlyExposed %>% dplyr::select(exposedYearCells,thresh.val) %>% unnest(exposedYearCells) %>% filter(year==2016 | year==2023)  %>% mutate(exposed=1)
	allSpYearCells=readRDS(paste0( '/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_expProj/',basename(x))) %>% dplyr::select(cellid,year,sp_code,t2m) %>% filter(year==2016 | year==2023) %>% left_join(onlyExposed,by=c('cellid'='cell','year')) %>% mutate(thresh.val=thresh)
},mc.cores=22) 

d21=d2%>% bind_rows %>% mutate(exposed=replace_na(exposed,0),margin=thresh.val-t2m)

# check all exposed are <0
all( (d21 %>% filter(exposed>0) %>% pull(margin)) <=0)
# check all not exposed are >0
all( (d21 %>% filter(exposed==0) %>% pull(margin)) >0)

# prep for plot
a.1=d21 %>% filter(year==2016 & margin<20 & margin > -3)
a.2=d21 %>% filter(year==2023 & margin<20 & margin > -3)

spMar=d21 %>% group_by(sp_code,year) %>% summarize(sp_margin=median(margin))# %>% filter(sp_margin <10 )
b.1=spMar %>% filter(year==2016)
b.2=spMar %>% filter(year==2023)

plot.f5=paste0(plotDir,'/Fig2_16_vs_23_v2.pdf')
	pdf(plot.f5,w=16,h=8)
	par(mfrow=c(1,2),oma=c(2,2,2,2))
	hist(a.1$margin,col='red',breaks=seq(-3,20,by=.5), main='',xlab='thermal margin ([niche limit] - [cell temperature])', ylab='# species x cells',ylim=c(0,4e6),cex.lab=1.4,cex.axis=1.5)
	hist(a.2$margin,add=T,col=grey(.5,.5), breaks=seq(-3,20,by=.5))
	mtext('a',outer=T,at=.05,side=3,cex=2,line=-5)
	abline(v=0,lty=2,lwd=3)
	legend('topright',legend=c(2016,2023),col=c('red',grey(.5,.5)), pch=15,bty='n',cex=2)

	hist(b.1$sp_margin,col='red',breaks=seq(-1,10,by=.5),main='', xlab='species median thermal margin', ylab='# species',cex.lab=1.4,cex.axis=1.5)

	# try to show just the new exposure in 2023
	hist(b.2$sp_margin,add=T,col=grey(.5,.5),breaks=seq(-1,10,by=.5))
	abline(v=0,lty=2,lwd=3)
	mtext('b',outer=T,side=3,at=.55,cex=2,line=-5)
	legend('topright',legend=c(2016,2023),col=c('red',grey(.5,.5)), pch=15,bty='n',cex=2)
dev.off()
system(paste0('open ', plot.f5))

# There's a peak about 2 degrees from 2023. The number and locations of speceis that will be exposured will depend on the spatial pattern, but 2023 
#even a half a degree would more than double the number of sp x cells exposed compared to 2023



#=========================================================
# how often are there 2 degree increases
rr=rast('/Users/ctg/Downloads/delta_t2m_2316.tif')
r1=crop(rr,extent(-180,180,-61,86))
allland=sum(values(!is.na(r1)))
(land.5=sum(values(r1)>=0.5,na.rm=T)/allland)
(land1=sum(values(r1)>=1,na.rm=T)/allland)
(land2=sum(values(r1)>=2,na.rm=T)/allland)





# ====================================================
# ====================================================
# ====================================================
# ====================================================
#exploring patterns in margins

# # # this works with the old version. wasn't needed for v2
# # # why are some margins > 10. seems like that should be rare
# # 
# # fuck=marg %>% filter(t2m_margin_99_99_23>30)
# # head(fuck)
# # 
# # r1=rast('/Users/ctg/Documents/SDMs/Exposure_2023/climThresholds/t2m-q099_99p/Ablepharus_darvazi_t2m-q099_99p.tif')
# # plot(trim(r1$`2023`))
# # 
# # # check the max temps in the historical recors
# # pat="Ablepharus_pannonicus"
# # pat="Zosterops_palpebrosus"
# # pat='Phylloscopus_reguloides'
# # f1=list.files('/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_expProj',pattern=pat,full.names=T)
# # df=readRDS(f1)
# # max(df$t2m)
# # 
# # fuck=marg %>% filter(t2m_margin_99_99_23>40)
# # unique(fuck$sp)
# # 
# # # it seems like these are real diffs for wide ranging species
# # 
# # # what about super negative margins
# # fuck=marg %>% filter(t2m_margin_99_99_23 < (-5))
# # # are these all excluded cells
# # out1.1=readRDS('/Users/ctg/Dropbox/Projects/2023_Exposure/marginalCells.rds')
# # out1.11=out1.1 %>% dplyr::select(sp,cellID,isMarginal,marginalAnd2023) %>% rename(cellid=cellID)
# # fuck2=fuck %>% left_join(out1.11,by=c('sp','cellid'))
# # sum(fuck2$isMarginal)/nrow(fuck2)
# # 
# # margTooBig=marg %>% filter(t2m_margin_99_99_23>30)
# # margTooSmall=marg %>% filter(t2m_margin_99_99_23 < (-5))
# # saveRDS(list(margTooBig=margTooBig,margTooSmall=margTooSmall),file='/Users/ctg/Dropbox/Projects/2023_Exposure/marginsToExplore.rds')
# # 
# # margTooSmall %>% arrange(t2m_margin_99_99_23) %>% head
# # 
# # fuck3=margTooBig %>% left_join(out1.11,by=c('sp','cellid'))
# # sum(fuck3$isMarginal)/nrow(fuck3)
# # 
# # # try to find a tipping point
# # par(mfrow=c(1,1))
# # min(marg$t2m_margin_99_99_23)
# # hist(marg$t2m_margin_99_99_23,col='red',breaks=seq(-5,45,by=1))
# # hist(marg$t2m_margin_99_99_16,add=T,breaks=seq(-5,45,by=1),col=grey(.5,.5))
# # 
# # d1=density(marg$t2m_margin_99_99_16)
# # d2=density(marg$t2m_margin_99_99_23)
# # lines(d2,col='red')























