# Fig 1. Timeline

#============================================================
# define el nino years
# Wiki: It is thought that there have been at least 30 El Niño events between 1900 and 2024, with the 1982–83, 1997–98 and 2014–16 events among the strongest on record.[51] Since 2000, El Niño events have been observed in 2002–03, 2004–05, 2006–07, 2009–10, 2014–16, 2018–19,[52][53][54] and 2023–24.[55][56]. Major ENSO events were recorded in the years 1790–93, 1828, 1876–78, 1891, 1925–26, 1972–73, 1982–83, 1997–98, 2014–16, and 2023–24.[57][58][59] During strong El Niño episodes, a secondary peak in sea surface temperature across the far eastern equatorial Pacific Ocean sometimes follows the initial peak.[60]
years=c(1990:2023)
bigEl=round(c(1997.5,2014.5,2015.5,2023.5))
someEl=round(c(2002.5,2004.5,2006.5,2009.5,2018.8))
bigEl.toplot=findInterval(bigEl,years)
someEl.toplot=findInterval(someEl,years)
#========================================================
# get exposure
ff=list.files('/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_stats_v2',full.names=T)
d11=mclapply(ff,function(x){ # x=ff[12] # 20s
	print(x)
	a=readRDS(x) %>% filter(year.q == .99 & !isRounded & cell.q==.99 & quantileType==7) %>% dplyr::select(-contains('mar')) %>% dplyr::select(-contains('XnM'),-expPre1990) %>% mutate(across(X1990:X2023,function(z){round(z/rangeSize,3)})) %>% mutate(spName=basename(file_path_sans_ext(x))) %>% pivot_longer(starts_with('X'),names_to='year',values_to='pExp') 
	# need long format to filter by percent exposed
},mc.cores=11) %>% bind_rows %>% left_join(spMeta,by='spName') %>% dplyr::select(-spName,thresh.val)
# years with no exposure get lost; need to store as 0s
d2=d11 %>% filter(pExp>=.2) %>% mutate(year=as.numeric(sub('X','',year)))  %>% count(year,group) 
# years with no exposure get lost; need to store as 0s
want=expand.grid(year=years,group=unique(d2$group))
d3=d2 %>% full_join(want) %>%  arrange(year) %>% mutate(n=replace_na(n,0)) %>% pivot_wider(names_from='year',values_from='n')
d5=as.matrix(d3[-1])
row.names(d5)=unlist(d3[,1])

#==================================================
# prep climate time sereies
cl=readRDS('/Users/ctg/Dropbox/Projects/2023_Exposure/globalMeansWeightedLat.rds') %>% filter(year>=1990)
maxTemp<-max(cl$t2m_C)
minTemp<-min(cl$t2m_C)
yNTicks=6
temp.seq=unique(round(seq(minTemp,maxTemp,length=yNTicks),1))
lab.cex=1.01
tck=-.01; y2line=-2
spMax=4300#sum(d5[,'2023'],na.rm=T)
full.tmp=scales::rescale(cl$t2m_C,to=c(0,spMax))

#=================================================
# Plot
cols=c("red1", "gold","steelblue1",'steelblue4')
plot.f1=paste0(plotDir,'/timeline_v8.pdf')

pdf(plot.f1,w=20,h=8)
par(oma=c(3,3,3,3),mar=c(5,6,3,1))
	fuck=barplot(d5, col=cols ,  border="white", space=0.04, font.axis=2, xlab="",ylab='',las=2,cex.axis=1.3*lab.cex,ylim=c(0,spMax+500))
	mtext("Year",side=1,line=5,cex=2)
	mtext('Number of Species Thermally Exposed',side=2,line=5,cex=2)
	axis(4, at = scales::rescale(temp.seq,to=c(0,spMax)),labels = temp.seq,cex.axis=1.3*lab.cex,padj=0,las=1,tck=tck,col.axis="black",col="black",line=y2line,hadj=0)
	mtext("Mean Global Temperature (C)",side=4,line=2,cex=2)
	points(fuck,full.tmp,type="l",col="red3",lwd=3)
	points(fuck[someEl.toplot], full.tmp[round(someEl.toplot)],pch='*',cex=4)
	points(fuck[bigEl.toplot], full.tmp[round(bigEl.toplot)],pch='*',cex=7)
	legend('topleft',legend=rownames(d5),pch=15,col=cols,bty='n',cex=2)
	
dev.off()
system(paste0('open ', plot.f1))















# not used in v2
# # #===============================================
# # # to get stats
# # apply(d5,2,sum,na.rm=T)
# # 
# # # low threshold
# # d2=lapply(seq_along(d1),function(x){
# # 	print(x)
# # 	d=d1[[x]] %>% filter(per.expYear>=.1) %>% count(taxon) %>% arrange(desc(n))
# # 	fuck=try({d$year=unique(d1[[x]]$year)},silent=T)
# # 	if(class(fuck)=='try-error'){
# # 		d=data.frame(taxon=c("amphibians","reptiles","mammals","birds" ),n=0,year=unique(d1[[x]]$year))
# # 	}
# # 	d
# # }) %>% bind_rows
# # 
# # d4=pivot_wider(d2,names_from=year,values_from=n)
# # d5=as.matrix(d4[-1])
# # row.names(d5)=unlist(d4[,1])
# # apply(d5,2,sum,na.rm=T)
# # 
# # d2=lapply(seq_along(d1),function(x){
# # 	print(x)
# # 	d=d1[[x]] %>% filter(per.expYear>=.3) %>% count(taxon) %>% arrange(desc(n))
# # 	fuck=try({d$year=unique(d1[[x]]$year)},silent=T)
# # 	if(class(fuck)=='try-error'){
# # 		d=data.frame(taxon=c("amphibians","reptiles","mammals","birds" ),n=0,year=unique(d1[[x]]$year))
# # 	}
# # 	d
# # }) %>% bind_rows
# # 
# # d4=pivot_wider(d2,names_from=year,values_from=n)
# # d5=as.matrix(d4[-1])
# # row.names(d5)=unlist(d4[,1])
# # apply(d5,2,sum,na.rm=T)

# # # ======================================================
# # # get timelines of annual footprint of departure, binary exposure (anything in the cell), cell * species. Is it accelerating?
# # f1=list.files('/Users/ctg/Documents/SDMs/Exposure_2023/climDeparture',pattern='departure_t2m_ref_1940-2022_proj',full.names=T)
# # r1=stack(f1)
# # foot1=sapply(seq_along(f1),function(x) {rr=stack(f1[x])[[5]]; sum(values(rr),na.rm=T)})
# # yr=sapply(strsplit(f1,'_'),function(x) sub('.tif','',x[[7]]))
# # df=data.frame(yr=as.numeric(yr), dep=foot1)
# # 
# # #exposure
# # basedir='/Users/ctg/Dropbox/Projects/2023_Exposure/SummariesYearly_v6/'
# # template=stack('/Users/ctg/Documents/SDMs/Exposure_2023/climDeparture/departure_tp_ref_1940-2022_proj_2023.tif')[[6]]>0
# # values(template)[!is.na(values(template))]=0
# # myClimateStat='q099'; myNicheStat='100p' #'99p'
# # r2=lapply(yr,function(x){
# # 	f.sp=list.files(paste0(basedir,x,'/'), pattern=paste0('t2m.*',myClimateStat,'.*',myNicheStat,'.*tif'),full.names=T)
# # 	ra=stack(f.sp)
# # 	r1=ra[[c(2,4,6,8)]] %>% calc(sum,na.rm=T)
# # 	r1=mask(r1,template)
# # 	r1=crop(r1,extent(-180,180,-61,86))
# # 	r1
# # })
# # 
# # foot2=sapply(1:length(r2),function(x) { r3=r2[[x]]>0; sum(values(r3),na.rm=T)})
# # 
# # foot3=sapply(1:length(r2),function(x) {sum(values(r2[[x]]),na.rm=T)})
# # df$expBin=foot2
# # df$expAll=foot3
# # # its not an error that there is more exposure in 2002 that 2007. its just that it doesn't cumulate to >20% of speceis ranges in 2002
# # par(mfrow=c(2,1))
# # plot(df$yr,df$dep,type='l',bty='n',xlab='year',las=1,ylab='# pixels')
# # lines(df$yr,df$expBin,col='red1')
# # legend('topleft',legend=c('climate departure','species exposure'),col=c('black','red'),lty=1, bty='n')
# # 
# # plot(df$yr,df$expAll,type='l',bty='n',xlab='year',las=1,ylab='# pixel * species')

