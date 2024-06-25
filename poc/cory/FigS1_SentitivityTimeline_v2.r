years=c(1990:2023)
bigEl=round(c(1997.5,2014.5,2015.5,2023.5))
someEl=round(c(2002.5,2004.5,2006.5,2009.5,2018.8))
bigEl.toplot=findInterval(bigEl,years)
someEl.toplot=findInterval(someEl,years)
#========================================================
# upper
ff=list.files('/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_stats_v2',full.names=T)

d11=mclapply(ff,function(x){ # x=ff[12] # 20s
	print(x)
	a=readRDS(x) %>% filter(year.q == .95 & !isRounded & cell.q==.95 & quantileType==7) %>% dplyr::select(-contains('mar')) %>% dplyr::select(-contains('XnM'),-expPre1990) %>% mutate(across(X1990:X2023,function(z){round(z/rangeSize,3)})) %>% mutate(spName=basename(file_path_sans_ext(x))) %>% pivot_longer(starts_with('X'),names_to='year',values_to='pExp') 
	# need long format to filter by percent exposed
},mc.cores=11) %>% bind_rows %>% left_join(spMeta,by='spName') %>% dplyr::select(-spName,thresh.val)
# years with no exposure get lost; need to store as 0s
d2=d11 %>% filter(pExp>=.2) %>% mutate(year=as.numeric(sub('X','',year)))  %>% count(year,group) 
# years with no exposure get lost; need to store as 0s
want=expand.grid(year=years,group=unique(d2$group))
d3=d2 %>% full_join(want) %>%  arrange(year) %>% mutate(n=replace_na(n,0)) %>% pivot_wider(names_from='year',values_from='n')
d5=as.matrix(d3[-1])
row.names(d5)=unlist(d3[,1])

#lower
d11=mclapply(ff,function(x){ # x=ff[12] # 20s
	print(x)
	a=readRDS(x) %>% filter(year.q == .99 & !isRounded & cell.q==1 & quantileType==7) %>% dplyr::select(-contains('mar')) %>% dplyr::select(-contains('XnM'),-expPre1990) %>% mutate(across(X1990:X2023,function(z){round(z/rangeSize,3)})) %>% mutate(spName=basename(file_path_sans_ext(x))) %>% pivot_longer(starts_with('X'),names_to='year',values_to='pExp') 
	# need long format to filter by percent exposed
},mc.cores=11) %>% bind_rows %>% left_join(spMeta,by='spName') %>% dplyr::select(-spName,thresh.val)
# years with no exposure get lost; need to store as 0s
d2=d11 %>% filter(pExp>=.2) %>% mutate(year=as.numeric(sub('X','',year)))  %>% count(year,group) 
# years with no exposure get lost; need to store as 0s
want=expand.grid(year=years,group=unique(d2$group))
d3=d2 %>% full_join(want) %>%  arrange(year) %>% mutate(n=replace_na(n,0)) %>% pivot_wider(names_from='year',values_from='n')
d5.1=as.matrix(d3[-1])
row.names(d5.1)=unlist(d3[,1])

#==================================================
# prep climate time sereies
cl=readRDS('/Users/ctg/Dropbox/Projects/2023_Exposure/globalMeansWeightedLat.rds') %>% filter(year>=1990)
maxTemp<-max(cl$t2m_C)
minTemp<-min(cl$t2m_C)
yNTicks=6
temp.seq=unique(round(seq(minTemp,maxTemp,length=yNTicks),1))
lab.cex=1.01
tck=-.01; y2line=-2
spMax=8400; # sum(d5[,'2023'],na.rm=T)
full.tmp=scales::rescale(cl$t2m_C,to=c(0,spMax))
#full.tmp=full.tmp[-c(1:2)]
#=================================================
# Plot
cols=c("red1", "gold","steelblue1",'steelblue4')
plot.f1=paste0(plotDir,'/S1_timeline_v8.pdf')

pdf(plot.f1,w=18,h=8*2)
par(mfrow=c(2,1),oma=c(3,3,3,3),mar=c(5,6,3,1))
	# lower
	fuck=barplot(d5, col=cols ,  border="white", space=0.04, font.axis=2, xlab="Year",ylab='Number of Species Thermally Exposed',las=1,cex.lab=1.5,ylim=c(0,spMax+500))
	mtext('a',side=3,at=1,cex=2)
	mtext('Lowest Upper Thermal Niche Limit',side=3,at=18,cex=2.5)
	mtext("Year",side=1,line=5,cex=2)
	mtext('Number of Species Thermally Exposed',side=2,line=5,cex=2)
	
	axis(4, at = scales::rescale(temp.seq,to=c(0,spMax)),labels = temp.seq,cex.axis=1.3*lab.cex,padj=0,las=1,tck=tck,col.axis="black",col="black",line=y2line,hadj=0)
	mtext("Mean Global Temperature (C)",side=4,line=2,cex=2)
	points(fuck,full.tmp,type="l",col="red3",lwd=3)
	points(fuck[someEl.toplot], full.tmp[round(someEl.toplot)],pch='*',cex=3)
	points(fuck[bigEl.toplot], full.tmp[round(bigEl.toplot)],pch='*',cex=5)
	legend('topleft',legend=rownames(d5),pch=15,col=cols,bty='n',cex=2)
	
	# upper
	fuck=barplot(d5.1, col=cols ,  border="white", space=0.04, font.axis=2, xlab="Year",ylab='Number of Species Thermally Exposed',las=1,cex.lab=1.5,ylim=c(0,spMax+500))
	mtext('b',side=3,at=1,cex=2)
	mtext('Highest Upper Thermal Niche Limit',side=3,at=18,cex=2.5)
	mtext("Year",side=1,line=5,cex=2)
	mtext('Number of Species Thermally Exposed',side=2,line=5,cex=2)

	axis(4, at = scales::rescale(temp.seq,to=c(0,spMax)),labels = temp.seq,cex.axis=1.3*lab.cex,padj=0,las=1,tck=tck,col.axis="black",col="black",line=y2line,hadj=0)
	mtext("Mean Global Temperature (C)",side=4,line=2,cex=2)
	points(fuck,full.tmp,type="l",col="red3",lwd=3)
	points(fuck[someEl.toplot], full.tmp[round(someEl.toplot)],pch='*',cex=3)
	points(fuck[bigEl.toplot], full.tmp[round(bigEl.toplot)],pch='*',cex=5)
	legend('topleft',legend=rownames(d5),pch=15,col=cols,bty='n',cex=2)
	
dev.off()
system(paste0('open ', plot.f1))








