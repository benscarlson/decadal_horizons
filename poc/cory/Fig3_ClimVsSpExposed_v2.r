
basedir='/Users/ctg/Dropbox/Projects/2023_Exposure/SummariesYearly_v8/'


template=stack('/Users/ctg/Documents/SDMs/Exposure_2023/climDeparture/departure_tp_ref_1940-2022_proj_2023.tif')[[6]]>0
values(template)[!is.na(values(template))]=0
#======================================================
# !!! could updaet this to be be based on the new table format. this avoids difs in a few sp. see fig s4. doesn't affect anything, just assures perfect alignment

#V2 notes. I only sp X cell for exposed cells. 

ff=list.files('/Users/ctg/Documents/SDMs/Exposure_2023/db3/t2m_marg_v2',full.names=T)

d2=mclapply(ff,function(x){ # x=ff[12] # 5 min
	print(x)
	readRDS(x) %>% filter(year.q == .99 & !isRounded & cell.q==.99 & quantileType==7) %>% dplyr::select(exposedYearCells) %>% unnest(exposedYearCells) %>% filter(year==2023)  
},mc.cores=11) 

out3=d2 %>% bind_rows %>% count(cell) #%>% summarize_all(sum)
	# put onto raster
template=stack('/Users/ctg/Documents/SDMs/Exposure_2023/climDeparture/departure_tp_ref_1940-2022_proj_2023.tif')[[6]]>0
values(template)[!is.na(values(template))]=0
r2023.2=template
values(r2023.2)[out3$cell]=out3$n
r2023.2=mask(r2023.2,template)
r2023.2=crop(r2023.2,extent(-180,180,-61,86))
writeRaster(r2023.2,file=paste0(intDir,'/Fig2_exposure_v2.tif'))

# climate departure
t2m=stack('/Users/ctg/Documents/SDMs/Exposure_2023/climDeparture/magnitude_t2m_ref_1940-2022_proj_2023.tif')[['q0.99']]
#t2m=stack('/Users/ctg/Documents/SDMs/Exposure_2023/climDeparture/magnitude_t2m_ref_1940-2022_proj_2023.tif')[['q1']]
t2m[values(t2m<0)]=0
t2m=mask(t2m,template)
t2m=crop(t2m,extent(-180,180,-61,86))

breaks=c(0,1,12,25,37,50,75,100,150,200,300,600)
cm.cols11=function(x,bias=1) { colorRampPalette(c('steelblue1','gold','red1','red4'),bias=bias)(x)
}
cols=c('grey93',cm.cols11(length(breaks)-2,bias=.7))
cols4=c('white',colorRampPalette(c('gold','red1'))(100))

plot.f1=paste0(plotDir,'/ExposureByTemp_q99_p99_v8.pdf')
pdf(plot.f1,w=10,h=6)

par(mfrow=c(2,1),mar=c(0,0,0,1),oma=c(1,3,1,1))

plot(t2m,col=cols4,main='', ylab='' ,bty='none',cex.main=3,box=FALSE, axes=FALSE,ylim=c(-61,86),smallplot= c(.73,.75,.1,.83),cex.axis=2.5,legend.args=list(text='\u00B0C', line=1,side=3,cex=1.3,cex.axis=2))
#mtext('Mean Annual Temperature',side=2,adj=.95,cex=1.4,outer=T)
mtext('Climate Departure',side=2,adj=.8,cex=1.2,outer=T,line=0)
sp::plot(world.shp,add=TRUE,lwd=.5,border='grey40')
mtext('a',side=3,adj=.1,cex=2,line=-1.1)

plot(r2023.2[[1]],col=cols,main='',bty='none',cex.main=3,box=FALSE, breaks=breaks,axes=FALSE,ylim=c(-61,86),zlim=c(0,max(breaks)),smallplot= c(.73,.75,.1,.83),	legend.args=list(text='# species', line=1,side=3,cex=1.3,cex.axis=2))#,axis.args=list(at=ats,labels=breaks))
sp::plot(world.shp,add=TRUE,lwd=.5,border='grey40')
mtext('Species Exposure',side=2,adj=.2,cex=1.2,outer=T,line=0)
mtext('b',side=3,adj=.1,cex=2,line=-1.1)

dev.off()
system(paste0('open ', plot.f1))

# check vs old version
old=raster('/Users/ctg/Dropbox/Projects/2023_Exposure/Fig2_exposure.tif')
plot(old-r2023.2) # just a couple from the island speceis added
#===================================
# zooms

plot.f1=paste0(plotDir,'/ExposureByTemp_q99_p99_zoom_v8.pdf')
pdf(plot.f1,w=24,h=6)
par(mfrow=c(1,4),mar=c(1,1,1,1))

plot(r2023.2[[1]],col=cols,main='',cex.main=3, breaks=breaks,axes=FALSE,ylim=c(0,40),xlim=c(-105,-65),zlim=c(0,max(breaks)),legend=F)#,smallplot= c(.87,.89,.1,.9),	legend.args=list(text='# species', line=1,side=3,cex=1.5,cex.axis=2))
sp::plot(world.shp.hi,add=TRUE,lwd=.5,border='grey40')
mtext('c',side=3,adj=.1,cex=5,line=-5.1)

plot(r2023.2[[1]],col=cols,main='',cex.main=3, breaks=breaks,axes=FALSE,ylim=c(-40,0),xlim=c(-90,-50),zlim=c(0,max(breaks)),legend=F)#,smallplot= c(.87,.89,.1,.9),	legend.args=list(text='# species', line=1,side=3,cex=1.5,cex.axis=2))
sp::plot(world.shp.hi,add=TRUE,lwd=.5,border='grey40')
mtext('d',side=3,adj=.1,cex=5,line=-5.1)

plot(r2023.2[[1]],col=cols,main='',cex.main=3, breaks=breaks,axes=FALSE,ylim=c(-5,35),xlim=c(10,50),zlim=c(0,max(breaks)),legend=F)#,smallplot= c(.87,.89,.1,.9),	legend.args=list(text='# species', line=1,side=3,cex=1.5,cex.axis=2))
sp::plot(world.shp.hi,add=TRUE,lwd=.5,border='grey40')
mtext('e',side=3,adj=.1,cex=5,line=-5.1)

plot(r2023.2[[1]],col=cols,main='',cex.main=3, breaks=breaks,axes=FALSE,ylim=c(-5,35),xlim=c(90,130),zlim=c(0,max(breaks)),legend=F)#,smallplot= c(.87,.89,.1,.9),	legend.args=list(text='# species', line=1,side=3,cex=1.5,cex.axis=2))
sp::plot(world.shp.hi,add=TRUE,lwd=.5,border='grey40')
mtext('f',side=3,adj=.1,cex=5,line=-5.1)

dev.off()
system(paste0('open ', plot.f1))










# #============================================================
# #try again with a zoom plot
# 
# plot.f=paste0(plotDir,'/ExposureByVar_',myClimateStat,'_',myNicheStat,'_v4.png')
# png(plot.f,h=700*6*1,w=1000*6*2)
# par(mfcol=c(3,6),mar=c(0,3,0,39),oma=c(3,30,3,6))
# fdZoomPlot(r[[1]],plot.f=NULL,shp=world.shp,open=T, legend.args=list(text='# species',line=3,side=3,cex=7,adj=c(.3,.6)), axis.args=list(cex.axis=9), plot.labs=letters[1:6],lab.cex=18,quantileBreaks=FALSE,form.args=list(digits=1,scientific=NA,round=0))
# 
# fdZoomPlot(r[[2]],plot.f=NULL,shp=world.shp,open=T, legend.args=list(text='# species',line=3,side=3,cex=7,adj=c(.3,.6)), axis.args=list(cex.axis=9), plot.labs=letters[7:12],lab.cex=18,quantileBreaks=FALSE,form.args=list(digits=1,scientific=NA,round=0))
# 
# fdZoomPlot(r[[3]],plot.f=NULL,shp=world.shp,open=T, legend.args=list(text='# species',line=3,side=3,cex=7,adj=c(.3,.6)), axis.args=list(cex.axis=9), plot.labs=letters[13:18],lab.cex=18,quantileBreaks=FALSE,form.args=list(digits=1,scientific=NA,round=0))
# #mtext('# Species Exposed', outer=T,side=3,line=10,cex=10)
# mtext('Max. Monthly Temp.', outer=T,side=2,line=10,cex=10,at=.85)
# mtext('Min. Monthly Precip.', outer=T,side=2,line=10,cex=10,at=.5)
# mtext('Max. Monthly Precip.', outer=T,side=2,line=10,cex=10,at=.15)
# 
# dev.off()
# system(paste0('open ',plot.f))



